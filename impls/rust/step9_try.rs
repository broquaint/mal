use std::env::args;
use std::io::{self, Write};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

extern crate regex;

mod types;
use types::MalVal::{self, *};
use types::MalUserFn;
use types::MalRet;
use types::MalErr;
mod reader;
use reader::read_str;
use reader::mal_sym;
mod printer;
use printer::pr_str;
mod env;
use env::MalEnv;
mod core;
use core::core_ns; // Also exports err!, mlist! macros.
use core::make_bound_env;
use core::call_user_fun;

// Simplify common EVAL use case where the first arg is cloned into a new Rc.
macro_rules! ervl {
    ($val:expr, $env:expr) => { EVAL(Rc::new($val.clone()), $env) }
}

fn eval_ast(ast: Rc<MalVal>, menv: &Rc<MalEnv>) -> MalRet {
    match &*ast {
        List(l) => {
            let mut new_list: Vec<MalVal> = Vec::new();
            // Iterate with for-loop to raise the first Err encountered.
            for v in l.iter() {
                new_list.push(ervl!(v, menv)?);
            }
            Ok(as_mal_list!(new_list))
        },
        Vector(l) => {
            let mut new_vec: Vec<MalVal> = Vec::new();
            // Iterate with for-loop to raise the first Err encountered.
            for v in l.iter() {
                new_vec.push(ervl!(v, menv)?);
            }
            Ok(Vector(Rc::new(new_vec)))
        },
        Map(l) => {
            let mut new_map: HashMap<String, MalVal> = HashMap::new();
            // Iterate with for-loop to raise the first Err encountered.
            for (k, v) in l.iter() {
                new_map.insert(k.clone(), ervl!(v, menv)?);
            }
            Ok(Map(Rc::new(new_map)))
        },
        Sym(s) => menv.get(&s),
        v => Ok(v.clone())
    }
}

fn is_true(cond: MalVal) -> bool {
    match cond {
        Bool(b) => b,
        Nil     => false,
        _       => true
    }
}

fn make_env(binds: &Rc<Vec<MalVal>>, outer_env: &Rc<MalEnv>) -> Result<Rc<MalEnv>, MalErr> {
    let new_env = Rc::new(MalEnv {
        outer: Some(Rc::clone(outer_env)),
        data:  Rc::new(RefCell::new(HashMap::new())),
        id:    outer_env.id * 2,
    });

    if binds.len() % 2 != 0 {
        return err!("binds for let* wasn't even")
    }

    for bind in binds.chunks(2) {
        match &bind[0] {
            Sym(k) => {
                let v = ervl!(bind[1], &new_env)?;
                new_env.set(k.clone(), v);
            }
            _ => return err!("bind in let* wasn't a symbol")
        }
    }

    Ok(new_env)
}

fn quasiquote(ast: MalVal) -> MalVal {
    match ast {
        List(l) | Vector(l) if !l.is_empty() => {
            let(head, tail) = l.split_first().unwrap();
            match head {
                Sym(s) if s == "unquote" => return tail[0].clone(),
                List(l) | Vector(l) => {
                    // A chained if-let would make this a bit cleaner.
                    if let Sym(s) = &l[0] {
                        if s == "splice-unquote" {
                            let(_, ht) = l.split_first().unwrap();
                            return mal_list![
                                mal_sym("concat"), ht[0].clone(), quasiquote(as_mal_list![tail.to_vec()])
                            ]
                        }
                    }
                }
                _ => { /* This would all be nicer with chained if-let */ }
            }
            mal_list![mal_sym("cons"), quasiquote(head.clone()), quasiquote(as_mal_list![tail.to_vec()])]
        }
        _ => mal_list![mal_sym("quote"), ast]
    }
}

fn is_macro_call(ast: Rc<MalVal>, env: &Rc<MalEnv>) -> Option<MalUserFn> {
    match &*ast {
        List(l) if !l.is_empty() => {
            if let Sym(s) = &l[0] {
                let e = env.find(&s);
                if e.is_none() { return None }
                let r = e.unwrap().get(&s);
                match r {
                    // Return a MalUserFn here to saving pulling it out again in macroexpand.
                    Ok(UserFun(f)) if f.is_macro => Some(f),
                    _ => None
                }
            } else { None }
        }
        _ => None
    }
}

fn macroexpand(mut ast: Rc<MalVal>, env: &Rc<MalEnv>) -> Result<Rc<MalVal>, MalErr> {
    while let Some(f) = is_macro_call(Rc::clone(&ast), env) {
        if let List(l) = &*ast {
            let (_, args) = l.split_first().unwrap();
            match call_user_fun(&f, args) {
                Ok(next_ast) => { ast = Rc::new(next_ast); }
                Err(e) => return Err(e)
            }
        }
        else {
            return err!("bug! is_macro_call returned Some but ast[0] wasn't a List!")
        }
    }

    Ok(Rc::clone(&ast))
}

#[allow(non_snake_case)]
pub fn EVAL(mut ast: Rc<MalVal>, cur_env: &Rc<MalEnv>) -> MalRet {
    let env = RefCell::new(Rc::clone(cur_env));

    'eval_loop: loop {
        ast = macroexpand(ast, &env.borrow())?;

        match &*ast {
            List(l) => {
                if l.is_empty() {
                    return Ok(mal_list![])
                }
                else {
                    // Shouldn't panic as we know l isn't empty.
                    let (sym, rest) = l.split_first().unwrap();
                    if let Sym(tok) = sym {
                        match tok.as_str() {
                            "def!" => {
                                let (name, args) = rest.split_first().unwrap();
                                return match name {
                                    Sym(s) => {
                                        let val = ervl!(args[0], &env.borrow())?;
                                        env.borrow().set(s.clone(), val.clone());
                                        Ok(val)
                                    },
                                    _ => err!("First elem of def! wasn't a Sym")
                                }
                            }
                            "defmacro!" => {
                                let (name, args) = rest.split_first().unwrap();
                                return match name {
                                    Sym(s) => {
                                        let val = ervl!(args[0], &env.borrow())?;
                                        if let UserFun(f) = val {
                                            let mut mac = f.clone();
                                            mac.is_macro = true;
                                            env.borrow().set(s.clone(), UserFun(mac));
                                            Ok(Nil) // Not correct, should return val.
                                        }
                                        else {
                                            err!("Can only defmacro! a function")
                                        }
                                    },
                                    _ => err!("First elem of defmacro! wasn't a Sym")
                                }
                            }
                            "macroexpand" => {
                                let astrc  = macroexpand(Rc::new(rest[0].clone()), &env.borrow())?;
                                let newast = Rc::try_unwrap(astrc); // Result<MalVal, Rc<MalVal>>
                                return newast.map_err(|_| MalErr(Rc::new(Str("couldn't get at result of macroexpand?!".to_string()))));
                            }
                            "let*" => {
                                let binds = &rest[0];
                                let form = &rest[1];
                                match binds {
                                    List(bl) | Vector(bl) => {
                                        let new_env = make_env(bl, &env.borrow())?;
                                        env.replace(new_env);
                                        ast = Rc::new(form.clone());
                                        continue 'eval_loop
                                    }
                                    _ => return err!("First elem of let* wasn't a list/vec")
                                }
                            }
                            "do" => {
                                let(last, leading) = rest.split_last().unwrap();
                                eval_ast(Rc::new(as_mal_list![leading.to_vec()]), &env.borrow())?;
                                ast = Rc::new(last.clone());
                                continue 'eval_loop
                            }
                            "if" => {
                                // rest[0] = cond, rest[1] = true branch, rest[2] = false branch
                                let next_ast = if is_true(ervl!(rest[0], &env.borrow())?) {
                                    &rest[1]
                                }
                                else if rest.len() > 2 {
                                    &rest[2]
                                }
                                else {
                                   &Nil
                                };
                                ast = Rc::new(next_ast.clone());
                                continue 'eval_loop
                            }
                            "fn*" => {
                                let binds = &rest[0];
                                let body  = &rest[1];
                                match binds {
                                    List(bl) | Vector(bl) => {
                                        ast = Rc::new(
                                            UserFun(
                                                MalUserFn {
                                                    binds:    bl.clone(),
                                                    body:     Rc::new(body.clone()),
                                                    env:      Rc::clone(&env.borrow()),
                                                    is_macro: false,
                                                    // Allow calling user functions in core.
                                                    eval:  EVAL
                                                }
                                            )
                                        );
                                        continue 'eval_loop
                                    }
                                    _ => return err!("First elem of fn* wasn't a list/vec")
                                }
                            }
                            "eval" => {
                                // Get the ast to run EVAL against.
                                let tmp_ast = ervl!(rest[0], &env.borrow())?;
                                // Evaluate the produced AST.
                                return EVAL(Rc::new(tmp_ast), &env.borrow().root());
                            }
                            "quote" => {
                                return Ok(rest[0].clone());
                            }
                            "quasiquote" => {
                                ast = Rc::new(quasiquote(rest[0].clone()));
                                continue 'eval_loop
                            }
                            "try*" => {
                                return match ervl!(rest[0], &env.borrow()) {
                                    Ok(res) => Ok(res),
                                    Err(err) => {
                                        if rest.len() == 1 {
                                            // Throw it up if nothing can catch it
                                            Err(err)
                                        }
                                        else if let List(exvec) = &rest[1] {
                                            let(sym, bind, body) = (&exvec[0], &exvec[1], &exvec[2]);
                                            match (sym, bind) {
                                                (Sym(s), Sym(_)) if s == "catch*" => {
                                                    // Only expect a single bind to the single error.
                                                    let bindlist  = Rc::new(vec![bind.clone()]);
                                                    let catch_env = make_bound_env(&env.borrow(), &bindlist, &[err.to_val()])?;
                                                    EVAL(Rc::new(body.clone()), &Rc::new(catch_env))
                                                }
                                                (_, err) => errf!("Expected catch*, got {}", pr_str(err.clone(), true))
                                            }
                                        }
                                        else {
                                            err!("try/catch had a weird structure")
                                        }
                                    }
                                }
                            }
                            _ => { /* fallthrough to function call */ }
                        }
                    }

                    return if let List(fcall) = eval_ast(ast, &env.borrow())? {
                        let (fun, args) = fcall.split_first().unwrap();
                        match fun {
                            CoreFun(f) => f(args),
                            UserFun(f) => call_user_fun(f, args),
                            f => errf!("Can't call '{}' as a function", pr_str(f.clone(), true))
                        }
                    }
                    else {
                        err!("Somehow eval_ast(List) didn't return a list?")
                    }
                }
            }
            v => return eval_ast(Rc::new(v.clone()), &env.borrow())
        }
    }
}

#[allow(non_snake_case)]
fn PRINT(code: MalVal) -> String {
    pr_str(code, true) // TODO Don't be so lazy with clone.
}

#[allow(non_snake_case)]
fn READ(code: String) -> Result<MalVal, String> {
    read_str(code)
}

fn rep(code: String, menv: &Rc<MalEnv>) -> Result<String, String> {
    let ast = READ(code)?;
    match EVAL(Rc::new(ast), menv) {
        Ok(res) => Ok(PRINT(res)),
        Err(err) => Err(pr_str(err.to_val(), true)),
    }
}

fn rep_lit(code: &str, env: &Rc<MalEnv>) {
    if let Err(e) = rep(code.to_string(), env) {
       println!("Failed to eval '{}': {}", code, e);
       std::process::exit(1);
   }
}

fn main() {
    let repl_env = Rc::new(MalEnv { outer: None, data: Rc::new(RefCell::new(HashMap::new())), id: 1 });

    for (k,v) in &core_ns() {
        repl_env.set(k.clone(), v.clone());
    }

    rep_lit("(def! not (fn* (a) (if a false true)))", &repl_env);
    rep_lit("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", &repl_env);
    rep_lit("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", &repl_env);

    let argstar  = "*ARGV*".to_string();
    let mut argv = args();
    if argv.len() > 1 {
        let file = argv.next().unwrap();
        repl_env.set(
            argstar, as_mal_list![argv.map(|arg| Str(arg)).collect()]
        );
        rep_lit(format!("(load-file {})", file).as_str(), &repl_env)
    }
    else {
        repl_env.set(argstar, mal_list![]);
        loop {
            print!("user> ");
            io::stdout().flush().unwrap();

            let mut input = String::new();
            match io::stdin().read_line(&mut input) {
                Ok(_)  => {
                    let result = rep(input, &repl_env);
                    println!("{}", result.map_or_else(|s| s, |e| e));
                }
                Err(_) => { break; }
            }
        }
    }
}
