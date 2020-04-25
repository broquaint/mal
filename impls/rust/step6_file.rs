use std::io::{self, Write};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::Cell;
use std::cell::RefCell;

extern crate regex;

mod types;
use types::MalVal::{self, *};
use types::MalUserFn;
mod reader;
use reader::read_str;
mod printer;
use printer::pr_str;
mod env;
use env::MalEnv;
mod core;
use core::core_ns; // Also exports err! macro.
use core::MalRet;

macro_rules! mlist {
    ($e:expr) => { List(Rc::new($e)) }
}

fn eval_ast(ast: &MalVal, menv: &Rc<MalEnv>) -> MalRet {
//    println!("eval_ast: {}", pr_str(ast.clone(), true));
    match ast {
        List(l) => {
            let mut new_list: Vec<MalVal> = Vec::new();
            // Iterate with for-loop to raise the first Err encountered.
            for v in l.iter() {
                new_list.push(EVAL(&v, menv)?);
            }
            Ok(mlist!(new_list))
        },
        Vector(l) => {
            let mut new_vec: Vec<MalVal> = Vec::new();
            // Iterate with for-loop to raise the first Err encountered.
            for v in l.iter() {
                new_vec.push(EVAL(v, menv)?);
            }
            Ok(Vector(Rc::new(new_vec)))
        },
        Map(l) => {
            let mut new_map: HashMap<String, MalVal> = HashMap::new();
            // Iterate with for-loop to raise the first Err encountered.
            for (k, v) in l.iter() {
                new_map.insert(k.clone(), EVAL(v, menv)?);
            }
            Ok(Map(Rc::new(new_map)))
        },
        Sym(s) => menv.get(&s),
        _ => Ok(ast.clone())
    }
}

fn is_true(cond: MalVal) -> bool {
    match cond {
        Bool(b) => b,
        Nil     => false,
        _       => true
    }
}

fn make_env(binds: &Rc<Vec<MalVal>>, outer_env: &Rc<MalEnv>) -> Result<Rc<MalEnv>, String> {
    let new_env = Rc::new(MalEnv {
        outer: Some(Rc::clone(outer_env)),
        data:  Rc::new(RefCell::new(HashMap::new()))
    });

    if binds.len() % 2 != 0 {
        return err!("binds for let* wasn't even")
    }

    for bind in binds.chunks(2) {
        match &bind[0] {
            Sym(k) => {
                let v = EVAL(&bind[1], &new_env)?;
                new_env.set(k.clone(), v);
            }
            _ => return err!("bind in let* wasn't a symbol")
        }
    }

    Ok(new_env)
}

fn make_fun_env(env: &Rc<MalEnv>, binds: &Rc<Vec<MalVal>>, args: &[MalVal]) -> Result<MalEnv, String> {
    let mut params = HashMap::new();

    for idx in 0 .. binds.len() {
        match &binds[idx] {
            Sym(s) => {
                if s == "&" {
                    if let Sym(rest_bind) = &binds[idx + 1] {
                        let rest_args = &args[idx .. args.len()];
                        params.insert(rest_bind.clone(), mlist!(rest_args.to_vec()));
                        break;
                    }
                    else {
                        return err!("Got a non-sym after & in binds")
                    }
                }
                else {
                    if idx < args.len() {
                        params.insert(s.clone(), args[idx].clone());
                    }
                    else {
                        return Err(format!("have {} binds but got {} args", binds.len(), args.len()));
                    }
                }
            }
            _ => return err!("Got a non-sym in fn*")
        }
    }

    Ok(MalEnv { outer: Some(Rc::clone(env)), data: Rc::new(RefCell::new(params)) })
}

fn call_user_fun(fun: &MalUserFn, args: &[MalVal]) -> MalRet {
    let inner_env = make_fun_env(&fun.env, &fun.binds, args)?;
    Ok(EVAL(&fun.body, &Rc::new(inner_env))?)
}

#[allow(non_snake_case)]
fn EVAL(cur_ast: &MalVal, cur_env: &Rc<MalEnv>) -> MalRet {
    //    println!("    EVAL: {}", pr_str(ast.clone(), true));

    let ast = Cell::new(cur_ast);
    let env = RefCell::new(Rc::clone(cur_env));

    'eval_loop: loop {
        match ast.get() {
            List(l) => {
                if l.is_empty() {
                    return Ok(ast.get().clone())
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
                                        let val = EVAL(&args[0], &env.borrow())?;
                                        env.borrow().set(s.clone(), val.clone());
                                        Ok(val)
                                    },
                                    _ => err!("First elem of def! wasn't a Sym")
                                }
                            }
                            "let*" => {
                                let binds = &rest[0];
                                let form = &rest[1];
                                match binds {
                                    List(bl) | Vector(bl) => {
                                        let new_env = make_env(bl, &env.borrow())?;
                                        env.replace(new_env);
                                        ast.replace(form);
                                        continue 'eval_loop
                                    }
                                    _ => return err!("First elem of let* wasn't a list/vec")
                                }
                            }
                            "do" => {
                                let(last, leading) = rest.split_last().unwrap();
                                eval_ast(&mlist!(leading.to_vec()), &env.borrow())?;
                                ast.replace(&last);
                                continue 'eval_loop
                            }
                            "if" => {
                                // rest[0] = cond, rest[1] = true branch, rest[2] = false branch
                                let next_ast = if is_true(EVAL(&rest[0], &env.borrow())?) {
                                    &rest[1]
                                }
                                else if rest.len() > 2 {
                                    &rest[2]
                                }
                                else {
                                   &Nil
                                };
                                ast.replace(next_ast);
                                continue 'eval_loop
                            }
                            "fn*" => {
                                let binds = &rest[0];
                                let body  = &rest[1];
                                return match binds {
                                    List(bl) | Vector(bl) => {
                                        Ok(
                                            UserFun(
                                                MalUserFn {
                                                    binds: bl.clone(),
                                                    body:  Rc::new(body.clone()),
                                                    env:   Rc::clone(&env.borrow())
                                                }
                                            )
                                        )
                                    }
                                    _ => err!("First elem of fn* wasn't a list/vec")
                                }
                            }
                            _ => { /* fallthrough to function call */ }
                        }
                    }

                    return if let List(fcall) = eval_ast(ast.get(), &env.borrow())? {
                        let (fun, args) = fcall.split_first().unwrap();
                        match fun {
                            CoreFun(f) => f(args),
                            UserFun(f) => call_user_fun(f, args),
                            _ => err!("Tried to call function on non-Fun")
                        }
                    }
                    else {
                        err!("Somehow eval_ast(List) didn't return a list?")
                    }
                }
            }
            v => return eval_ast(&v, &env.borrow())
        }
    }
}

#[allow(non_snake_case)]
fn PRINT(code: &MalVal) -> String {
    pr_str(code.clone(), true) // TODO Don't be so lazy with clone.
}

#[allow(non_snake_case)]
fn READ(code: String) -> MalRet {
    read_str(code)
}

fn rep(code: String, menv: &Rc<MalEnv>) -> Result<String, String> {
    let ast = READ(code)?;
    match EVAL(&ast, menv) {
        Ok(ast) => Ok(PRINT(&ast)),
        Err(e) => Err(e)
    }
}

fn main() {
    let repl_env = Rc::new(MalEnv { outer: None, data: Rc::new(RefCell::new(HashMap::new())) });

    for (k,v) in &core_ns() {
        repl_env.set(k.clone(), v.clone());
    }

    if let Err(e) = rep("(def! not (fn* (a) (if a false true)))".to_string(), &repl_env) {
        println!("Failed to eval not: {}", e);
    }

    loop {
        print!("user> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_)  => {
                let result = rep(input, &repl_env);
                println!("{}", match result { Ok(s) => s, Err(e) => e });
            }
            Err(_) => { break; }
        }
    }
}
