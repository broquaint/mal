use std::io::{self, Write};
use std::collections::HashMap;
use std::rc::Rc;

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
use core::core_ns;

macro_rules! err {
    ($e:expr) => { Err($e.to_string()) }
}

type MalRet = Result<MalVal, String>;

fn eval_ast(ast: &MalVal, menv: &mut MalEnv) -> MalRet {
//    println!("eval_ast: {}", pr_str(ast.clone(), true));
    match ast {
        List(l) => {
            let mut new_list: Vec<MalVal> = Vec::new();
            // Iterate with for-loop to raise the first Err encountered.
            for v in l.iter() {
                new_list.push(EVAL(&v, menv)?);
            }
            Ok(List(Rc::new(new_list)))
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

fn make_env(binds: &Rc<Vec<MalVal>>, new_env: &mut MalEnv) -> Result<bool, String> {
    if binds.len() % 2 != 0 {
        return err!("binds for let* wasn't even")
    }

    for bind in binds.chunks(2) {
        match &bind[0] {
            Sym(k) => {
                let v = EVAL(&bind[1], new_env)?;
                new_env.set(k.clone(), v);
            }
            _ => return err!("bind in let* wasn't a symbol")
        }
    }

    // XXX Bleurgh, really just want early error on non-sym.
    Ok(true)
}

fn is_true(cond: MalVal) -> bool {
    match cond {
        Bool(b) => b,
        Nil     => false,
        _       => true
    }
}

fn call_user_fun(fun: &MalUserFn, args: &[MalVal]) -> MalRet{
    let mut fun_env = fun.env.make_inner_with(&fun.binds, args)?;
    Ok(EVAL(&fun.body, &mut fun_env)?)
}

#[allow(non_snake_case)]
fn EVAL(ast: &MalVal, menv: &mut MalEnv) -> MalRet {
//    println!("    EVAL: {}", pr_str(ast.clone(), true));
    match ast {
        List(l) => {
            if l.is_empty() {
                Ok(ast.clone())
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
                                    let val = EVAL(&args[0], menv)?;
                                    menv.set(s.clone(), val.clone());
                                    Ok(val)
                                },
                                _ => err!("First elem of def! wasn't a Sym")
                            }
                        }
                        "let*" => {
                            let binds = &rest[0];
                            let form  = &rest[1];
                            return match binds {
                                List(bl) | Vector(bl) => {
                                    let mut inner_env = menv.make_inner();
                                    make_env(bl, &mut inner_env)?;
                                    Ok(EVAL(form, &mut inner_env)?)
                                }
                                _ => err!("First elem of let* wasn't a list/vec")
                            }
                        }
                        "do" => {
                            let(last, leading) = rest.split_last().unwrap();
                            for elem in leading {
                                EVAL(&elem, menv)?;
                            }
                            return Ok(EVAL(&last, menv)?)
                        }
                        "if" => {
                            // rest[0] = cond, rest[1] = true branch, rest[2] = false branch
                            return if is_true(EVAL(&rest[0], menv)?) {
                                Ok(EVAL(&rest[1], menv)?)
                            }
                            else if rest.len() > 2 {
                                Ok(EVAL(&rest[2], menv)?)
                            }
                            else {
                                Ok(Nil)
                            }
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
                                                env:   menv.clone()
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

                if let List(fcall) = eval_ast(ast, menv)? {
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
        v => eval_ast(&v, menv)
    }
}

#[allow(non_snake_case)]
fn PRINT(code: &MalVal) -> String {
    pr_str(code.clone(), true) // TODO Don't be so lazy with clone.
}

#[allow(non_snake_case)]
fn READ(code: String) -> Result<MalVal, String> {
    read_str(code)
}

fn rep(code: String, menv: &mut MalEnv) -> Result<String, String> {
    let ast = READ(code)?;
    match EVAL(&ast, menv) {
        Ok(ast) => Ok(PRINT(&ast)),
        Err(e) => Err(e)
    }
}

fn main() {
    let mut repl_env = MalEnv { outer: None, data: HashMap::new() };

    for (k,v) in &core_ns() {
        repl_env.set(k.clone(), v.clone());
    }

    loop {
        print!("user> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_)  => {
                let result = rep(input, &mut repl_env);
                println!("{}", match result { Ok(s) => s, Err(e) => e });
            }
            Err(_) => { break; }
        }
    }
}
