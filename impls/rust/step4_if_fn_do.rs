use std::io::{self, Write};
use std::collections::HashMap;
use std::rc::Rc;

extern crate regex;

mod types;
use types::MalVal::{self, Int, Sym, List, Vector, Map, Fun};
mod reader;
use reader::read_str;
mod printer;
use printer::pr_str;
mod env;
use env::MalEnv;

macro_rules! err {
    ($e:expr) => { Err($e.to_string()) }
}

type MalRet = Result<MalVal, String>;

fn eval_ast(ast: &MalVal, menv: &mut MalEnv) -> MalRet {
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

#[allow(non_snake_case)]
fn EVAL(ast: &MalVal, menv: &mut MalEnv) -> MalRet {
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
                            match name {
                                Sym(s) => {
                                    let val = EVAL(&args[0], menv)?;
                                    menv.set(s.clone(), val.clone());
                                    Ok(val)
                                },
                                _ => err!("First elem of def! wasn't a Sym")
                            }
                        },
                        "let*" => {
                            let binds = &rest[0];
                            let form  = &rest[1];
                            match binds {
                                List(bl) | Vector(bl) => {
                                    let mut inner_env = menv.make_inner();
                                    make_env(bl, &mut inner_env)?;
                                    Ok(EVAL(form, &mut inner_env)?)
                                }
                                _ => err!("First elem of let* wasn't a list/vec")
                            }
                        }
                        _ => {
                            if let List(fcall) = eval_ast(ast, menv)? {
                                let (fun, args) = fcall.split_first().unwrap();
                                match fun {
                                    Fun(f) => f(args),
                                    _ => err!("Tried to call function on non-Fun")
                                }
                            }
                            else {
                                err!("Somehow eval_ast(List) didn't return a list?")
                            }
                        }
                    }
                }
                else {
                    Err(format!("got non-sym '{}' at the head of a list?", pr_str(sym.clone())))
                }
            }
        }
        v => eval_ast(&v, menv)
    }
}

#[allow(non_snake_case)]
fn PRINT(code: &MalVal) -> String {
    pr_str(code.clone()) // TODO Don't be so lazy with clone.
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

fn mal_add(args: &[MalVal]) -> MalRet {
    match (&args[0], &args[1]) {
        (Int(a), Int(b)) => Ok(MalVal::Int(a + b)),
        _ => err!("Can only add two Ints!")
    }
}


fn mal_sub(args: &[MalVal]) -> MalRet {
    match (&args[0], &args[1]) {
        (Int(a), Int(b)) => Ok(MalVal::Int(a - b)),
        _ => err!("Can only subtract two Ints!")
    }
}

fn mal_mul(args: &[MalVal]) -> MalRet {
    match (&args[0], &args[1]) {
        (Int(a), Int(b)) => Ok(MalVal::Int(a * b)),
        _ => err!("Can only multiply two Ints!")
    }
}

fn mal_div(args: &[MalVal]) -> MalRet {
    match (&args[0], &args[1]) {
        (Int(a), Int(b)) => Ok(MalVal::Int(a / b)),
        _ => err!("Can only divide two Ints!")
    }
}

fn main() {
    let mut repl_env = MalEnv { outer: None, data: HashMap::new() };
    repl_env.set("+".to_string(), Fun(mal_add));
    repl_env.set("-".to_string(), Fun(mal_sub));
    repl_env.set("*".to_string(), Fun(mal_mul));
    repl_env.set("/".to_string(), Fun(mal_div));

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
