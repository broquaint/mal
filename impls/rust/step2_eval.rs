use std::io::{self, Write};
use std::collections::HashMap;
use std::rc::Rc;

extern crate regex;

mod types;
//use types::MalVal;
use types::MalVal::{self, Int, Sym, List, Fun};
mod reader;
use reader::read_str;
mod printer;
use printer::pr_str;

type MalRet = Result<MalVal, String>;
type MalEnv = HashMap<String, MalVal>;

fn eval_ast(ast: &MalVal, menv: &MalEnv) -> MalRet {
    match ast {
        List(l) => {
            let mut new_list: Vec<MalVal> = Vec::new();
            // Iterate with for-loop to raise the first Err encountered.
            for v in l.iter() {
                // TODO Change List to List(Rc(&MalVal)) to avoid excess clones.
                new_list.push(EVAL(&v, &menv)?);
            }
            Ok(List(Rc::new(new_list)))
        },
        Sym(s) => if menv.contains_key(s.as_str()) {
                Ok(menv.get(s.as_str()).unwrap().clone())
            }
            else {
                Err(format!("Unknown symbol '{}'", &s))
            }
        _ => Ok(ast.clone())
    }
}

#[allow(non_snake_case)]
fn EVAL(ast: &MalVal, menv: &MalEnv) -> MalRet {
    match ast {
        List(l) => {
            if l.is_empty() {
                Ok(ast.clone())
            }
            else {
                if let List(fcall) = eval_ast(ast, menv)? {
                    // Shouldn't panic as we know l isn't empty.
                    let (fun, args) = fcall.split_first().unwrap();
                    match fun {
                        Fun(f) => f(args),
                        _ => Err("Tried to call function on non-Fun".to_string())
                    }
                }
                else {
                    Err("Somehow eval_ast(List) didn't return a list?".to_string())
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

fn rep(code: String, menv: &MalEnv) -> Result<String, String> {
    let ast = READ(code)?;
    match EVAL(&ast, menv) {
        Ok(ast) => Ok(PRINT(&ast)),
        Err(e)  => Err(e)
    }
}

fn mal_add(args: &[MalVal]) -> MalRet {
    match (&args[0], &args[1]) {
        (Int(a), Int(b)) => Ok(MalVal::Int(a + b)),
        _ => Err("Can only add two Ints!".to_string())
    }
}


fn mal_sub(args: &[MalVal]) -> MalRet {
    match (&args[0], &args[1]) {
        (Int(a), Int(b)) => Ok(MalVal::Int(a - b)),
        _ => Err("Can only subtract two Ints!".to_string())
    }
}

fn mal_mul(args: &[MalVal]) -> MalRet {
    match (&args[0], &args[1]) {
        (Int(a), Int(b)) => Ok(MalVal::Int(a * b)),
        _ => Err("Can only multiply two Ints!".to_string())
    }
}

fn mal_div(args: &[MalVal]) -> MalRet {
    match (&args[0], &args[1]) {
        (Int(a), Int(b)) => Ok(MalVal::Int(a / b)),
        _ => Err("Can only divide two Ints!".to_string())
    }
}

fn main() {
    let mut repl_env : MalEnv = HashMap::new();
    repl_env.insert("+".to_string(), Fun(mal_add));
    repl_env.insert("-".to_string(), Fun(mal_sub));
    repl_env.insert("*".to_string(), Fun(mal_mul));
    repl_env.insert("/".to_string(), Fun(mal_div));

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
