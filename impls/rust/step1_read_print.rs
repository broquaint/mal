use std::io::{self, Write};

extern crate regex;

// This works ... but is clearly a wrongun.
mod types;
use types::MalVal;
mod reader;
use reader::read_str;
mod printer;
use printer::pr_str;

#[allow(non_snake_case)]
fn READ(code: String) -> MalVal {
    read_str(code)
}

#[allow(non_snake_case)]
fn EVAL(code: MalVal) -> MalVal {
    code
}

#[allow(non_snake_case)]
fn PRINT(code: MalVal) -> String {
    pr_str(code)
}

fn rep(code: String) -> String {
    PRINT(EVAL(READ(code)))
}

fn main() {
    loop {
        print!("user> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_)  => { println!("{}", rep(input)); }
            Err(_) => { break; }
        }
    }
}
