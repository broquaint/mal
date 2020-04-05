use std::io::{self, Write};

#[allow(non_snake_case)]
fn READ(code: String) -> String {
    code
}

#[allow(non_snake_case)]
fn EVAL(code: String) -> String {
    code
}

#[allow(non_snake_case)]
fn PRINT(code: String) -> String {
    code
}

fn rep(code: String) -> String {
    PRINT(READ(EVAL(code)))
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
