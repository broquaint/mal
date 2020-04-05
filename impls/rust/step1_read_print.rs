use std::io;

fn main() {
    loop {
        println!("user> ");
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                println!("{}", input);
            }
            Err(error) => println!("error: {}", error),
        }
    }
}
