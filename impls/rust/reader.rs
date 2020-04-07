use std::rc::Rc;
use regex::Regex;

use types::MalVal::{self, Int, Sym, List};

#[derive(Debug)]
struct Reader {
    pos: usize,
    tokens: Vec<String>
}

impl Reader {
    fn next(&mut self) -> &String {
        let tok = &self.tokens[self.pos];
        self.pos += 1;
        return tok;
    }
    fn peek(&self) -> &String {
        &self.tokens[self.pos]
    }
}

// [\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)
fn tokenize(input: String) -> Reader {
    let re = Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\.|[^"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#).unwrap();
    let mut tokens: Vec<String> = Vec::new();
    // TODO, handle no match!
    for tok in re.captures_iter(&input) {
        tokens.push(String::from(tok[0].trim().trim_matches(',')));
    }

//    println!("input : {}", input);
//    println!("tokens: {:?}", tokens);
    return Reader { pos: 0, tokens };
}

fn is_number(tok: &String) -> bool {
    // Only supporting ints.
    Regex::new(r"^-?\d+$").unwrap().is_match(tok)
}

fn read_atom(r: &mut Reader) -> MalVal {
    let tok = r.next();
    return if is_number(tok) {
        Int(tok.parse::<i64>().unwrap())
    }
    else {
        Sym(tok.clone())
    }
}

fn read_list(r: &mut Reader) -> MalVal {
    r.next();
    let mut list: Vec<MalVal> = Vec::new();
    while r.peek() != ")" {
        list.push(read_form(r));
    }
    return List(Rc::new(list));
}

fn read_form(r: &mut Reader) -> MalVal {
    let tok = r.peek();
    match tok.as_str() {
        "(" => read_list(r),
        _   => read_atom(r),
    }
}

pub fn read_str(input: String) -> MalVal {
    let mut r = tokenize(input);
    read_form(&mut r)
}
