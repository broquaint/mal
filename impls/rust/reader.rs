use std::rc::Rc;

use regex::Regex;
use regex::Captures;

use types::MalVal::{self, Int, Sym, Str, List};

#[derive(Debug)]
struct Reader {
    pos: usize,
    tokens: Vec<String>
}

impl Reader {
    fn next(&mut self) -> Result<&String, String> {
        return if self.pos < self.tokens.len() {
            let tok = &self.tokens[self.pos];
            self.pos += 1;
            Ok(tok)
        }
        else {
            Err(String::from("Reached end of input unexpectedly in next()"))
        }
    }

    fn peek(&self) -> Result<&String, String> {
        return if self.pos < self.tokens.len() {
            Ok(&self.tokens[self.pos])
        }
        else {
            Err(String::from("Reached end of input unexpectedly in peek()"))
        }
    }

    fn is_last(&self) -> bool {
        self.pos == self.tokens.len() - 1
    }
}

// [\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)
fn tokenize(input: String) -> Reader {
    let re = Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\"|[^"])*"?|;.*|[^\s\[\]{}('"`,;)]*)[\s,]*"#).unwrap();
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

fn make_string(tok: &str) -> String {
    let escapes = Regex::new(r#"(\\"|\\n|\\\\)"#).unwrap();
    escapes.replace_all(tok, |c: &Captures| match &c[1] {
        r#"\""# => r#"""#,
        r"\n" => "\n",
        r"\\" => r"\",
        _ => panic!("How did we get here in make_string?")
    }).to_string()
}

fn read_atom(r: &mut Reader) -> Result<MalVal, String> {
    let tok = r.next()?;
    // Handle quote specifically to avoid panic.
    if tok.starts_with("\"") {
        return if tok.len() > 1 {
            Ok(Str(make_string(&tok[1 .. tok.len() - 1])))
        }
        else {
            Err("Reach end of input, missing quote".to_string())
        }
    }
    else {   
        Ok(
            if is_number(tok) {
                Int(tok.parse::<i64>().unwrap())
            }
            else {
                Sym(tok.clone())
            }
        )
    }
}

fn read_list(r: &mut Reader) -> Result<MalVal, String> {
    // We know that the current token is ( at this point.
    #[allow(unused_must_use)]
    r.next()?;
    let mut list: Vec<MalVal> = Vec::new();

    while r.peek()? != ")" {
        list.push(read_form(r)?);
    }

    // Move past the end paren.
    if !r.is_last() {
        r.next()?;
    }

    return Ok(List(Rc::new(list)));
}

fn read_form(r: &mut Reader) -> Result<MalVal, String> {
    let tok = r.peek()?;
//    println!("rf tok = {}", tok);
    match tok.as_str() {
        "(" => read_list(r),
        _   => read_atom(r),
    }
}

pub fn read_str(input: String) -> Result<MalVal, String> {
    read_form(&mut tokenize(input))
}
