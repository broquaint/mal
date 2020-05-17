use std::collections::HashMap;

use regex::Regex;
use regex::Captures;

use ::mal_list;
use ::mal_map;
use types::MalVal::{self, Int, Sym, Bool, Str, Nil};
use types::VecLike;
use types::MapLike;

type ReaderRet = Result<MalVal, String>;

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
            Err(format!("Reached end of input unexpectedly in peek() at pos {}", self.pos))
        }
    }

    fn is_last(&self) -> bool {
        self.pos == self.tokens.len() - 1
    }
}

fn tokenize(input: String) -> Reader {
    let re = Regex::new(r###"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]+)"###).unwrap();

    let tokens = re.captures_iter(&input)
        .map(|caps| caps[1].to_string())
        .filter(|tok| tok.len() > 0 && !tok.starts_with(";"))
        .collect();

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

pub fn mal_sym(s: &str) -> MalVal {
    Sym(s.to_string())
}

// https://graphemica.com/%CA%9E
pub static KW_PREFIX: &str = "\u{029E}"; // : => ʞ

pub fn make_keyword(kw: &str) -> String {
    format!("{}{}", KW_PREFIX, kw)
}

fn read_atom(r: &mut Reader) -> ReaderRet {
    let tok = r.next()?;
    // Handle quote specifically to avoid panic.
    if tok.starts_with("\"") || tok.starts_with(":") {
        return if tok.len() > 1 {
            Ok(Str(
                if tok.starts_with("\"") {
                    make_string(&tok[1 .. tok.len() - 1])
                }
                else {
                    // This is a keyword but implemented as Str for type sanity.
                    make_keyword(&tok[1 .. tok.len()])
                }
            ))
        }
        else {
            Err("Reach end of input, missing quote/symbol".to_string())
        }
    }
    else if tok == "nil" {
        Ok(Nil)
    }
    else if tok == "true" || tok == "false" {
        Ok(Bool(tok == "true"))
    }
    else if tok == "@" {
        let atom = r.next()?.clone();
        Ok(mal_list![mal_sym("deref"), Sym(atom)])
    }
    else if tok == "'" {
        Ok(mal_list![mal_sym("quote"), read_form(r)?])
    }
    else if tok == "`" {
        Ok(mal_list![mal_sym("quasiquote"), read_form(r)?])
    }
    else if tok == "~" {
        Ok(mal_list![mal_sym("unquote"), read_form(r)?])
    }
    else if tok == "~@" {
        Ok(mal_list![mal_sym("splice-unquote"), read_form(r)?])
    }
    else if tok == "^" {
        let meta = read_form(r)?;
        let val  = read_form(r)?;
        Ok(mal_list![mal_sym("with-meta"), val, meta])
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

fn read_seq(r: &mut Reader, end: &str) -> Result<Vec<MalVal>, String> {
    // We know that the current token is ( at this point.
    #[allow(unused_must_use)]
    r.next()?;
    let mut list: Vec<MalVal> = Vec::new();

    while r.peek()? != end {
        list.push(read_form(r)?);
    }

    // Move past the end paren.
    if !r.is_last() {
        r.next()?;
    }

    return Ok(list);
}

fn read_list(r: &mut Reader) -> ReaderRet {
    // Not using as_mal_list! in sympathy with read_vec.
    Ok(VecLike::as_list(read_seq(r, ")")?))
}

fn read_vec(r: &mut Reader) -> ReaderRet {
    Ok(VecLike::as_vec(read_seq(r, "]")?))
}

fn read_map(r: &mut Reader) -> ReaderRet {
    let pairs = read_seq(r, "}")?;

    if pairs.len() % 2 == 0 {
        let mut map : HashMap<String, MalVal> = HashMap::new();
        for pair in pairs.chunks(2) {
            match &pair[0] {
                Str(s) => { map.insert(s.clone(), pair[1].clone()); }
                _      => return Err("only Str can be use as a map key".to_string())
            }
        }
        Ok(mal_map![map])
    }
    else {
        Err(format!("maps requires an even number of items, got {} items", pairs.len()))
    }
}

fn read_form(r: &mut Reader) -> ReaderRet {
    let tok = r.peek()?;
//    println!("rf tok = {}", tok);
    match tok.as_str() {
        "(" => read_list(r),
        "[" => read_vec(r),
        "{" => read_map(r),
        _   => read_atom(r),
    }
}

pub fn read_str(input: String) -> ReaderRet {
    read_form(&mut tokenize(input))
}
