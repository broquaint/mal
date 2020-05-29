use types::MalVal::{self, *};
use reader::{KW_PREFIX};
use regex::{Regex, Captures};

pub fn malstr_as_string(s: String, print_readably: bool) -> String {
    if s.starts_with(KW_PREFIX) {
        // ʞ => : where ʞ is two bytes wide.
        format!(":{}", &s[2 .. s.len()])
    } else if print_readably {
        let unescapes = Regex::new("(\"|\n|\\\\)").unwrap();
        format!(r#""{}""#, unescapes.replace_all(&s, |c: &Captures| match &c[1] {
            "\"" => r#"\""#, // " => \"
            "\n" => "\\n",   // ␤ => \n
            r"\" => r"\\",   // \ => \\
            _    => panic!("How did we get here in malstr_as_string?")
        })).to_string()
    } else {
        s
    }
}

pub fn pr_str(val: MalVal, print_readably: bool) -> String {
    // Helpers to avoid repeating print_readably.
    let pr     = |v| pr_str(v, print_readably);
    let as_str = |v| malstr_as_string(v, print_readably);

    match &val {
        Int(n) => n.to_string(),
        Sym(s) => s.clone(),
        Bool(b) => (if *b { "true" } else { "false" }).to_string(),
        Nil  => "nil".to_string(),
        Str(s) => as_str(s.clone()),
        List(l) => format!("({})", l.iter()
                           .map(|v| { pr(v.clone()) })
                           .collect::<Vec<String>>().join(" ")),
        Vector(l) => format!("[{}]", l.iter()
                           .map(|v| { pr(v.clone()) })
                             .collect::<Vec<String>>().join(" ")),
        // It seems {{}} is the \ of format!()
        // https://doc.rust-lang.org/std/fmt/index.html#escaping
        Map(m) => format!("{{{}}}", m.iter()
                          .map(|(k,v)| format!("{} {}", as_str(k.clone()), pr(v.clone())))
                          .collect::<Vec<String>>().join(" ")),
        CoreFun(_) | UserFun(_) => String::from("#<function>"),
        Atom(a) => format!("(atom {})", pr(a.borrow().clone())),
    }
}

pub fn rs_pr_str(val: MalVal) -> String {
    let pr     = |v| rs_pr_str(v);
    let as_str = |v| malstr_as_string(v, true);

    match &val {
        Int(n) => format!("Int({})", n.to_string()),
        Sym(s) => format!("Sym({})", s),
        Bool(b) => format!("Bool({})", (if *b { "true" } else { "false" }).to_string()),
        Nil  => "Nil(nil)".to_string(),
        Str(s) => format!("Str({})", as_str(s.clone())),
        List(l) => format!("List({})", l.iter()
                           .map(|v| { pr(v.clone()) })
                           .collect::<Vec<String>>().join(" ")),
        Vector(l) => format!("Vector[{}]", l.iter()
                           .map(|v| { pr(v.clone()) })
                             .collect::<Vec<String>>().join(" ")),
        // It seems {{}} is the \ of format!()
        // https://doc.rust-lang.org/std/fmt/index.html#escaping
        Map(m) => format!("Map{{{}}}", m.iter()
                          .map(|(k,v)| format!("{} {}", as_str(k.clone()), pr(v.clone())))
                          .collect::<Vec<String>>().join(" ")),
        CoreFun(_) => String::from("#<core function>"),
        UserFun(_) => String::from("#<user function>"),
        Atom(a) => format!("Atom(atom {})", pr(a.borrow().clone())),
    }
}
