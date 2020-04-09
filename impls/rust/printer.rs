use types::MalVal::{self, Int, Sym, Str, List, Vector, Map};
use reader::{KW_PREFIX};

use regex::{Regex, Captures};

fn malstr_as_string(s: String) -> String {
    if s.starts_with(KW_PREFIX) {
        // ʞ => : where ʞ is two bytes wide.
        format!(":{}", &s[2 .. s.len()])
    }
    else {
        let unescapes = Regex::new("(\"|\n|\\\\)").unwrap();
        format!(r#""{}""#, unescapes.replace_all(&s, |c: &Captures| match &c[1] {
            "\"" => r#"\""#, // " => \"
            "\n" => "\\n",   // ␤ => \n
            r"\" => r"\\",   // \ => \\
            _    => panic!("How did we get here in malstr_as_string?")
        })).to_string()
    }
}

pub fn pr_str(val: MalVal) -> String {
    match val {
        Int(n) => n.to_string(),
        Sym(s) => s,
        Str(s) => malstr_as_string(s),
        List(l) => format!("({})", l.iter()
                           .map(|v| { pr_str(v.clone()) })
                           .collect::<Vec<String>>().join(" ")),
        Vector(l) => format!("[{}]", l.iter()
                           .map(|v| { pr_str(v.clone()) })
                             .collect::<Vec<String>>().join(" ")),
        // It seems {{}} is the \ of format!()
        // https://doc.rust-lang.org/std/fmt/index.html#escaping
        Map(m) => format!("{{{}}}", m.iter()
                          .map(|(k,v)| format!("{} {}", malstr_as_string(k.clone()), pr_str(v.clone())))
                          .collect::<Vec<String>>().join(" ")),
    }
}
