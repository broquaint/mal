use types::MalVal::{self, Int, Sym, Str, List};

use regex::{Regex, Captures};

fn malstr_as_string(s: String) -> String {
    let unescapes = Regex::new("(\"|\n|\\\\)").unwrap();
    format!(r#""{}""#, unescapes.replace_all(&s, |c: &Captures| match &c[1] {
        "\"" => r#"\""#, // "  => \"
        "\n" => "\\n",   // NL => \n
        r"\" => r"\\",   // \  => \\
        _    => panic!("How did we get here in malstr_as_string?")
    })).to_string()
}

pub fn pr_str(val: MalVal) -> String {
    match val {
        Int(n) => n.to_string(),
        Sym(s) => s,
        Str(s) => malstr_as_string(s),
        List(l) => format!("({})", l.iter()
                           .map(|v| { pr_str(v.clone()) })
                           .collect::<Vec<String>>().join(" ")),
    }
}
