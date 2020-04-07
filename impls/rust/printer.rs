use types::MalVal::{self, Int, Sym, List};

pub fn pr_str(val: MalVal) -> String {
    match val {
        Int(n) => n.to_string(),
        Sym(s) => s,
        List(l) => format!("({})", l.iter()
                           .map(|v| { pr_str(v.clone()) })
                           .collect::<Vec<String>>().join(" ")),
    }
}
