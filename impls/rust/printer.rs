use std::rc::Rc;

use types::MalVal;

pub fn pr_str(val: MalVal) -> String {
    match val {
        MalVal::Int(n) => n.to_string(),
        MalVal::Sym(s) => s,
        MalVal::List(l) => format!(
            "({})",
            l.iter()
                .map(|v| { pr_str(Rc::try_unwrap(v.clone()).unwrap()) })
                .collect::<Vec<String>>().join(" ")
        )
    }
}
