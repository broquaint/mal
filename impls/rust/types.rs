use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum MalVal {
    Int(i64),
    Str(String),
    Sym(String),
    List(Rc<Vec<MalVal>>)
}
