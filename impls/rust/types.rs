use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum MalVal {
    Int(i64),
    Sym(String),
    List(Rc<Vec<MalVal>>)
}
