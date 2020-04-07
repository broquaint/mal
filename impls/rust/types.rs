use std::rc::Rc;

#[derive(Debug)]
pub enum MalVal {
    Int(i64),
    Sym(String),
    List(Vec<Rc<MalVal>>)
}
