use std::rc::Rc;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum MalVal {
    Int(i64),
    Str(String),
    Sym(String),
    List(Rc<Vec<MalVal>>),
    Vector(Rc<Vec<MalVal>>),
    Map(Rc<HashMap<String, MalVal>>),
}
