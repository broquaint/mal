use std::rc::Rc;
use std::collections::HashMap;

type MalRet = Result<MalVal, String>;
pub type MalFn = fn(&[MalVal]) -> MalRet;

#[derive(Clone)]
pub enum MalVal {
    Int(i64),
    Str(String),
    Sym(String),
    List(Rc<Vec<MalVal>>),
    Vector(Rc<Vec<MalVal>>),
    Map(Rc<HashMap<String, MalVal>>),
    Fun(MalFn)
}
