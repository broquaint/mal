use std::rc::Rc;
use std::collections::HashMap;

use env::MalEnv;

type MalRet = Result<MalVal, String>;
pub type MalFnSig = fn(&[MalVal]) -> MalRet;

#[derive(Clone)]
pub struct MalUserFn {
    pub binds: Rc<Vec<MalVal>>,
    pub body:  Rc<MalVal>,
    pub env:   MalEnv,
}

#[derive(Clone)]
pub enum MalVal {
    Int(i64),
    Str(String),
    Sym(String),
    Bool(bool),
    Nil,
    List(Rc<Vec<MalVal>>),
    Vector(Rc<Vec<MalVal>>),
    Map(Rc<HashMap<String, MalVal>>),
    UserFun(MalUserFn),
    CoreFun(MalFnSig),
}
