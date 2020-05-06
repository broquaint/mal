use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use env::MalEnv;

pub type MalRet   = Result<MalVal, MalErr>;
pub type MalFnSig = fn(&[MalVal]) -> MalRet;

#[derive(Clone)]
pub struct MalUserFn {
    pub binds:    Rc<Vec<MalVal>>,
    pub body:     Rc<MalVal>,
    pub env:      Rc<MalEnv>,
    pub is_macro: bool,
    // Probably gross way of exposing EVAL into core.
    pub eval:  fn(Rc<MalVal>, &Rc<MalEnv>) -> MalRet,
}

#[derive(Clone)]
pub struct MalErr(pub Rc<MalVal>);

impl MalErr {
    pub fn to_val(self) -> MalVal {
        let MalErr(r) = self;
        (*r).clone()
    }
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
    Atom(Rc<RefCell<MalVal>>),
}
