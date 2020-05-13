use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use std::ops::Index;
use std::slice::Iter;
use std::slice::Chunks;

use env::MalEnv;
use printer::pr_str;

pub type MalRet   = Result<MalVal, MalErr>;
pub type MalFnSig = fn(&[MalVal]) -> MalRet;

#[derive(Clone)]
pub struct MalUserFn {
    pub binds:    Box<VecLike>,
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
pub struct VecLike {
    pub v:    Box<Vec<MalVal>>,
    pub meta: Box<MalVal>,
}

impl VecLike {
    pub fn new(v: MalVal) -> VecLike {
        VecLike { v: Box::new(vec![v]), meta: Box::new(MalVal::Nil) }
    }

    pub fn from_vec(vv: Vec<MalVal>) -> VecLike {
        VecLike { v: Box::new(vv), meta: Box::new(MalVal::Nil) }
    }

    // Implement some commonly used Vec methods, hence VecLike

    pub fn len(&self) -> usize {
        self.v.len()
    }

    pub fn is_empty(&self) -> bool {
        self.v.is_empty()
    }

    pub fn iter(&self) -> Iter<MalVal> {
        self.v.iter()
    }

    pub fn as_slice(&self) -> &[MalVal] {
        self.v.as_slice()
    }

    pub fn chunks(&self, chunk_size: usize) -> Chunks<MalVal> {
        self.v.chunks(chunk_size)
    }

    pub fn split_first(&self) -> (&MalVal, &[MalVal]) {
        // We only call split_first() when it's safe to unwrap.
        self.v.split_first().unwrap()
    }
}

impl Index<usize> for VecLike {
    type Output = MalVal;

    fn index(&self, idx: usize) -> &Self::Output {
        &self.v[idx]
    }
}

#[derive(Clone)]
pub enum MalVal {
    Int(i64),
    Str(String),
    Sym(String),
    Bool(bool),
    Nil,
    List(VecLike),
    Vector(VecLike),
    Map(Rc<HashMap<String, MalVal>>),
    UserFun(MalUserFn),
    CoreFun(MalFnSig),
    Atom(Rc<RefCell<MalVal>>),
}

impl MalVal {
    pub fn as_vec(&self) -> Result<Rc<Vec<MalVal>>, MalErr> {
        match self {
            MalVal::List(l) | MalVal::Vector(l) => Ok(Rc::new(*l.v.clone())),
            _ => Err(MalErr(Rc::new(MalVal::Str(format!("Can't treat '{}' as list/vec", pr_str(self.clone(), true))))))
        }
    }
}
