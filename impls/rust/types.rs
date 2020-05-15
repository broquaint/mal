use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use std::ops::Index;
use std::slice::Iter as SliceIter;
use std::slice::Chunks;
use std::collections::hash_map::Keys;
use std::collections::hash_map::Values;
use std::collections::hash_map::Iter as MapIter;

use env::MalEnv;
use printer::pr_str;
use core::make_bound_env;

use ::as_mal_err;
use ::errf;
use ::v_to_str;

pub type MalRet   = Result<MalVal, MalErr>;
pub type MalFnSig = fn(&[MalVal]) -> MalRet;

// Probably gross way of exposing EVAL into core.
type EvalSig = fn(Rc<MalVal>, &Rc<MalEnv>) -> MalRet;

#[derive(Clone)]
pub struct MalCoreFn {
    pub fun:  MalFnSig,
    meta_val: Box<MalVal>,
}

impl MalCoreFn {
    pub fn new(fun: MalFnSig, meta: MalVal) -> MalCoreFn {
        MalCoreFn { fun, meta_val: Box::new(meta) }
    }

    pub fn as_fun(fun: MalFnSig) -> MalVal {
        MalVal::CoreFun(MalCoreFn::new(fun, MalVal::Nil))
    }
}

#[derive(Clone)]
pub struct MalUserFn {
    pub binds:    Box<VecLike>,
    pub body:     Rc<MalVal>,
    pub env:      Rc<MalEnv>,
    pub is_macro: bool,
    pub eval:     EvalSig,
    meta_val:     Box<MalVal>,
}

impl MalUserFn {
    pub fn new(binds: VecLike, body: MalVal, env: Rc<MalEnv>, is_macro: bool, eval: EvalSig, meta: MalVal) -> MalUserFn {
        MalUserFn {
            binds:    Box::new(binds),
            body:     Rc::new(body),
            env,
            is_macro,
            eval,
            meta_val: Box::new(meta)
        }
    }

    pub fn as_fun(binds: VecLike, body: MalVal, env: Rc<MalEnv>, is_macro: bool, eval: EvalSig) -> MalVal {
        MalVal::UserFun(MalUserFn::new(binds, body, env, is_macro, eval, MalVal::Nil))
    }
}

pub trait MalCallable {
    fn call(&self, args: &[MalVal]) -> MalRet;
}

impl MalCallable for MalCoreFn {
    fn call(&self, args: &[MalVal]) -> MalRet {
        (self.fun)(args)
    }
}

impl MalCallable for MalUserFn {
    fn call(&self, args: &[MalVal]) -> MalRet {
        let inner_env = make_bound_env(&self.env, &self.binds, args)?;
        Ok((self.eval)(Rc::clone(&self.body), &Rc::new(inner_env))?)
    }
}

#[derive(Clone)]
pub struct VecLike {
    v:    Box<Vec<MalVal>>,
    meta_val: Box<MalVal>,
}

// Implement some commonly used Vec methods, hence VecLike
impl VecLike {
    pub fn new(val: Vec<MalVal>, meta: MalVal) -> VecLike {
        VecLike { v: Box::new(val), meta_val: Box::new(meta) }
    }

    pub fn as_list(val: Vec<MalVal>) -> MalVal {
        MalVal::List(VecLike::new(val, MalVal::Nil))
    }

    pub fn as_vec(val: Vec<MalVal>) -> MalVal {
        MalVal::Vector(VecLike::new(val, MalVal::Nil))
    }

    pub fn to_vec(&self) -> Vec<MalVal> {
        *self.v.clone()
    }

    pub fn len(&self) -> usize {
        self.v.len()
    }

    pub fn is_empty(&self) -> bool {
        self.v.is_empty()
    }

    pub fn iter(&self) -> SliceIter<MalVal> {
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

    pub fn rest(&self) -> MalVal {
        VecLike::as_list(self.v[1 ..].to_vec())
    }
}

impl Index<usize> for VecLike {
    type Output = MalVal;

    fn index(&self, idx: usize) -> &Self::Output {
        &self.v[idx]
    }
}

#[derive(Clone)]
pub struct MapLike {
    v:     Box<HashMap<String, MalVal>>,
    meta_val: Box<MalVal>,
}

impl MapLike {
    pub fn new(val: HashMap<String, MalVal>, meta: MalVal) -> MapLike {
        MapLike { v: Box::new(val), meta_val: Box::new(meta) }
    }

    pub fn as_map(val: HashMap<String, MalVal>) -> MalVal {
        MalVal::Map(MapLike::new(val, MalVal::Nil))
    }

    pub fn clone_map(&self) -> HashMap<String, MalVal> {
        *self.v.clone()
    }

    pub fn len(&self) -> usize {
        self.v.len()
    }

    pub fn iter(&self) -> MapIter<String, MalVal> {
        self.v.iter()
    }

    pub fn keys(&self) -> Keys<String, MalVal> {
        self.v.keys()
    }

    pub fn values(&self) -> Values<String, MalVal> {
        self.v.values()
    }

    pub fn get(&self, k: &String) -> Option<&MalVal> {
        self.v.get(k)
    }

    pub fn contains_key(&self, k: &String) -> bool {
        self.v.contains_key(k)
    }
}

pub trait HasMeta<T> {
    fn get_meta(&self) -> &MalVal;

    fn meta(&self) -> MalVal {
        (*self.get_meta()).clone()
    }

    fn with_meta(&self, val: &MalVal) -> T;
}

impl HasMeta<MalCoreFn> for MalCoreFn {
    fn get_meta(&self) -> &MalVal { &*self.meta_val }

    fn with_meta(&self, val: &MalVal) -> MalCoreFn {
        MalCoreFn::new(self.fun, val.clone())
    }
}

impl HasMeta<MalUserFn> for MalUserFn {
    fn get_meta(&self) -> &MalVal { &*self.meta_val }

    fn with_meta(&self, val: &MalVal) -> MalUserFn {
        MalUserFn::new(
            *self.binds.clone(),
            (*self.body).clone(),
            Rc::clone(&self.env),
            self.is_macro,
            self.eval,
            val.clone()
        )
    }
}

impl HasMeta<VecLike> for VecLike {
    fn get_meta(&self) -> &MalVal { &*self.meta_val }

    fn with_meta(&self, meta: &MalVal) -> VecLike {
        VecLike::new(*self.v.clone(), meta.clone())
    }
}

impl HasMeta<MapLike> for MapLike {
    fn get_meta(&self) -> &MalVal { &*self.meta_val }

    fn with_meta(&self, val: &MalVal) -> MapLike {
        MapLike::new(*self.v.clone(), val.clone())
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
    Map(MapLike),
    UserFun(MalUserFn),
    CoreFun(MalCoreFn),
    Atom(Rc<RefCell<MalVal>>),
}

impl MalVal {
    pub fn as_vec(&self) -> Result<Rc<Vec<MalVal>>, MalErr> {
        match self {
            MalVal::List(l) | MalVal::Vector(l) => Ok(Rc::new(*l.v.clone())),
            _ => errf!("Can't treat '{}' as list/vec", v_to_str!(self))
        }
    }
}

#[derive(Clone)]
pub struct MalErr(pub Rc<MalVal>);

impl MalErr {
    pub fn to_val(self) -> MalVal {
        let MalErr(r) = self;
        (*r).clone()
    }
}
