use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;

use types::MalVal::{self, *};
use types::MalErr;
use types::MalRet;
use types::VecLike;

use ::as_mal_list;
use ::as_mal_err;
use ::errf;
use ::err;

#[derive(Clone)]
pub struct MalEnv {
    outer: Option<Rc<MalEnv>>,
    data:  Rc<RefCell<HashMap<String, MalVal>>>,
//    pub id:    i32
}

impl MalEnv {
    pub fn make(outer: Option<Rc<MalEnv>>, params: HashMap<String, MalVal>) -> Rc<MalEnv> {
        Rc::new(MalEnv {
            outer: outer,
            data:  Rc::new(RefCell::new(params)),
        })
    }

    pub fn make_root() -> Rc<MalEnv> {
        MalEnv::make(None, HashMap::new())
    }

    pub fn make_inner(outer_env: &Rc<MalEnv>) -> Rc<MalEnv> {
        MalEnv::make(Some(Rc::clone(outer_env)), HashMap::new())
    }

    pub fn make_bound(outer_env: &Rc<MalEnv>, params: HashMap<String, MalVal>) -> Rc<MalEnv> {
        MalEnv::make(Some(Rc::clone(outer_env)), params)
    }

    pub fn make_bound_env(env: &Rc<MalEnv>, binds: &Box<VecLike>, args: &[MalVal]) -> Result<Rc<MalEnv>, MalErr> {
        let mut params = HashMap::new();
    
        for idx in 0 .. binds.len() {
            match &binds[idx] {
                Sym(s) => {
                    if s == "&" {
                        if let Sym(rest_bind) = &binds[idx + 1] {
                            let rest_args = &args[idx .. args.len()];
                            params.insert(rest_bind.clone(), as_mal_list!(rest_args.to_vec()));
                            break;
                        }
                        else {
                            return err!("Got a non-sym after & in binds")
                        }
                    }
                    else {
                        if idx < args.len() {
                            params.insert(s.clone(), args[idx].clone());
                        }
                        else {
                            return errf!("have {} binds but got {} args", binds.len(), args.len());
                        }
                    }
                }
                _ => return err!("Got a non-sym in fn*")
            }
        }
    
        Ok(MalEnv::make_bound(env, params))
    }

    pub fn set(&self, k: String, v: MalVal) {
        self.data.deref().borrow_mut().insert(k, v.clone());
    }

    pub fn find(&self, k: &String) -> Option<&MalEnv> {
        if self.data.borrow().contains_key(k) {
            Some(self)
        }
        else if let Some(outer_env) = &self.outer {
            outer_env.find(k)
        }
        else {
            None
        }
    }

    pub fn root(&self) -> Rc<MalEnv> {
        if let Some(outer_env) = &self.outer {
            outer_env.root()
        }
        else {
            // Cloning isn't ideal but it works and that's enough for now.
            Rc::new(self.clone())
        }
    }

    pub fn get(&self, k: &String) -> MalRet {
        if let Some(menv) = self.find(k) {
            Ok(menv.data.borrow().get(k).unwrap().clone())
        }
        else {
            // errf! would be nicer but failing at macro use/export.
            errf!("'{}' not found", k)
        }
    }
}

