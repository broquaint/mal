use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;

use types::MalVal::{self, *};
use types::MalErr;
use types::MalRet;
use types::VecLike;
use types::EvalSig;

use printer::pr_str;
use printer::malstr_as_string;

use ::as_mal_list;
use ::as_mal_err;
use ::errf;
use ::err;
use ::v_to_str;

#[derive(Clone)]
pub struct MalEnv {
    outer: Option<Rc<MalEnv>>,
    data:  Rc<RefCell<HashMap<String, MalVal>>>,
}

impl MalEnv {
    fn new(outer: Option<Rc<MalEnv>>, params: HashMap<String, MalVal>) -> Rc<MalEnv> {
        Rc::new(MalEnv {
            outer: outer,
            data:  Rc::new(RefCell::new(params)),
        })
    }

    pub fn new_root() -> Rc<MalEnv> {
        MalEnv::new(None, HashMap::new())
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

impl ToString for MalEnv {
    fn to_string(&self) -> String {
        format!("{{\n{}}}", self.data.borrow().iter()
                .filter(|(_,v)| !matches!(v, CoreFun(_)))
                .map(|(k,v)| format!("  {} {}\n", malstr_as_string(k.clone(), true), v_to_str!(v)))
                .collect::<Vec<String>>().join(" "))
    }
}

type MalEnvRet = Result<Rc<MalEnv>, MalErr>;

pub trait MakeRcEnv {
    fn bound_to(&self, binds: &Box<VecLike>, args: &[MalVal]) -> MalEnvRet;

    fn for_let(&self, binds: &VecLike, eval: EvalSig) -> MalEnvRet;
}

impl MakeRcEnv for Rc<MalEnv> {
    fn bound_to(&self, binds: &Box<VecLike>, args: &[MalVal]) -> MalEnvRet {
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

        Ok(MalEnv::new(Some(Rc::clone(self)), params))
    }

    fn for_let(&self, binds: &VecLike, eval: EvalSig) -> MalEnvRet {
        let new_env = MalEnv::new(Some(Rc::clone(self)), HashMap::new());

        if binds.len() % 2 != 0 {
            return err!("binds for let* wasn't even")
        }

        for bind in binds.chunks(2) {
            match &bind[0] {
                Sym(k) => {
                    let v = eval(Rc::new(bind[1].clone()), Rc::clone(&new_env))?;
                    new_env.set(k.clone(), v);
                }
                _ => return err!("bind in let* wasn't a symbol")
            }
        }

        Ok(new_env)
    }
}
