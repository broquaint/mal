use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use types::MalVal::{self, List, Sym};

#[derive(Clone)]
pub struct MalEnv {
    pub outer: Option<Box<MalEnv>>,
    pub data:  Rc<RefCell<HashMap<String, MalVal>>>
}

impl MalEnv {
    pub fn make_inner(&self) -> MalEnv {
        MalEnv { outer: Some(Box::new(self.clone())), data: Rc::new(RefCell::new(HashMap::new())) }
    }

    pub fn make_inner_with(&self, binds: &Rc<Vec<MalVal>>, args: &[MalVal]) -> Result<MalEnv, String> {
        let mut params = HashMap::new();

        for idx in 0 .. binds.len() {
            match &binds[idx] {
                Sym(s) => {
                    if s == "&" {
                        if let Sym(rest) = &binds[idx + 1] {
                            let etc = &args[idx .. args.len()];
                            params.insert(rest.clone(), List(Rc::new(etc.to_vec())));
                            break;
                        }
                        else {
                            return Err("Got a non-sym after & in binds".to_string())
                        }
                    }
                    else {
                        params.insert(s.clone(), args[idx].clone());
                    }
                }
                _ => return Err("Got a non-sym in fn*".to_string())
            }
        }

        Ok(MalEnv { outer: Some(Box::new(self.clone())), data: Rc::new(RefCell::new(params)) })
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

    pub fn get(&self, k: &String) -> Result<MalVal, String> {
        if let Some(menv) = self.find(k) {
            Ok(menv.data.borrow().get(k).unwrap().clone())
        }
        else {
            Err(format!("'{}' not found", k))
        }
    }
}
