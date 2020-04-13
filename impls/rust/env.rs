use std::rc::Rc;
use std::collections::HashMap;

use types::MalVal::{self, Sym};

#[derive(Clone)]
pub struct MalEnv {
    pub outer: Option<Box<MalEnv>>,
    pub data:  HashMap<String, MalVal>
}

impl MalEnv {
    pub fn make_inner(&self) -> MalEnv {
        MalEnv { outer: Some(Box::new(self.clone())), data: HashMap::new() }
    }

    pub fn make_inner_with(&self, binds: &Rc<Vec<MalVal>>, args: &[MalVal]) -> Result<MalEnv, String> {
        let mut params = HashMap::new();

        for idx in 0 .. binds.len() {
            match &binds[idx] {
                // TODO Result!
                Sym(s) => { params.insert(s.clone(), args[idx].clone()); }
                _ => return Err("Got a non-sym in fn*".to_string())
            }
        }

        Ok(MalEnv { outer: Some(Box::new(self.clone())), data: params })
    }

    pub fn set(&mut self, k: String, v: MalVal) {
        self.data.insert(k, v.clone());
    }

    pub fn find(&self, k: &String) -> Option<&MalEnv> {
        if self.data.contains_key(k) {
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
            Ok(menv.data.get(k).unwrap().clone())
        }
        else {
            Err(format!("'{}' not found", k))
        }
    }
}
