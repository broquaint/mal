use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use types::MalVal;

#[derive(Clone)]
pub struct MalEnv {
    pub outer: Option<Rc<MalEnv>>,
    pub data:  Rc<RefCell<HashMap<String, MalVal>>>,
    pub id:    i32
}

impl MalEnv {
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

impl ToString for MalEnv {
    fn to_string(&self) -> String {
        let oenv = match &self.outer {
            Some(oe) => oe.id.to_string(),
            None     => "root".to_string(),
        };
        format!("MalEnv<id: {}, outer_env: {}>", self.id, oenv)
    }
}
