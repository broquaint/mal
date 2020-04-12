use std::collections::HashMap;

use types::MalVal;

pub struct MalEnv {
    pub outer: Option<Box<MalEnv>>,
    pub data:  HashMap<String, MalVal>
}

impl MalEnv {
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
