use std::rc::Rc;
use std::collections::HashMap;

use printer::pr_str;
use types::MalVal::{self, *};

type MalRet = Result<MalVal, String>;

macro_rules! err {
    ($e:expr) => { Err($e.to_string()) }
}

/*
macro_rules! core_fn {
    ($func_name:ident, $body:block) => {
        fn $func_name(args: &MalVal) -> MalRet
    }
}
 */

type MalFnSig = fn(&[MalVal]) -> MalRet;

fn add_to_core(ns: &mut HashMap<String, MalVal>, name: &str, fun: MalFnSig) {
    ns.insert(name.to_string(), CoreFun(fun));
}

fn _pr_str(args: &[MalVal]) -> String {
    args.iter().map(|v| { pr_str(v.clone(), true) })
        .collect::<Vec<String>>()
        .join(" ")
}

fn _str(args: &[MalVal], joiner: &str) -> String {
    args.iter().map(|v| { pr_str(v.clone(), false) })
        .collect::<Vec<String>>()
        .join(joiner)
}

fn is_equal(args: &[MalVal]) -> bool {
    mal_eq(&args[0], &args[1])
}

// Would be trivial if I could figure out how to implement PartialEq for
// the *Fun values.
fn mal_eq(this: &MalVal, that: &MalVal) -> bool {
    match (this, that) {
        (Int(a), Int(b)) => a == b,
        (Str(a), Str(b)) => a == b,
        (Sym(a), Sym(b)) => a == b,
        (Bool(a), Bool(b)) => a == b,
        (Nil, Nil) => true,
        (List(a), List(b)) => compare_lists(&a, &b),
        (Vector(a), Vector(b)) => compare_lists(&a, &b),
        // A little shabby but it'll do.
        (List(a), Vector(b)) => compare_lists(&a, &b),
        (Vector(a), List(b)) => compare_lists(&a, &b),
        (Map(a), Map(b)) => compare_maps(&a, &b),
        // (UserFun(_), UserFun(_)) => false, // TODO?
        // (CoreFun(_), CoreFun(_)) => false, // TODO?
         _ => false
    }
}

fn compare_lists(this: &Rc<Vec<MalVal>>, that: &Rc<Vec<MalVal>>) -> bool {
    this.len() == that.len() &&
        (0 .. this.len()).all(|idx| { mal_eq(&this[idx], &that[idx]) })
}

fn compare_maps(this: &Rc<HashMap<String, MalVal>>, that: &Rc<HashMap<String, MalVal>>) -> bool {
    this.len() == that.len() &&
        this.keys().all(|k| {
            that.contains_key(k) && mal_eq(this.get(k).unwrap(), that.get(k).unwrap())
        })
}

pub fn core_ns() -> HashMap<String, MalVal> {
    let mut ns = HashMap::new();

    let mut add = |name, fun| { add_to_core(&mut ns, name, fun) };

    add("+", |args| {
        match (&args[0], &args[1]) {
            (Int(a), Int(b)) => Ok(MalVal::Int(a + b)),
            _ => err!("Can only add two Ints!")
        }
    });

    add("-", |args| {
        match (&args[0], &args[1]) {
            (Int(a), Int(b)) => Ok(MalVal::Int(a - b)),
            _ => err!("Can only subtract two Ints!")
        }
    });

    add("*", |args| {
        match (&args[0], &args[1]) {
            (Int(a), Int(b)) => Ok(MalVal::Int(a * b)),
            _ => err!("Can only multiply two Ints!")
        }
    });

    add("/", |args| {
        match (&args[0], &args[1]) {
            (Int(a), Int(b)) => Ok(MalVal::Int(a / b)),
            _ => err!("Can only divide two Ints!")
        }
    });

    add("pr-str", |args| {
        Ok(Str(_pr_str(args)))
    });

    add("str", |args| {
        Ok(Str(_str(args, "")))
    });

    add("prn", |args| {
        println!("{}", _pr_str(args));
        Ok(Nil)
    });

    add("println", |args| {
        println!("{}", _str(args, " "));
        Ok(Nil)
    });

    add("list", |args| {
        Ok(List(Rc::new(args.to_vec())))
    });

    add("list?", |args| {
        Ok(Bool(matches!(args[0], List(_))))
    });

    add("empty?", |args| {
        Ok(Bool(matches!(&args[0], List(l) | Vector(l) if l.is_empty())))
    });

    add("count", |args| {
        match &args[0] {
            List(l) | Vector(l) => Ok(Int(l.len() as i64)),
            Nil => Ok(Int(0)),
            _ => err!("Can't count a non list/vec")
        }
    });

    add("=", |args| {
        Ok(Bool(is_equal(args)))
    });

    return ns
}
