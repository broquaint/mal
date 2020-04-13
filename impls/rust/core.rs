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

    return ns
}
