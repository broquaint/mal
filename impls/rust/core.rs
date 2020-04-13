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

fn mal_add(args: &[MalVal]) -> MalRet {
    match (&args[0], &args[1]) {
        (Int(a), Int(b)) => Ok(MalVal::Int(a + b)),
        _ => err!("Can only add two Ints!")
    }
}


fn mal_sub(args: &[MalVal]) -> MalRet {
    match (&args[0], &args[1]) {
        (Int(a), Int(b)) => Ok(MalVal::Int(a - b)),
        _ => err!("Can only subtract two Ints!")
    }
}

fn mal_mul(args: &[MalVal]) -> MalRet {
    match (&args[0], &args[1]) {
        (Int(a), Int(b)) => Ok(MalVal::Int(a * b)),
        _ => err!("Can only multiply two Ints!")
    }
}

fn mal_div(args: &[MalVal]) -> MalRet {
    match (&args[0], &args[1]) {
        (Int(a), Int(b)) => Ok(MalVal::Int(a / b)),
        _ => err!("Can only divide two Ints!")
    }
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

fn mal_pr_str(args: &[MalVal]) -> MalRet {
    Ok(Str(_pr_str(args)))
}

fn mal_str(args: &[MalVal]) -> MalRet {
    Ok(Str(_str(args, "")))
}

fn prn(args: &[MalVal]) -> MalRet {
    println!("{}", _pr_str(args));
    Ok(Nil)
}

fn mal_println(args: &[MalVal]) -> MalRet {
    println!("{}", _str(args, " "));
    Ok(Nil)
}

pub fn core_ns() -> HashMap<String, MalVal> {
    let mut ns = HashMap::new();
    ns.insert("+".to_string(), CoreFun(mal_add));
    ns.insert("-".to_string(), CoreFun(mal_sub));
    ns.insert("*".to_string(), CoreFun(mal_mul));
    ns.insert("/".to_string(), CoreFun(mal_div));

    ns.insert("pr-str".to_string(), CoreFun(mal_pr_str));
    ns.insert("str".to_string(), CoreFun(mal_str));
    ns.insert("prn".to_string(), CoreFun(prn));
    ns.insert("println".to_string(), CoreFun(mal_println));
    return ns
}
