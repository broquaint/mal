use std::fs;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use env::MalEnv;
use reader::read_str;
use printer::pr_str;
use printer::rs_pr_str;
use types::MalVal::{self, *};
use types::MalFnSig;
use types::MalUserFn;

pub type MalRet = Result<MalVal, String>;

#[macro_export]
macro_rules! err {
    ($e:expr) => { Err($e.to_string()) }
}

#[macro_export]
macro_rules! as_mal_list {
    ($e:expr) => { List(Rc::new($e)) }
}

#[macro_export]
macro_rules! mal_list {
    ($($e:expr),*) => { List(Rc::new(vec![$($e),*])) }
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

fn int_op(args: &[MalVal], fun: fn(i64, i64) -> i64) -> MalRet {
    if args.len() == 2 {
        match (&args[0], &args[1]) {
            (Int(a), Int(b)) => Ok(Int(fun(*a, *b))),
            _ => err!("Can only operate on two ints")
        }
    }
    else {
        Err(format!("Expected 2 args, got {}", args.len()))
    }
}

fn cmp_op(args: &[MalVal], fun: fn(i64, i64) -> bool) -> MalRet {
    match (&args[0], &args[1]) {
        (Int(a), Int(b)) => Ok(Bool(fun(*a, *b))),
        _ => err!("Can only compare two ints")
    }
}

fn read_file(path: &String) -> MalRet {
    let res = fs::read(path.as_str());
    match res {
        Ok(data) => Ok(Str(String::from_utf8_lossy(&data).to_string())),
        Err(e) => Err(e.to_string())
    }
}

pub fn make_bound_env(env: &Rc<MalEnv>, binds: &Rc<Vec<MalVal>>, args: &[MalVal]) -> Result<MalEnv, String> {
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
                        return Err(format!("have {} binds but got {} args", binds.len(), args.len()));
                    }
                }
            }
            _ => return err!("Got a non-sym in fn*")
        }
    }

    Ok(MalEnv { outer: Some(Rc::clone(env)), data: Rc::new(RefCell::new(params)), id: env.id * 2 })
}

pub fn call_user_fun(fun: &MalUserFn, args: &[MalVal]) -> MalRet {
    let inner_env = make_bound_env(&fun.env, &fun.binds, args)?;
    Ok((fun.eval)(Rc::clone(&fun.body), &Rc::new(inner_env))?)
}

fn add_to_core(ns: &mut HashMap<String, MalVal>, name: &str, fun: MalFnSig) {
    ns.insert(name.to_string(), CoreFun(fun));
}

pub fn core_ns() -> HashMap<String, MalVal> {
    let mut ns = HashMap::new();

    let mut add = |name, fun| { add_to_core(&mut ns, name, fun) };

    add("+", |args| { int_op(args, |a,b| { a + b }) });
    add("-", |args| { int_op(args, |a,b| { a - b }) });
    add("*", |args| { int_op(args, |a,b| { a * b }) });
    add("/", |args| { int_op(args, |a,b| { a / b }) });

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

    add("dbg", |args| {
        Ok(Str(rs_pr_str(args[0].clone())))
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

    add("<",  |args| { cmp_op(args, |a,b| { a <  b }) });
    add("<=", |args| { cmp_op(args, |a,b| { a <= b }) });
    add(">",  |args| { cmp_op(args, |a,b| { a >  b }) });
    add(">=", |args| { cmp_op(args, |a,b| { a >= b }) });

    add("read-string", |args| {
        match &args[0] {
            Str(s) => read_str(s.clone()),
            _ => err!("Can't read-str a non-string")
        }
    });

    add("slurp", |args| {
        match &args[0] {
            Str(s) => read_file(s),
            _ => err!("Can't slurp a non-str")
        }
    });

    add("atom", |args| {
        Ok(Atom(Rc::new(RefCell::new(args[0].clone()))))
    });
    add("atom?", |args| {
        Ok(Bool(matches!(args[0], Atom(_))))
    });
    add("deref", |args| {
        match &args[0] {
            Atom(a) => Ok(a.borrow().clone()),
            _ => err!("Can't deref a non-atom")
        }
    });
    add("reset!", |args| {
        match &args[0] {
            Atom(a) => {
                a.replace(args[1].clone());
                Ok(args[1].clone())
            }
            _ => err!("Can't reset! a non-atom")
        }
    });
    add("swap!", |args| {
        if args.len() < 2 {
            err!("Not enough args for swap!, need at least 2")
        }
        else {
            // ([atom, fn], [&args]) = ...
            let(atfun, params) = args.split_at(2);
            match (&atfun[0], &atfun[1]) {
                (Atom(a), UserFun(f)) => {
                    let mut fnargs = vec![a.borrow().clone()];
                    fnargs.extend_from_slice(params);

                    let new_val = call_user_fun(f, fnargs.as_slice())?;
                    a.replace(new_val.clone());
                    Ok(new_val)
                }
                (Atom(a), CoreFun(f)) => {
                    let mut fnargs = vec![a.borrow().clone()];
                    fnargs.extend_from_slice(params);

                    let new_val = f(fnargs.as_slice())?;
                    a.replace(new_val.clone());
                    Ok(new_val)
                }

                _ => err!("Can't swap! a non-atom")
            }
        }
    });

    add("cons", |args| {
        let mut newlist = vec![args[0].clone()];
        match &args[1] {
            List(l) | Vector(l) => {
                newlist.extend_from_slice(l.as_slice());
                Ok(as_mal_list![newlist])
            }
            _ => err!("second arg of cons must be list/vec")
        }
    });

    add("concat", |args| {
        let mut newlist = vec![];
        for larg in args {
            match larg {
                List(l) | Vector(l) => { newlist.extend_from_slice(l.as_slice()) }
                _ => return err!("can only concat list/vec")
            }
        }
        Ok(as_mal_list![newlist])
    });

    // TODO Handle args being empty, otherwise we panic on args[0]
    add("nth", |args| {
        match &args[0] {
            List(l) | Vector(l) => {
                let n = if let Int(v) = &args[1] { v }
                        else { return err!("second arg to nth must be a number") };
                let idx = *n as usize;
                if idx < l.len() {
                    Ok(l[idx].clone())
                }
                else {
                    Err(format!("the index {} is out of bounds for {}", idx, pr_str(args[0].clone(), true)))
                }
            }
            _ => err!("can only call nth on list/vec")
        }
    });

    add("first", |args| {
        match &args[0] {
            List(l) | Vector(l) => {
                if l.is_empty() { Ok(Nil) } else { Ok(l[0].clone()) }
            }
            Nil => Ok(Nil),
            _ => Err(format!("Can't call first on {}", pr_str(args[0].clone(), true)))
        }
    });

    add("rest", |args| {
        match &args[0] {
            List(l) | Vector(l) => {
                if l.is_empty() {
                    Ok(mal_list![])
                }
                else {
                    Ok(as_mal_list![l[1 ..].to_vec()])
                }
            }
            Nil => Ok(mal_list![]),
            _ => Err(format!("Can't call rest on {}", pr_str(args[0].clone(), true)))
        }
    });

    return ns
}
