use std::fs;
use std::io::{self, Write};
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::time::SystemTime;

use env::MalEnv;
use reader::read_str;
use reader::make_keyword;
use reader::KW_PREFIX;
use printer::pr_str;
use printer::rs_pr_str;
use types::MalVal::{self, *};
use types::MalFnSig;
use types::MalUserFn;
use types::MalErr;
use types::MalRet;
use types::VecLike;

#[macro_export]
macro_rules! as_mal_err {
    ($e:expr) => { Err(MalErr(Rc::new($e))) }
}

#[macro_export]
macro_rules! errf {
    ($fmt:expr, $($e:expr),*) => {
        as_mal_err!(Str(format!($fmt, $($e),*)))
    }
}

#[macro_export]
macro_rules! err {
    ($e:expr) => { as_mal_err!(Str($e.to_string())) }
}

#[macro_export]
macro_rules! as_mal_list {
    ($e:expr) => { List(VecLike { v: Box::new($e), meta: Box::new(Nil) }) }
}

#[macro_export]
macro_rules! mal_list {
    ($($e:expr),*) => {
        List(VecLike { v: Box::new(vec![$($e),*]), meta: Box::new(Nil) })
    }
}

macro_rules! v_to_str {
    ($e:expr) => { pr_str($e.clone(), true) }
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

fn compare_lists(this: &VecLike, that: &VecLike) -> bool {
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
            _ => errf!("Can only operate on two ints, got: {} & {}", v_to_str!(&args[0]), v_to_str!(&args[1]))
        }
    }
    else {
        errf!("Expected 2 args, got {}", args.len())
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
        Err(e) => err!(e)
    }
}

pub fn make_bound_env(env: &Rc<MalEnv>, binds: &Box<VecLike>, args: &[MalVal]) -> Result<MalEnv, MalErr> {
    let mut params = HashMap::new();

    for idx in 0 .. binds.len() {
        match &binds.v[idx] {
            Sym(s) => {
                if s == "&" {
                    if let Sym(rest_bind) = &binds.v[idx + 1] {
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

    Ok(MalEnv { outer: Some(Rc::clone(env)), data: Rc::new(RefCell::new(params)), id: env.id * 2 })
}

pub fn call_user_fun(fun: &MalUserFn, args: &[MalVal]) -> MalRet {
    let inner_env = make_bound_env(&fun.env, &fun.binds, args)?;
    Ok((fun.eval)(Rc::clone(&fun.body), &Rc::new(inner_env))?)
}

pub fn call_fun(maybe_fun: &MalVal, args: &[MalVal]) -> MalRet {
    match maybe_fun {
        CoreFun(f) => f(args),
        UserFun(f) => call_user_fun(f, args),
        _ => errf!("can't treat this as a function: {}", v_to_str!(maybe_fun))
    }
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
        Ok(as_mal_list![args.to_vec()])
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
            Str(s) => read_str(s.clone()).map_err(|e| MalErr(Rc::new(Str(e)))),
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
            match &atfun[0] {
                Atom(a) => {
                    let mut fnargs = vec![a.borrow().clone()];
                    fnargs.extend_from_slice(params);

                    let new_val = call_fun(&atfun[1], fnargs.as_slice())?;
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
                    errf!("the index {} is out of bounds for {}", idx, v_to_str!(args[0]))
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
            _ => errf!("Can't call first on {}", v_to_str!(args[0]))
        }
    });

    add("rest", |args| {
        match &args[0] {
            List(l) | Vector(l) => {
                if l.is_empty() {
                    Ok(mal_list![])
                }
                else {
                    Ok(as_mal_list![l.v[1 ..].to_vec()])
                }
            }
            Nil => Ok(mal_list![]),
            _ => errf!("Can't call rest on {}", v_to_str!(args[0]))
        }
    });

    add("throw", |args| {
        as_mal_err!(args[0].clone())
    });

    add("apply", |args| {
        let(fun, rest) = args.split_first().unwrap();
        let(args_b, args_a) = rest.split_last().unwrap();
        let last_arg = args_b.as_vec()?;
        call_fun(&fun, &[&args_a[..], &last_arg[..]].concat())
    });

    add("map", |args| {
        let mut res = Vec::new();
        for v in args[1].as_vec()?.iter() {
            res.push(call_fun(&args[0], &[v.clone()])?);
        }
        Ok(as_mal_list![res])
    });

    add("nil?", |args| {
        Ok(Bool(matches!(&args[0], Nil)))
    });

    add("true?", |args| {
        Ok(Bool(matches!(&args[0], Bool(b) if *b)))
    });

    add("false?", |args| {
        Ok(Bool(matches!(&args[0], Bool(b) if !*b)))
    });

    add("symbol?", |args| {
        Ok(Bool(matches!(&args[0], Sym(_))))
    });

    add("symbol", |args| {
        match &args[0] {
            Str(s) => Ok(Sym(s.clone())),
            v => errf!("expected a string for (symbol) got: {}", v_to_str!(v))
        }
    });

    add("keyword", |args| {
        match &args[0] {
            Str(s) => Ok(Str(
                if s.starts_with(KW_PREFIX) { s.clone() } else { make_keyword(&s[..]) }
            )),
            v => errf!("expected a string for (symbol) got: {}", v_to_str!(v))
        }
    });

    add("keyword?", |args| {
        Ok(Bool(matches!(&args[0], Str(s) if s.starts_with(KW_PREFIX))))
    });

    add("vector", |args| {
        Ok(Vector(VecLike { v: Box::new(args.to_vec()), meta: Box::new(Nil) }))
    });

    add("vector?", |args| {
        Ok(Bool(matches!(&args[0], Vector(_))))
    });

    add("sequential?", |args| {
        Ok(Bool(matches!(&args[0], List(_) | Vector(_))))
    });

    add("hash-map", |args| {
        if args.len() % 2 != 0 {
            err!("hash-map received an odd number of elements")
        } else {
            let mut map : HashMap<String, MalVal> = HashMap::new();
            for pair in args.chunks(2) {
                match &pair[0] {
                    Str(s) => { map.insert(s.clone(), pair[1].clone()); }
                    _      => return errf!("map keys can only be string or keyword, got: {}", v_to_str!(&pair[0]))
                }
            }
            Ok(Map(Rc::new(map)))
        }
    });

    add("map?", |args| {
        Ok(Bool(matches!(&args[0], Map(_))))
    });

    add("assoc", |args| {
        let (src_map, rest) = args.split_first().unwrap();
        if rest.len() % 2 != 0 {
            errf!("assoc received an odd number of elements, got {} args", rest.len())
        } else if let Map(m) = src_map {
            let mut map : HashMap<String, MalVal> = (**m).clone();
            for pair in rest.chunks(2) {
                match &pair[0] {
                    Str(s) => { map.insert(s.clone(), pair[1].clone()); }
                    _      => return errf!("map keys can only be string or keyword, got: {}", v_to_str!(&pair[0]))
                }
            }
            Ok(Map(Rc::new(map)))
        } else {
            errf!("assoc expects map, got: {}", v_to_str!(src_map))
        }
    });

    add("dissoc", |args| {
        let (src_map, rest) = args.split_first().unwrap();
        if let Map(m) = src_map {
            let mut map : HashMap<String, MalVal> = (**m).clone();
            for k in rest {
                match &k {
                    Str(s) => { map.remove(s); }
                    _      => return errf!("map keys can only be string or keyword, got: {}", v_to_str!(k))
                }
            }
            Ok(Map(Rc::new(map)))
        } else {
            errf!("dissoc expects map, got: {}", v_to_str!(src_map))
        }
    });

    add("get", |args| {
        match (&args[0], &args[1]) {
            (Map(m), Str(s)) => Ok(m.get(s).map_or(Nil, |v| v.clone())),
            (Nil, _) => Ok(Nil),
            _ => errf!("get expects map & string/keyword, got: {}, {}", v_to_str!(&args[0]), v_to_str!(&args[1]))
        }
    });

    add("contains?", |args| {
        match (&args[0], &args[1]) {
            (Map(m), Str(s)) => Ok(Bool(m.contains_key(s))),
            _ => errf!("contains? expects map & string/keyword, got: {}, {}", v_to_str!(&args[0]), v_to_str!(&args[1]))
        }
    });

    add("keys", |args| {
        if let Map(m) = &args[0] {
            Ok(as_mal_list![m.keys().map(|k| Str(k.clone())).collect()])
        } else {
            errf!("keys expects map, got: {}", v_to_str!(&args[0]))
        }
    });

    add("vals", |args| {
        if let Map(m) = &args[0] {
            Ok(as_mal_list![m.values().map(|v| v.clone()).collect()])
        } else {
            errf!("vals expects map, got: {}", v_to_str!(&args[0]))
        }
    });

    add("readline", |args| {
        if let Str(s) = &args[0] {
            print!("{}", s);
            io::stdout().flush().unwrap();
        } else {
            return errf!("readline expects a string, got: {}", v_to_str!(&args[0]))
        }

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_)  => {
                if input.len() > 0 {
                    Ok(Str(input.trim_end().to_string()))
                } else {
                    Ok(Nil)
                }
            }
            Err(_) => err!("failed to read stdin?")
        }
    });

    add("time-ms", |_| {
        SystemTime::now().duration_since(SystemTime::UNIX_EPOCH)
            .map_or_else(|_| err!("now is before the unix epoch!?"),
                         |n| Ok(Int(n.as_millis() as i64)))
    });

    add("meta", |_args| {
        Ok(Nil)
    });

    add("with-meta", |_args| {
        Ok(Nil)
    });

    add("fn?", |args| {
        Ok(Bool(matches!(&args[0], CoreFun(_) | UserFun(_))))
    });

    add("string?", |args| {
        Ok(Bool(matches!(&args[0], Str(_))))
    });

    add("number?", |args| {
        Ok(Bool(matches!(&args[0], Int(_))))
    });

    add("conj", |args| {
        match &args[0] {
            List(_l) => Ok(Nil),
            Vector(_l) => Ok(Nil),
            _ => errf!("conj expects list/vector, got: {}", v_to_str!(&args[0]))
        }
    });

    add("seq", |args| {
        match &args[0] {
            List(_l) => Ok(Nil),
            Vector(_l) => Ok(Nil),
            Str(_s) => Ok(Nil),
            _ => Ok(Nil),
        }
    });

    return ns
}
