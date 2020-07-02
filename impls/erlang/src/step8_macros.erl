-module(step8_macros).

-include("types.hrl").

-export([main/1]).

main(Args) ->
    Env = env:new(core:ns()),
    rep("(def! not (fn* (a) (if a false true)))", Env),
    rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", Env),
    rep("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))", Env),
    Eval = fun([Ast]) -> eval(Ast, Env) end,
    env:set(sym("eval"), Eval, Env),

    ArgvSym = sym("*ARGV*"),
    if
        length(Args) > 0 ->
            ProgArgs = lists:map(fun(S) -> #mal_str{val=S} end, tl(Args)),
            env:set(ArgvSym, list(ProgArgs), Env),
            rep(io_lib:format("(load-file \x22~s\x22)", [hd(Args)]), Env);
        true ->
            Argv = lists:map(fun(S) -> #mal_str{val=S} end, Args),
            env:set(ArgvSym, list(Argv), Env),
            repl(Env)
    end.

rep(Code, Env) ->
    {success, Ast} = read(Code),
    print(eval(Ast, Env)).

repl(Env) ->
    case io:get_line(standard_io, "user> ") of
        eof ->
            io:format("~n"),
            ok;
        {error, ErrorDescription} ->
            io:format("Error reading input: ~p~n", [ErrorDescription]),
            exit(ioerr);
        Input ->
            case read(Input) of
                {success, Ast} ->
                    try eval(Ast, Env) of
                        Res -> io:format("~s~n", [print(Res)]),
                               repl(Env)
                    catch
                        _:{malerr, Err} -> io:format("Exception: ~s~n", [Err]),
                                           repl(Env);
                        E:Reason:Stack -> io:format("Mal bug: ~p~nReason: ~p~nStack trace: ~p~n", [E, Reason, Stack]),
                                          repl(Env)
                    end;
                {error, Err} ->
                    io:format("Reader error: ~s~n", [Err]),
                    repl(Env)
            end
    end.

read(Code) ->
    reader:read_str(Code).

print(Ast) ->
    printer:pr_str(Ast).

eval(Ast, Env) ->
    subval(macroexpand(Ast, Env), Env).
subval(Ast, Env) when not is_record(Ast, mal_list) -> eval_ast(Ast, Env);
subval(#mal_list{elems=[]}, _) -> #mal_list{elems=[]};
subval(TopAst, Env) ->
    AstRec = macroexpand(TopAst, Env),
    AstList = AstRec#mal_list.elems,
    [H|Tail] = AstList,
%    io:format("ast is: ~s~n", [printer:pr_str(AstRec)]),
%    io:format("env is: ~p~n", [Env]),
    case H of
        #mal_sym{val="def!"} ->
            [K, E] = Tail,
            V = eval(E, Env),
            env:set(K, V, Env),
            V;
        #mal_sym{val="defmacro!"} ->
            [K, E] = Tail,
            F = eval(E, Env),
            M = F#mal_fn{is_macro=true},
            env:set(K, M, Env),
            M;
        #mal_sym{val="let*"} ->
            [Binds, Body] = Tail,
            eval(Body, env:for_let(Binds, fun eval/2, Env));
        #mal_sym{val="do"} ->
            Do = fun (Expr, _) -> eval(Expr, Env) end,
            lists:foldl(Do, mal_nil, Tail);
        #mal_sym{val="if"} ->
            [Cond, Body|Else] = Tail,
            case cond_to_res(eval(Cond, Env)) of
                true  -> eval(Body, Env);
                false -> if length(Else) > 0 -> eval(hd(Else), Env);
                            true -> mal_nil end
            end;
        #mal_sym{val="macroexpand"} ->
            macroexpand(hd(Tail), Env);
        #mal_sym{val="quote"} ->
            hd(Tail);
        #mal_sym{val="quasiquote"} ->
            eval(quasiquote(hd(Tail)), Env);
        #mal_sym{val="fn*"} ->
            [Params, Body] = Tail,
            #mal_fn{ast=Body, params=Params, env=Env, eval=fun eval/2};
        _ ->
            #mal_list{elems=[F|Args]} = eval_ast(AstRec, Env),
            case F of
                #mal_fn{ast=Body, params=Params, env=CEnv} ->
                    eval(Body, env:for_fn(Params, Args, CEnv));
                _ ->
                    F(Args)
            end
    end.

eval_ast(#mal_list{elems=Elems}, Env) ->
    Acc = fun(A, L) -> L ++ [eval(A, Env)] end,
    list(lists:foldl(Acc, [], Elems));
eval_ast(#mal_vec{elems=Elems}, Env) ->
    Acc = fun(A, L) -> L ++ [eval(A, Env)] end,
    #mal_vec{elems=lists:foldl(Acc, [], Elems)};
eval_ast(#mal_map{pairs=Pairs}, Env) ->
    Acc = fun(K, V, M) -> maps:put(K, eval(V, Env), M) end,
    #mal_map{pairs=maps:fold(Acc, #{}, Pairs)};
eval_ast(Ast, Env) when is_record(Ast, mal_sym) ->
    env:get(Ast, Env);
eval_ast(Ast, _) -> Ast.

cond_to_res(V) when V =:= mal_false -> false;
cond_to_res(V) when V =:= mal_nil -> false;
cond_to_res(_) -> true.

list(L) -> #mal_list{elems=L}.

sym(S) -> #mal_sym{val=S}.

quasiquote({R, [H|T]}) when R == mal_list orelse R == mal_vec -> quasiquote([H|T]);
quasiquote([H|T]) ->
    case H of
        #mal_sym{val="unquote"} -> hd(T);
        {_, [#mal_sym{val="splice-unquote"}|R]} ->
            list([sym("concat"), hd(R), quasiquote(T)]);
        _ -> list([sym("cons"), quasiquote(H), quasiquote(T)])
    end;
quasiquote([]) -> list([sym("quote"), list([])]);
quasiquote(Ast) -> list([sym("quote"), Ast]).

macroexpand(Ast, Env)
  when is_record(hd(Ast#mal_list.elems), mal_sym) ->
    #mal_list{elems=[Sym|Args]} = Ast,
    try env:get(Sym, Env) of
        #mal_fn{ast=Body, params=Params, env=FnEnv, is_macro=true} ->
            Next = eval(Body, env:for_fn(Params, Args, FnEnv)),
            macroexpand(Next, Env);
        _ -> Ast
    catch _ -> Ast
    end;

macroexpand(Ast, _) -> Ast.
