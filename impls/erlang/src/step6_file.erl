-module(step6_file).

-include("types.hrl").

-export([main/1]).

main(Args) ->
    Env = env:new(core:ns()),
    rep("(def! not (fn* (a) (if a false true)))", Env),
    rep("(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))", Env),
    Eval = fun([Ast]) -> eval(Ast, Env) end,
    env:set(#mal_sym{val="eval"}, Eval, Env),

    ArgvSym = #mal_sym{val="*ARGV*"},
    if
        length(Args) > 0 ->
            ProgArgs = lists:map(fun(S) -> #mal_str{val=S} end, tl(Args)),
            env:set(ArgvSym, #mal_list{elems=ProgArgs}, Env),
            rep(io_lib:format("(load-file \x22~s\x22)", [hd(Args)]), Env);
        true ->
            Argv = lists:map(fun(S) -> #mal_str{val=S} end, Args),
            env:set(ArgvSym, #mal_list{elems=Argv}, Env),
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

eval(Ast, Env) when not is_record(Ast, mal_list) -> eval_ast(Ast, Env);
eval(#mal_list{elems=[]}, _) -> #mal_list{elems=[]};
eval(AstRec, Env) ->
    AstList = AstRec#mal_list.elems,
    [H|Tail] = AstList,
%    io:format("ast is: ~p~n", [printer:pr_str(AstRec)]),
%    io:format("env is: ~p~n", [Env]),
    case H of
        #mal_sym{val="def!"} ->
            [K, E] = Tail,
            V = eval(E, Env),
            env:set(K, V, Env),
            V;
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

cond_to_res(V) when V =:= mal_false -> false;
cond_to_res(V) when V =:= mal_nil -> false;
cond_to_res(_) -> true.

eval_ast(#mal_list{elems=Elems}, Env) ->
    Acc = fun(A, L) -> L ++ [eval(A, Env)] end,
    #mal_list{elems=lists:foldl(Acc, [], Elems)};
eval_ast(#mal_vec{elems=Elems}, Env) ->
    Acc = fun(A, L) -> L ++ [eval(A, Env)] end,
    #mal_vec{elems=lists:foldl(Acc, [], Elems)};
eval_ast(#mal_map{pairs=Pairs}, Env) ->
    Acc = fun(K, V, M) -> maps:put(K, eval(V, Env), M) end,
    #mal_map{pairs=maps:fold(Acc, #{}, Pairs)};
eval_ast(Ast, Env) when is_record(Ast, mal_sym) ->
    env:get(Ast, Env);
eval_ast(Ast, _) -> Ast.
