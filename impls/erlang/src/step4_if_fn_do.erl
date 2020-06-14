-module(step4_if_fn_do).

-include("types.hrl").

-export([main/1]).

main(_) ->
    ReplEnv = env:new(core:ns()),
    repl(ReplEnv).


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
                    try eval(Ast, Env)
                             of {Res, NextEnv} -> io:format("~s~n", [print(Res)]),
                                                  repl(NextEnv)
                    catch
                        _:{malerr, Err} -> io:format("Exception: ~s~n", [Err]),
                                         repl(Env);
                        _:Reason:Stack -> io:format("Mal bug: ~p~n~p~n", [Reason, Stack]),
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
eval(#mal_list{elems=[]}, Env) -> {#mal_list{elems=[]}, Env};
eval(AstRec, CurEnv) ->
    AstList = AstRec#mal_list.elems,
    [H|Tail] = AstList,
    case H of
        #mal_sym{val="def!"} ->
            [K, E] = Tail,
            {V, Env} = eval(E, CurEnv),
            {V, env:set(K, V, Env)};
        #mal_sym{val="let*"} ->
            [Binds, Body] = Tail,
            {Res, _} = eval(Body, let_binds(Binds, CurEnv)),
            {Res, CurEnv};
        #mal_sym{val="do"} ->
            Do = fun (Expr, {_ , Env}) -> eval(Expr, Env) end,
            lists:foldl(Do, {H, CurEnv}, AstList);
        #mal_sym{val="if"} ->
            [Cond, Body|Else] = Tail,
            case cond_to_res(eval(Cond, CurEnv)) of % Shouldn't need to propagate Env here.
                true  -> eval(Body, CurEnv);
                false -> if length(Else) > 0 -> eval(hd(Else), CurEnv);
                            true -> {mal_nil, CurEnv} end
            end;
        #mal_sym{val="fn*"} ->
            [Params, Body] = Tail,
            Fn = fun(Args) -> element(1, eval(Body, fn_binds(Params, Args, CurEnv))) end,
            {Fn, CurEnv};
        _ ->
            {#mal_list{elems=[F|Args]}, Env} = eval_ast(AstRec, CurEnv),
            {F(Args), Env}
    end.

fn_binds([], _, Env) -> Env;
fn_binds(#mal_list{elems=Params}, Args, Env) ->
    fn_binds(Params, Args, Env);
fn_binds(#mal_vec{elems=Params}, Args, Env) ->
    fn_binds(Params, Args, Env);
fn_binds([Param|Rest], [Arg|Args], Env) ->
    fn_binds(Rest, Args, env:set(Param, Arg, Env)).

cond_to_res({V, _}) when V =:= mal_false -> false;
cond_to_res({V, _}) when V =:= mal_nil -> false;
cond_to_res(_) -> true.

let_binds([], Env) -> Env;
let_binds(#mal_list{elems=Binds}, Env) -> let_binds(Binds, Env);
let_binds(#mal_vec{elems=Binds}, Env) -> let_binds(Binds, Env);
let_binds([Sym, Expr|Tail], Env) ->
    {Val, NextEnv} = eval(Expr, Env),
    let_binds(Tail, env:set(Sym, Val, NextEnv)).

eval_list_elem(Ast, {List, Env}) ->
    {Res, NextEnv} = eval(Ast, Env),
    {List ++ [Res], NextEnv}.
eval_map_elem(K, Ast, {Pairs, Env}) ->
    {Res, NextEnv} = eval(Ast, Env),
    {maps:put(K, Res, Pairs), NextEnv}.

eval_ast(Ast, Env) when is_record(Ast, mal_list) ->
    {E, NextEnv} = lists:foldl(fun eval_list_elem/2, {[], Env}, Ast#mal_list.elems),
    {#mal_list{elems=E}, NextEnv};
eval_ast(Ast, Env) when is_record(Ast, mal_vec) ->
    {E, NextEnv} = lists:foldl(fun eval_list_elem/2, {[], Env}, Ast#mal_vec.elems),
    {#mal_vec{elems=E}, NextEnv};
eval_ast(Ast, Env) when is_record(Ast, mal_map) ->
    {P, NextEnv} = maps:fold(fun eval_map_elem/3, {#{}, Env}, Ast#mal_map.pairs),
    {#mal_map{pairs=P}, NextEnv};
eval_ast(Ast, Env) when is_record(Ast, mal_sym) ->
    {env:get(Ast, Env), Env};
eval_ast(Ast, Env) -> {Ast, Env}.
