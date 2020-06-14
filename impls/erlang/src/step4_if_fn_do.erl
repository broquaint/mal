-module(step4_if_fn_do).

-include("types.hrl").

-export([main/1]).

main(_) ->
    StrToSym = fun (K, F, Env) -> maps:put(#mal_sym{val=K}, F, Env) end,
    ReplEnv = env:new(
                maps:fold(StrToSym, #{},
                          #{"+" => fun([A,B]) -> num(num(A) + num(B)) end,
                            "-" => fun([A,B]) -> num(num(A) - num(B)) end,
                            "*" => fun([A,B]) -> num(num(A) * num(B)) end,
                            "/" => fun([A,B]) -> num(trunc(num(A) / num(B))) end})),
    repl(ReplEnv).

num(#mal_num{val=V}) -> V;
num(V) when is_integer(V) -> #mal_num{val=V}.

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
                        _:Reason:Stack -> io:format("Runtime error: ~p~n~p~n", [Reason, Stack]),
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
eval(AstRec, Env) ->
    AstList = AstRec#mal_list.elems,
    [H|Tail] = AstList,
    % Assuming it's a symbol because otherwise it's an error.
    case H#mal_sym.val of
        "def!" -> 
            [K, E] = Tail,
            {V, NextEnv} = eval(E, Env),
            {V, env:set(K, V, NextEnv)};
        "let*" ->
            [Binds, Body] = Tail,
            {Res, _} = eval(Body, make_binds(Binds, Env)),
            {Res, Env};
        _ -> {#mal_list{elems=[F|Args]}, NextEnv} = eval_ast(AstRec, Env),
             {F(Args), NextEnv}
    end.

make_binds([], Env) -> Env;
make_binds(#mal_list{elems=Binds}, Env) -> make_binds(Binds, Env);
make_binds(#mal_vec{elems=Binds}, Env) -> make_binds(Binds, Env);
make_binds([Sym, Expr|Tail], Env) ->
    {Val, NextEnv} = eval(Expr, Env),
    make_binds(Tail, env:set(Sym, Val, NextEnv)).

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
