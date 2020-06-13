-module(step2_eval).

-include("types.hrl").

-export([main/1]).

main(_) ->
    ReplEnv = #{"+" => fun([A,B]) -> num(num(A) + num(B)) end,
                "-" => fun([A,B]) -> num(num(A) - num(B)) end,
                "*" => fun([A,B]) -> num(num(A) * num(B)) end,
                "/" => fun([A,B]) -> num(trunc(num(A) / num(B))) end},

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
                        _:Reason -> io:format("Runtime error: ~p~n", [Reason]),
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
eval(Ast, Env) ->
    {#mal_list{elems=[F|Args]}, NextEnv} = eval_ast(Ast, Env),
    {F(Args), NextEnv}.

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
    try map_get(Ast#mal_sym.val, Env) of V -> {V, Env}
    catch
        _:{badkey, K} -> throw({malerr, io_lib:format("Symbol ~s not in env", [K])})
    end;
eval_ast(Ast, Env) -> {Ast, Env}.
