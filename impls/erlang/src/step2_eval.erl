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
                    {Res, NextEnv} = eval(Ast, Env),
                    io:format("~s~n", [print(Res)]),
                    repl(NextEnv);
                {error, Err} ->
                    io:format("Error: ~s~n", [Err]),
                    repl(Env)
            end
        end.

read(Code) ->
    reader:read_str(Code).

print(Ast) ->
    printer:pr_str(Ast).

eval([], Env) -> {[], Env};
eval(Ast, Env) when not is_record(Ast, mal_list) -> eval_ast(Ast, Env);
eval(Ast, Env) ->
    {#mal_list{elems=[F|Args]}, NextEnv} = eval_ast(Ast, Env),
%    io:format("> applying ~p with ~p~n", [F, Args]),
    {F(Args), NextEnv}.

eval_list_elem(Ast, {List, Env}) ->
    {Res, NextEnv} = eval(Ast, Env),
    {List ++ [Res], NextEnv}.

eval_ast(Ast, Env) when is_record(Ast, mal_list) ->
    {E, NextEnv} = lists:foldl(fun eval_list_elem/2, {[], Env}, Ast#mal_list.elems),
    {#mal_list{elems=E}, NextEnv};
eval_ast(Ast, Env) when is_record(Ast, mal_sym) -> {map_get(Ast#mal_sym.val, Env), Env};
eval_ast(Ast, Env) -> {Ast, Env}.
