-module(step4_if_fn_do).

-include("types.hrl").

-export([main/1]).

main(_) ->
    env:start(core:ns()),
    repl().

repl() ->
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
                    try eval(Ast, env:current())
                             of Res -> io:format("~s~n", [print(Res)]),
                                       repl()
                    catch
                        _:{malerr, Err} -> io:format("Exception: ~s~n", [Err]),
                                         repl();
                        _:Reason:Stack -> io:format("Mal bug: ~p~n~p~n", [Reason, Stack]),
                                        repl()
                    end;
                {error, Err} ->
                    io:format("Reader error: ~s~n", [Err]),
                    repl()
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
%    io:format("ast is: ~p~n", [AstRec]),
%    io:format("env is: ~p~n", [Env]),
    case H of
        #mal_sym{val="def!"} ->
            [K, E] = Tail,
            V = eval(E, Env), % Bug! recursive fail?
            env:set(K, V),
            V;
        #mal_sym{val="let*"} ->
            [Binds, Body] = Tail,
            eval(Body, let_binds(Binds, Env));
        #mal_sym{val="do"} ->
            Do = fun (Expr, _) -> eval(Expr, Env) end,
            lists:foldl(Do, eval(H, Env), Tail);
        #mal_sym{val="if"} ->
            [Cond, Body|Else] = Tail,
            case cond_to_res(eval(Cond, Env)) of % Shouldn't need to propagate Env here.
                true  -> eval(Body, Env);
                false -> if length(Else) > 0 -> eval(hd(Else), Env);
                            true -> mal_nil end
            end;
        #mal_sym{val="fn*"} ->
            [Params, Body] = Tail,
            Fn = fun(Args) -> 
                         Binds = fn_binds(Params, Args, Env),
%                         io:format("binds are: ~p~n", [Binds]),
                         eval(Body, env:bound(Binds))
                 end,
            Fn;
        _ ->
            #mal_list{elems=[F|Args]} = eval_ast(AstRec, Env),
%            io:format("calling ~p with ~p~n", [F, Args]),
            F(Args)
    end.

fn_binds([], _, Env) -> Env;
fn_binds(#mal_list{elems=Params}, Args, Env) ->
    fn_binds(Params, Args, Env);
fn_binds(#mal_vec{elems=Params}, Args, Env) ->
    fn_binds(Params, Args, Env);
fn_binds([Param|Rest], [Arg|Args], Env) ->
    fn_binds(Rest, Args, env:with(Param, Arg, Env)).

cond_to_res(V) when V =:= mal_false -> false;
cond_to_res(V) when V =:= mal_nil -> false;
cond_to_res(_) -> true.

let_binds([], Env) -> Env;
let_binds(#mal_list{elems=Binds}, Env) -> let_binds(Binds, Env);
let_binds(#mal_vec{elems=Binds}, Env) -> let_binds(Binds, Env);
let_binds([Sym, Expr|Tail], Env) ->
    Val = eval(Expr, Env),
    let_binds(Tail, env:with(Sym, Val, Env)).

eval_list_elem(Ast, {List, Env}) -> {List ++ [eval(Ast, Env)], Env}.
eval_map_elem(K, Ast, {Pairs, Env}) -> {maps:put(K, eval(Ast, Env), Pairs), Env}.

eval_ast(Ast, Env) when is_record(Ast, mal_list) ->
    {Elems, _} = lists:foldl(fun eval_list_elem/2, {[], Env}, Ast#mal_list.elems),
    #mal_list{elems=Elems};
eval_ast(Ast, Env) when is_record(Ast, mal_vec) ->
    {Elems, _} = lists:foldl(fun eval_list_elem/2, {[], Env}, Ast#mal_vec.elems),
    #mal_vec{elems=Elems};
eval_ast(Ast, Env) when is_record(Ast, mal_map) ->
    {Pairs, _} = maps:fold(fun eval_map_elem/3, {#{}, Env}, Ast#mal_map.pairs),
    #mal_map{pairs=Pairs};
eval_ast(Ast, Env) when is_record(Ast, mal_sym) ->
    env:get(Ast, Env);
eval_ast(Ast, _) -> Ast.
