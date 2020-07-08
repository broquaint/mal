-module(env).

-include("types.hrl").

-export([for_let/3, for_fn/3, set/3, get/2, for/1, run/1, new/1]).

% Internal helpers with direct access to mal_env records.

e_find(K, Env) when Env#mal_env.outer =:= root ->
    case maps:find(K, Env#mal_env.data) of
        {ok, _} -> {ok, Env};
        error -> error
    end;
e_find(K, Env) ->
    case maps:find(K, Env#mal_env.data) of
        {ok, _} -> {ok, Env};
        error -> e_find(K, env:for(Env#mal_env.outer))
    end.

e_get(K, Env) ->
    case e_find(K, Env) of
        {ok, ResEnv} -> {ok, maps:get(K, ResEnv#mal_env.data)};
        error -> {malerr, io_lib:format("'~s' not found", [K#mal_sym.val])}
    end.

e_set(K, V, Env) ->
    Env#mal_env{data=maps:put(K, V, Env#mal_env.data)}.

% Binding helpers
for_let(Binds, Eval, Env) ->
    let_binds(Binds, Eval, new(Env)).
let_binds([], _, Env) -> Env;
let_binds(#mal_list{elems=Binds}, Eval, Env) -> let_binds(Binds, Eval, Env);
let_binds(#mal_vec{elems=Binds}, Eval, Env) -> let_binds(Binds, Eval, Env);
let_binds([Sym, Expr|Tail], Eval, Env) ->
    env:set(Sym, Eval(Expr, Env), Env),
    let_binds(Tail, Eval, Env).

for_fn(Params, Args, Env) ->
    fn_binds(Params, Args, new(Env)).
fn_binds([], _, Env) -> Env;
fn_binds({_, Params}, Args, Env) -> fn_binds(Params, Args, Env);
fn_binds([#mal_sym{val="&"}, Rest], Args, Env) ->
    env:set(Rest, #mal_list{elems=Args}, Env),
    Env;
fn_binds([Param|Rest], [Arg|Args], Env) ->
    env:set(Param, Arg, Env),
    fn_binds(Rest, Args, Env).

% Module interface for managing a stateful mal_env via a pid.

run(Env) ->
    receive
        {From, {get, K}} ->
%            io:format("Looking for ~p in ~p~n", [K, Env]),
            From ! e_get(K, Env),
            run(Env);
        {From, {for}} ->
            From ! {ok, Env},
            run(Env);
        {From, {set, K, V}} ->
            Res = e_set(K, V, Env),
            From ! ok,
            run(Res);
        E ->
            io:format("env:run fail: [~p]~n", [E]),
            throw(mal_env_err)
    end.

set(K, V, Pid) ->
    Pid ! {self(), {set, K, V}},
    receive
        ok -> set_ok
    after 100 ->
        throw(env_set_timeout)
    end.

get(K, Pid) ->
%    io:format("Getting ~p in ~p~n", [K, Pid]),
    Pid ! {self(), {get, K}},
    receive
        {ok, V} -> V;
        {malerr, E} -> throw({malerr, E})
    after 100 ->
        throw(env_get_timeout)
    end.

for(Pid) ->
%    io:format("current env is ~p~n", [Pid]),
    Pid ! {self(), {for}},
    receive
        {ok, Env} -> Env
    after 100 ->
        throw(env_for_timeout)
    end.

new(M) when is_map(M) ->
    new(root, M);
new(M) ->
    new(M, #{}).
new(Env, M) ->
    spawn(?MODULE, run, [#mal_env{outer=Env, data=M}]).
