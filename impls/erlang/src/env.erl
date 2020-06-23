-module(env).

-include("types.hrl").

-export([set/2, get/1, get/2, current/0, with/3, bound/1, run/1, start/1]).

e_find(K, Env) when Env#mal_env.outer =:= root ->
    case maps:find(K, Env#mal_env.data) of
        {ok, _} -> {ok, Env};
        error -> error
    end;
e_find(K, Env) ->
    case maps:find(K, Env#mal_env.data) of
        {ok, _} -> {ok, Env};
        error -> e_find(K, Env#mal_env.outer)
    end.

e_get(K, Env) ->
    case e_find(K, Env) of
        {ok, ResEnv} -> {ok, maps:get(K, ResEnv#mal_env.data)};
        error -> {malerr, io_lib:format("Symbol '~s' not found in env", [K#mal_sym.val])}
    end.

with(K, V, Env) ->
    #mal_env{outer=Env#mal_env.outer, data=maps:put(K, V, Env#mal_env.data)}.

bound(Env) ->
    #mal_env{outer=current(), data=Env#mal_env.data}.

run(Env) ->
    receive
        {From, {set, K, V}} ->
            Res = with(K, V, Env),
            From ! ok,
            run(Res);
        {From, {get, K}} ->
%            io:format("Looking for ~p in ~p~n", [K, Env]),
            From ! e_get(K, Env),
            run(Env);
        {From, {current}} ->
            From ! {ok, Env},
            run(Env);
        E ->
            io:format("env:run fail: [~p]~n", [E]),
            throw(mal_env_err)
    end.

set(K, V) ->
    Pid = whereis(mal_env),
    Pid ! {self(), {set, K, V}},
    receive
        ok -> ok
    after 100 ->
        throw(env_set_timeout)
    end.

get(K) ->
    Pid = whereis(mal_env),
%    io:format("Getting ~p in ~p~n", [K, Pid]),
    Pid ! {self(), {get, K}},
    receive
        {ok, V} -> V;
        {malerr, E} -> throw({malerr, E})
    after 100 ->
        throw(env_get_timeout)
    end.

get(K, Env) ->
    case e_get(K, Env) of
        {ok, V} -> V;
        {malerr, E} -> throw({malerr, E})
    end.

current() ->
    Pid = whereis(mal_env),
%    io:format("current env is ~p~n", [Pid]),
    Pid ! {self(), {current}},
    receive
        {ok, Env} -> Env
    after 100 ->
        throw(env_current_timeout)
    end.


% new() -> new(#{}).
start(M) ->
    Pid = spawn(?MODULE, run, [#mal_env{outer=root, data=M}]),
    register(mal_env, Pid).
