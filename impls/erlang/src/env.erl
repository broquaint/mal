-module(env).

-include("types.hrl").

-export([set/3, find/2, get/2, new/1]).

set(K, V, Env) ->
    #mal_env{outer=Env#mal_env.outer, data=maps:put(K, V, Env#mal_env.data)}.

find(K, Env) when Env#mal_env.outer =:= root ->
    case maps:find(K, Env#mal_env.data) of
        {ok, _} -> {ok, Env};
        error -> error
    end;
find(K, Env) ->
    case maps:find(K, Env#mal_env.data) of
        {ok, _} -> {ok, Env};
        error -> find(K, Env#mal_env.outer)
    end.

get(K, Env) ->
    case find(K, Env) of
        {ok, Env} -> maps:get(K, Env#mal_env.data);
        error -> throw({malerr, io_lib:format("Symbol '~s' not found in env", [K#mal_sym.val])})
    end.

new(M) ->
    #mal_env{outer=root, data=M}.
