-module(core).

-include("types.hrl").

-export([ns/0]).

to_bool(V) when V =:= true -> mal_true;
to_bool(_) -> mal_false.

% Make use of the fact that records are just syntax sugar for tuples.
is_equal({R1, V1}, {R2, V2}) ->
    R1 =:= R2 andalso V1 =:= V2;
is_equal(A, B) ->
    A =:= B.

int_cmp(F) ->
    fun([#mal_num{val=A}, #mal_num{val=B}]) -> to_bool(F(A,B)) end.

% This is a bit too cute.
num(#mal_num{val=V}) -> V;
num(V) when is_integer(V) -> #mal_num{val=V}.

% Separate function for indenting sanity.
functions() ->
    #{
      "prn" => fun([V]) ->
                       io:format("~s~n", [printer:pr_str(V)]),
                       mal_nil
               end, % TODO support print_readably
      "list" => fun(L) -> #mal_list{elems=L} end,
      "list?" => fun([L]) -> to_bool(is_record(L, mal_list)) end,
      "empty?" => fun([#mal_list{elems=L}]) -> to_bool(length(L) == 0) end,
      "count" => fun([L]) ->
                         case L of
                             #mal_list{elems=V} -> num(length(V));
                             mal_nil -> num(0)
                         end
                 end,
      "=" => fun([A,B]) -> to_bool(is_equal(A,B)) end,

      "<"  => int_cmp(fun(A,B) -> A < B end),
      "<=" => int_cmp(fun(A,B) -> A =< B end),
      ">"  => int_cmp(fun(A,B) -> A > B end),
      ">=" => int_cmp(fun(A,B) -> A >= B end),

      "+" => fun([A,B]) -> num(num(A) + num(B)) end,
      "-" => fun([A,B]) -> num(num(A) - num(B)) end,
      "*" => fun([A,B]) -> num(num(A) * num(B)) end,
      "/" => fun([A,B]) -> num(trunc(num(A) / num(B))) end
     }.

ns() ->
    StrToSym = fun(K, F, Acc) -> maps:put(#mal_sym{val=K}, F, Acc) end,
    maps:fold(StrToSym, #{}, functions()).
