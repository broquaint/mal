-module(core).

-include("types.hrl").

-export([ns/0]).

is_equal(_,_) ->
    mal_false.

int_cmp(F) ->
    fun([#mal_num{val=A}, #mal_num{val=B}]) ->
            case F(A,B) of
                true -> mal_true;
                false -> mal_false
            end
    end.

num(#mal_num{val=V}) -> V;
num(V) when is_integer(V) -> #mal_num{val=V}.

to_bool(V) when V =:= true -> mal_true;
to_bool(V) -> mal_false.
    

% Separate function for indenting sanity.
functions() ->
    #{
      "prn" => fun([V]) -> printer:pr_str(V), mal_nil end, % TODO support print_readably
      "list" => fun(L) -> #mal_list{elems=L} end,
      "list?" => fun([L]) -> to_bool(is_record(L, mal_list)) end,
      "empty?" => fun([#mal_list{elems=L}]) -> to_bool(length(L) == 0) end,
      "count" => fun([#mal_list{elems=L}]) -> #mal_num{val=length(L)} end,
      "=" => fun([A,B]) -> is_equal(A,B) end,

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
