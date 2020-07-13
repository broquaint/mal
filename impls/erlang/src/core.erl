-module(core).

-include("types.hrl").

-export([call/2, ns/0]).

to_bool(V) when V =:= true -> mal_true;
to_bool(_) -> mal_false.

compare_lists([], []) -> true;
compare_lists(X, Y) when length(X) =:= 0 orelse length(Y) =:= 0 -> false;
compare_lists([H1|T1], [H2|T2]) ->
    is_equal(H1, H2) and compare_lists(T1, T2).

compare_maps(M1, M2) ->
    Comp = fun(K, V, Acc) ->
                   Acc and maps:is_key(K, M2) and is_equal(V, maps:get(K, M2, no_key))
           end,
    maps:fold(Comp, true, M1).

% Make use of the fact that records are just syntax sugar for tuples.
% Can't assert V1/V2 are lists as this would also apply to strings >_<
is_equal({R1, V1}, {R2, V2})
  when (R1 == mal_list orelse R1 == mal_vec) and (R2 == mal_list orelse R2 == mal_vec) ->
    length(V1) =:= length(V2) andalso compare_lists(V1, V2);
is_equal({_, V1}, {_, V2}) when is_map(V1) and is_map(V2) ->
    (maps:size(V1) =:= maps:size(V2)) and compare_maps(V1, V2);
is_equal({R1, V1}, {R2, V2}) ->
    R1 =:= R2 andalso V1 =:= V2;
is_equal(A, B) ->
    A =:= B.

int_cmp(F) ->
    fun([#mal_num{val=A}, #mal_num{val=B}]) -> to_bool(F(A,B)) end.

% List support functions for pattern matching goodness.

cons([]) -> [];
cons([A, {_, L}]) -> [A] ++ L.

concat([]) -> [];
concat([{_, L}|Tail]) -> L ++ concat(Tail).

first(mal_nil) -> mal_nil;
first({_, []}) -> mal_nil;
first({_, [H|_]}) -> H.

rest(mal_nil) -> [];
rest({_, []}) -> [];
rest({_, [_|T]}) -> T.

make_map(P) -> make_map(#{}, P).
make_map(Map, []) -> #mal_map{pairs=Map};
make_map(Map, [K,V|T]) ->
    make_map(maps:put(K, V, Map), T).

dissoc(Map, []) -> #mal_map{pairs=Map};
dissoc(Map, [K|T]) -> dissoc(maps:remove(K, Map), T).

% Helper functions record construction.

num(#mal_num{val=V}) -> V;
num(V) when is_integer(V) -> #mal_num{val=V}.

str(V) -> #mal_str{val=V}.

list(L) -> #mal_list{elems=L}.

die(F, V) -> die(io_lib:format(F, V)).
die(E) -> throw({malerr, E}).

call(F, Args) ->
    case F of
        #mal_fn{ast=Body, params=Params, env=CEnv, eval=Eval} ->
            Eval(Body, env:for_fn(Params, Args, CEnv));
        _ ->
            F(Args)
    end.

% Separate function for indenting sanity.

functions() ->
    #{
      "dbg" => fun(A) -> io:format("~p~n", A), mal_nil end,
      "pr-str" =>
          fun(Args) ->
                  Strs = lists:map(fun(V) -> printer:pr_str(V, true) end, Args),
                  str(string:join(Strs, " "))
          end,
      "str" =>
          fun(Args) ->
                  Strs = lists:map(fun(V) -> printer:pr_str(V, false) end, Args),
                  str(string:join(Strs, ""))
          end,
      "prn" =>
          fun(Args) ->
                  Strs = lists:map(fun(V) -> printer:pr_str(V, true) end, Args),
                  io:format("~s~n", [string:join(Strs, " ")]),
                  mal_nil
          end,
      "println" =>
          fun(Args) ->
                  Strs = lists:map(fun(V) -> printer:pr_str(V, false) end, Args),
                  io:format("~s~n", [string:join(Strs, " ")]),
                  mal_nil
          end,
      "list" => fun(L) -> list(L) end,
      "list?" => fun([L]) -> to_bool(is_record(L, mal_list)) end,
      "empty?" => fun([{_, L}]) -> to_bool(length(L) == 0) end,
      "count" =>
          fun([L]) ->
                  case L of
                      {_, V} -> num(length(V));
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
      "/" => fun([A,B]) -> num(trunc(num(A) / num(B))) end,

      "read-string" =>
          fun([#mal_str{val=S}]) ->
                  case reader:read_str(S) of
                      {success, Ast} -> Ast;
                      {error, Err} -> die("Parse fail: ~s", [Err])
                  end
          end,
      "slurp" =>
          fun([#mal_str{val=F}]) ->
                  case file:read_file(F) of
                      {ok, Data} -> str(binary_to_list(Data));
                      {error, E} ->
                          die("Couldn't open '~s' for reading: ~p", [F, E])
                  end
          end,
      "atom" => fun([V]) -> #mal_atom{pid=atom:new(V)} end,
      "atom?" => fun([V]) -> to_bool(is_record(V, mal_atom)) end,
      "deref" => fun([#mal_atom{pid=Pid}]) -> atom:deref(Pid) end,
      "reset!" => fun([#mal_atom{pid=Pid}, V]) -> atom:reset(V, Pid), V end,
      "swap!" => fun([#mal_atom{pid=Pid}, F|Args]) -> atom:swap(F, Args, Pid) end,

      "cons" => fun(L) -> list(cons(L)) end,
      "concat" => fun(L) -> list(concat(L)) end,

      "nth" =>
          fun([{_, L}, {_, I}]) ->
                  if
                      I >= 0 andalso I =< length(L) -> lists:nth(I + 1, L);
                      true -> die("index ~p out of bounds", [I])
                  end
          end,
      "first" => fun([L]) -> first(L) end,
      "rest" => fun([L]) -> list(rest(L)) end,

      "throw" => fun([M]) -> throw({malerr, M}) end,

      "apply" =>
          fun([F|A]) ->
                  {_, L} = lists:last(A),
                  call(F, lists:droplast(A) ++ L)
          end,
      "map" =>
          fun([F, {_, L}]) ->
                  Tr = fun(E) -> call(F, [E]) end,
                  list(lists:map(Tr, L))
          end,

      "symbol?" => fun([V]) -> to_bool(is_record(V, mal_sym)) end,
      "keyword?" => fun([V]) -> to_bool(is_record(V, mal_kwd)) end,
      "nil?" => fun([V]) -> to_bool(V =:= mal_nil) end,
      "true?" => fun([V]) -> to_bool(V =:= mal_true) end,
      "false?" => fun([V]) -> to_bool(V =:= mal_false) end,
      "vector?" => fun([V]) -> to_bool(is_record(V, mal_vec)) end,
      "sequential?" => fun([V]) -> to_bool(is_record(V, mal_list) or is_record(V, mal_vec)) end,
      "map?" => fun([V]) -> to_bool(is_record(V, mal_map)) end,

      "symbol" => fun([{_,S}]) -> #mal_sym{val=S} end,
      "keyword" => fun([{_,S}]) -> #mal_kwd{val=S} end,
      "vector" => fun(A) -> #mal_vec{elems=A} end,
      "hash-map" => fun(P) -> make_map(P) end,

      "assoc" => fun([M|P]) -> make_map(M#mal_map.pairs, P) end,
      "dissoc" => fun([M|P]) -> dissoc(M#mal_map.pairs, P) end,
      "contains?" => fun([M, K]) -> to_bool(maps:is_key(K, M#mal_map.pairs)) end,
      "keys" => fun([M]) -> list(maps:keys(M#mal_map.pairs)) end,
      "vals" => fun([M]) -> list(maps:values(M#mal_map.pairs)) end,
      "get" =>
          fun([M, K]) ->
                  case M of
                      mal_nil -> mal_nil;
                      _ -> maps:get(K, M#mal_map.pairs, mal_nil)
                  end
          end,

      % Simplify adding new functions i.e no need to worry about trailing comma
      "identity" => fun([V|_]) -> V end
     }.

ns() ->
    StrToSym = fun(K, F, Acc) -> maps:put(#mal_sym{val=K}, F, Acc) end,
    maps:fold(StrToSym, #{}, functions()).
