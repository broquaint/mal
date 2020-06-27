-module(printer).

-include("types.hrl").

-export([pr_str/1]).

pr_str(Ast) ->
    if
        is_record(Ast, mal_list) ->
            Elems = [pr_str(E) || E <- Ast#mal_list.elems],
            "(" ++ string:join(Elems, " ") ++ ")";
        is_record(Ast, mal_vec) ->
            Elems = [pr_str(E) || E <- Ast#mal_vec.elems],
            "[" ++ string:join(Elems, " ") ++ "]";
        is_record(Ast, mal_map) ->
            Pairs = maps:to_list(Ast#mal_map.pairs),
            Elems = [pr_str(K) ++ " " ++ pr_str(V) || {K,V} <- Pairs],
            "{" ++ string:join(Elems, " ") ++ "}";
        is_record(Ast, mal_str) ->
            "\x22" ++ Ast#mal_str.val ++ "\x22";
        is_record(Ast, mal_num) ->
            integer_to_list(Ast#mal_num.val);
        is_record(Ast, mal_kwd) ->
            ":" ++ Ast#mal_kwd.val;
        is_record(Ast, mal_sym) ->
            Ast#mal_sym.val;
        Ast =:= mal_nil ->
            "nil";
        Ast =:= mal_true ->
            "true";
        Ast =:= mal_false ->
            "false";
        is_record(Ast, mal_fn) ->
            "#<user_function>";
        is_function(Ast) ->
            "#<core_function>";
        true ->
            io_lib:format("WAT ~p", [Ast])
    end.
