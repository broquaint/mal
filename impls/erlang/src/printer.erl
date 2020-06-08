-module(printer).

-include("types.hrl").

-export([pr_str/1]).

pr_str(Ast) ->
    if
        is_record(Ast, mal_list) ->
            Elems = [pr_str(E) || E <- Ast#mal_list.elems],
            "(" ++ string:join(Elems, " ") ++ ")";
        is_record(Ast, mal_str) ->
            "\x22" ++ Ast#mal_str.val ++ "\x22";
        is_record(Ast, mal_num) ->
            integer_to_list(Ast#mal_num.val);
        is_record(Ast, mal_sym) ->
            Ast#mal_sym.val
    end.
