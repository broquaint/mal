-module(printer).

-include("types.hrl").

-export([pr_str/1, pr_str/2]).

pr_str(Ast) -> pr_str(Ast, true).
pr_str(Ast, R) ->
    if
        is_record(Ast, mal_list) ->
            Elems = [pr_str(E, R) || E <- Ast#mal_list.elems],
            "(" ++ string:join(Elems, " ") ++ ")";
        is_record(Ast, mal_vec) ->
            Elems = [pr_str(E, R) || E <- Ast#mal_vec.elems],
            "[" ++ string:join(Elems, " ") ++ "]";
        is_record(Ast, mal_map) ->
            Pairs = maps:to_list(Ast#mal_map.pairs),
            Elems = [pr_str(K, R) ++ " " ++ pr_str(V, R) || {K,V} <- Pairs],
            "{" ++ string:join(Elems, " ") ++ "}";
        is_record(Ast, mal_str) ->
            handle_string(Ast#mal_str.val, R);
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

handle_string(Str, R) ->
    case R of
        true ->
            Escapes = [
                       {"\x5c", "\x5c\x5c"},  % \ -> \\
                       {"\x22", "\x5c\x22"},  % " -> \"
                       {"\n",   "\x5cn"}     % ␤ -> \n

                      ],
            Esc = fun({From, To}, S) -> string:replace(S, From, To, all) end,
            io_lib:format("\x22~s\x22", [lists:foldl(Esc, Str, Escapes)]);
        _ -> Str
    end.
