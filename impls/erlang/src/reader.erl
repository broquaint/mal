-module(reader).

-include("types.hrl").

-export([read_str/1]).

tokenize(Code) ->
    MalSyntaxRe = "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\x22(?:\x5c\x5c\x22|.*?)*\x22|;.*|[^\\s\[\\]{}('\x22`,;)]+)",
    case re:run(Code, MalSyntaxRe, [global]) of
        {match, Captured} -> 
            Tokens = [lists:sublist(Code, Pos + 1, Len) || [_, {Pos, Len}] <- Captured],
            {success, Tokens};
        nomatch -> {error, "Failed to match"}
    end.

mal_sym(V) -> #mal_sym{val=V}.

% mal_list(E) -> mal_list([E]);
mal_list(E) -> #mal_list{elems=E}.

read_seq([], _) -> {[], []};
read_seq([H|T], Delim) -> read_seq([H|T], [], Delim).

read_seq([], _, _) -> throw(unbalanced_seq);
read_seq(Tokens, Seq, Delim) ->
    [H|Tail] = Tokens,
    case H of
        Delim -> {Seq, Tail};
        _ -> {Elem, Rest} = read_form(Tokens),
             read_seq(Rest, Seq ++ [Elem], Delim)
    end.

read_map({Ast, Tail}) ->
    if
        length(Ast) rem 2 =/= 0 -> throw(uneven_map);
        true -> {#mal_map{pairs=make_map(#{}, Ast)}, Tail}
    end.

make_map(Map, []) -> Map;
make_map(Map, [K,V|Rest]) ->
    make_map(maps:put(K, V, Map), Rest).

make_string(RawStr) ->
    Escapes = [{"\x5c\x22", "\x22"},  % \" -> "
               {"\x5cn",    "\n"},    % \n -> ␤
               {"\x5c\x5c", "\x5c"}], % \\ -> \
    Esc = fun({From, To}, S) -> string:replace(S, From, To, all) end,
    #mal_str{val=lists:foldl(Esc, RawStr, Escapes)}.

read_atom([Token|Tail]) ->
    [C1|_] = Token,
    ReaderMacro = fun(Sym) ->
                          {Quoted, Rest} = read_form(Tail),
                          {mal_list([mal_sym(Sym), Quoted]), Rest}
                  end,
    Atom = fun(A) -> {A, Tail} end,
    if C1 =:= $" ->
            Atom(make_string(lists:sublist(Token, 2, length(Token) - 2)));
       C1 >= $0, C1 =< $9 -> % Hopefully useful Heuristic,
            Num = element(1, string:to_integer(Token)),
            Atom(#mal_num{val=Num});
       C1 =:= $-, length(Token) > 1 -> % TODO Figure out how to fold into num check above.
            Num = element(1, string:to_integer(Token)),
            Atom(#mal_num{val=Num});
       C1 =:= $: ->
            Atom(#mal_kwd{val=lists:sublist(Token, 2, length(Token) - 1)});
       Token == "nil" ->
            Atom(mal_nil);
       Token == "true" ->
            Atom(mal_true);
       Token == "false" ->
            Atom(mal_false);
       C1 =:= $' ->
            ReaderMacro("quote");
       C1 =:= $` ->
            ReaderMacro("quasiquote");
       Token == "~@" ->
            ReaderMacro("splice-unquote");
       C1 =:= $~ ->
            ReaderMacro("unquote");
       C1 =:= $@ ->
            [AtomName|Rest] = Tail,
            {mal_list([mal_sym("deref"), mal_sym(AtomName)]), Rest};
       C1 =:= $^ ->
            {Meta, R1} = read_form(Tail),
            {Val, Rest} = read_form(R1),
            {mal_list([mal_sym("with-meta"), Val, Meta]), Rest};
       true ->
            Atom(mal_sym(Token))
    end.

% Everything under read_form should return {List*, Tokens}
read_form([]) -> [];
read_form(Tokens) ->
    [H|Tail] = Tokens,
%    io:format("now at ~p~n", [r_peek(Reader)]),
    case H of
        "(" -> {List, Rest} = read_seq(Tail, ")"),
                   {mal_list(List), Rest};
        "[" -> {Vec, Rest} = read_seq(Tail, "]"),
               {#mal_vec{elems=Vec}, Rest};
        "{" -> read_map(read_seq(Tail, "}"));
        _Else -> read_atom(Tokens)
    end.

read_str(Code) ->
    case tokenize(Code) of
        {success, Reader} ->
            case catch read_form(Reader) of
                {Ast, _} -> {success, Ast};
                unbalanced_seq -> {error, "end of input"};
                uneven_map -> {error, "uneven pairs in map"}
                end;
        {error, Err} -> {error, Err}
        end.
