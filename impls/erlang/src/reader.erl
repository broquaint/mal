-module(reader).

-include("types.hrl").

-export([read_str/1]).

tokenize(Code) ->
    MalSyntaxRe = "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\x22(?:\\.|[^\\\x22])*\x22?|;.*|[^\\s\[\\]{}('\x22`,;)]+)",
    % run(Subject, RE, Options) ->
    %   {match, Captured} | match | nomatch | {error, ErrType}
    case re:run(Code, MalSyntaxRe, [global]) of
        {match, Captured} -> 
            Tokens = [lists:sublist(Code, Pos + 1, Len) || [_, {Pos, Len}] <- Captured],

            {success, Tokens};
        nomatch -> {error, "Failed to match"}
        % As this is a global match and a valid RE we shouldn't encounter these.
        % match | {error, ErrType} -> 
        end.

seq_for(End) ->
    case End of
        ")" -> #mal_list{elems=[]};
        "]" -> #mal_vec{elems=[]};
        "}" -> #mal_list{elems=[]} % Used to build up pairs
    end.
mal_sym(V) -> #mal_sym{val=V}.

add_to_seq(Seq, Elem) ->
    if is_record(Seq, mal_list) ->
            Seq#mal_list{elems=Seq#mal_list.elems ++ [Elem]};
       is_record(Seq, mal_vec) ->
            Seq#mal_vec{elems=Seq#mal_vec.elems ++ [Elem]}
    end.

read_seq([], Delim) -> {seq_for(Delim), []};
read_seq([H|T], Delim) -> read_seq([H|T], seq_for(Delim), Delim).

read_seq([], _, _) -> throw(unbalanced_seq);
read_seq(Tokens, Seq, Delim) ->
    [H|Tail] = Tokens,
    case H of
        Delim -> {Seq, Tail};
        _ -> {Elem, Rest} = read_form(Tokens),
             read_seq(Rest, add_to_seq(Seq, Elem), Delim)
    end.

read_map(Tokens) ->
    {Ast, Tail} = read_seq(Tokens, "}"),
    if length(Ast#mal_list.elems) rem 2 == 0 ->
            {make_map(#mal_map{pairs=#{}}, Ast#mal_list.elems), Tail};
       true -> throw(uneven_map)
    end.

add_to_map(K, V, Map) ->
    NewMap = maps:put(K, V, Map#mal_map.pairs),
    Map#mal_map{pairs=NewMap}.

make_map(Map, []) -> Map;
make_map(Map, [K,V|Rest]) ->
    make_map(add_to_map(K, V, Map), Rest).

read_atom([Token|Tail]) ->
    [C1|_] = Token,
    ReaderMacro = fun(Sym) ->
                          {Quoted, Rest} = read_form(Tail),
                          {mal_list([mal_sym(Sym), Quoted]), Rest}
                  end,
    Atom = fun(A) -> {A, Tail} end,
    if C1 =:= $" ->
            Atom(#mal_str{val=lists:sublist(Token, 2, length(Token) - 2)});
       C1 >= $0, C1 =< $9 -> % Hopefully useful Heuristic,
            {Num,_} = string:to_integer(Token),
            Atom(#mal_num{val=Num});
       C1 =:= $: ->
            Atom(#mal_kwd{val=lists:sublist(Token, 2, length(Token) - 1)});
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
        "(" -> read_seq(Tail, ")");
        "[" -> read_seq(Tail, "]");
        "{" -> read_map(Tail);
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
