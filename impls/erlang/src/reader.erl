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
        "]" -> #mal_vec{elems=[]}
    end.

add_to_seq(Seq, Elem) ->
    if is_record(Seq, mal_list) ->
            Seq#mal_list{elems=Seq#mal_list.elems ++ [Elem]};
       is_record(Seq, mal_vec) ->
            Seq#mal_vec{elems=Seq#mal_vec.elems ++ [Elem]}
    end.

read_seq([], Delim) -> {seq_for(Delim), []};
read_seq([H|T], Delim) -> read_seq([H|T], seq_for(Delim), Delim).

% XXX Should this be an error case?
read_seq([], Seq, _) -> {Seq, []};
read_seq(Tokens, Seq, Delim) ->
    [H|Tail] = Tokens,
    case H of
        Delim -> {Seq, Tail};
        _ -> {Elem, Rest} = read_form(Tokens),
             read_seq(Rest, add_to_seq(Seq, Elem), Delim)
    end.

read_atom([Atom|Tail]) ->
    [C1|_] = Atom,
    Val = if C1 =:= $" ->
                  #mal_str{val=lists:sublist(Atom, 2, length(Atom) - 2)};
             C1 >= $0, C1 =< $9 -> % Hopefully useful Heuristic,
                  {Num,_} = string:to_integer(Atom),
                  #mal_num{val=Num};
             true ->
                  #mal_sym{val=Atom}
          end,
    {Val, Tail}.

% Everything under read_form should return {List*, Tokens}
read_form([]) -> [];
read_form(Tokens) ->
    [H|Tail] = Tokens,
%    io:format("now at ~p~n", [r_peek(Reader)]),
    case H of
        "(" -> read_seq(Tail, ")");
        "[" -> read_seq(Tail, "]");
        %% "{" => read_map(r);
        _Else -> read_atom(Tokens)
    end.

read_str(Code) ->
    case tokenize(Code) of
        {success, Reader} -> 
            element(1, read_form(Reader));
        {error, Err} -> Err
        end.
