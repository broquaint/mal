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

%% is_mal_int(Token) ->
%%     % Just apply to_integer?
%%     case re:run(Token, "^[0-9]+$") of
%%         {match, _} -> true;
%%         _ -> false
%%     end.

%% mal_list_append(List, Elem) ->
%%     mal_list_append(List, [Elem]).
%% mal_list_append(List, [Elem]) ->
%%     CurElems = List#mal_list{elems},
%%     List#mal_list{elems = CurElems ++ Elem}.

read_seq([]) -> {#mal_list{elems=[]},[]};
read_seq([H|T]) -> read_seq([H|T], #mal_list{elems=[]}).

read_seq([], List) -> {List, []};
read_seq(Tokens, List) ->
    [H|Tail] = Tokens,
    case H of
        ")" -> {List, Tail};
        _   -> 
            {Elem, Rest} = read_form(Tokens),
            NewList = List#mal_list.elems ++ [Elem],
            read_seq(Rest, List#mal_list{elems=NewList})
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
        "(" -> read_seq(Tail);
        %% "[" => read_vec(r);
        %% "{" => read_map(r);
        _Else -> read_atom(Tokens)
    end.

read_str(Code) ->
    case tokenize(Code) of
        {success, Reader} -> 
            {Ast, _} = read_form(Reader),
            Ast;
        {error, Err} -> Err
        end.
