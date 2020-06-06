-module(reader).

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

%% r_peek(Reader) ->
%%     lists:nth(maps:get(pos, Reader), maps:get(tokens, Reader)).

%% r_next(Reader) ->
%%     CurPos = maps:get(pos, Reader),
%%     Token  = lists:nth(CurPos, maps:get(tokens, Reader)),
%%     {Token, maps:update(pos, CurPos + 1, Reader)}.

% -record(mal_list, {elems}).
% -record(mal_scalar, {val}).

%% mal_list_append(List, Elem) ->
%%     mal_list_append(List, [Elem]).
%% mal_list_append(List, [Elem]) ->
%%     CurElems = List#mal_list{elems},
%%     List#mal_list{elems = CurElems ++ Elem}.

read_seq([]) -> {[],[]};
read_seq([H|T]) -> read_seq([H|T], []).

read_seq([], List) -> {List, []};
%read_seq([H|Tail], List) when H == ")" -> {List, Tail};
read_seq(Tokens, List) ->
    [H|Tail] = Tokens,
    case H of
        ")" -> {List, Tail};
        _   -> 
            {Elem, Rest} = read_form(Tokens),
            read_seq(Rest, List ++ [Elem])
    end.

read_atom([H|Tail]) ->
    {H, Tail}.

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
