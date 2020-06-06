-module(reader).

-export([read_str/1]).

tokenize(Code) ->
    MalSyntaxRe = "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\x22(?:\\.|[^\\\x22])*\x22?|;.*|[^\\s\[\\]{}('\x22`,;)]+)",
    % run(Subject, RE, Options) ->
    %   {match, Captured} | match | nomatch | {error, ErrType}
    case re:run(Code, MalSyntaxRe, [global]) of
        {match, Captured} -> 
            Tokens = [lists:sublist(Code, Pos + 1, Len) || [_, {Pos, Len}] <- Captured],

            {success, #{tokens=>Tokens,pos=>1}};
        nomatch -> {error, "Failed to match"}
        % As this is a global match and a valid RE we shouldn't encounter these.
        % match | {error, ErrType} -> 
        end.

r_peek(Reader) ->
    lists:nth(maps:get(pos, Reader), maps:get(tokens, Reader)).

r_next(Reader) ->
    CurPos = maps:get(pos, Reader),
    Token  = lists:nth(CurPos, maps:get(tokens, Reader)),
    {Token, maps:update(pos, CurPos + 1, Reader)}.

%r_before_end(Reader) ->
%    maps:get(pos, Reader) < length(maps:get(tokens, Reader)).

read_seq(Reader) ->
    {_, RBegin} = r_next(Reader), % Move past opening paren.
    read_seq(RBegin, []).

read_seq(Reader, List) ->
    case r_peek(Reader) of
        ")" -> {_, REnd} = r_next(Reader),
               {List, REnd};
        _   -> {Element, RNext} = read_form(Reader),
               {List ++ [Element], RNext}
    end.

read_atom(Reader) ->
    r_next(Reader).

read_form(Reader) ->
%    io:format("now at ~p~n", [r_peek(Reader)]),
    case r_peek(Reader) of
        "(" -> read_seq(Reader);
        %% "[" => read_vec(r);
        %% "{" => read_map(r);
        _Else -> read_atom(Reader)
    end.

read_str(Code) ->
    case tokenize(Code) of
        {success, Reader} -> read_form(Reader);
        {error, Err} -> Err
        end.
