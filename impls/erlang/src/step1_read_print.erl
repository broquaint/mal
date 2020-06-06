-module(step1_read_print).

-export([main/1]).

main(_) ->
    case io:get_line(standard_io, "user> ") of
        eof ->
            io:format("~n"),
            ok;
        {error, ErrorDescription} ->
            io:format("Error reading input: ~p~n", [ErrorDescription]),
            exit(ioerr);
        Input ->
            {Ast, _} = print(eval(read(Input))),
            io:format("~s~n", [Ast]),
            main("")
        end.

read(Code) ->
    % TODO Errors!
    reader:read_str(Code).

eval(Code) ->
    Code.

print(Code) ->
    Code.

