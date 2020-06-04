-module(step0_repl).

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
            io:format("~s~n", [Input]),
            main("")
        end.

read(Code) ->
    Code.

eval(Code) ->
    Code.

print(Code) ->
    Code.
