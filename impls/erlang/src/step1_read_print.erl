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
            io:format("~s~n", [print(eval(read(Input)))]),
            main("")
        end.

read(Code) ->
    % TODO Errors!
    reader:read_str(Code).

eval(Code) ->
    Code.

print(Ast) ->
    printer:pr_str(Ast).

