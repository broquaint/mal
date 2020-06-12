-module(step2_eval).

-export([main/1]).

main(_) -> repl().

repl() ->
    case io:get_line(standard_io, "user> ") of
        eof ->
            io:format("~n"),
            ok;
        {error, ErrorDescription} ->
            io:format("Error reading input: ~p~n", [ErrorDescription]),
            exit(ioerr);
        Input ->
            case read(Input) of
                {success, Ast} ->
                    io:format("~s~n", [print(eval(Ast))]);
                {error, Err} -> io:format("Error: ~s~n", [Err])
            end,
            repl()
        end.

read(Code) ->
    % TODO Errors!
    reader:read_str(Code).

eval(Code) ->
    Code.

print(Ast) ->
    printer:pr_str(Ast).

