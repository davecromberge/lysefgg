-module(exceptions).
-compile(export_all).

%% The types of errors that may be found include:
%% compile-time, run-time, logical, generated
%% Most of the time, the acceptable solution is to let the code crash.
%% There are three types of exceptions that may be raised in erlang:
%% Errors, Exits and Throws.

%% Throws do not carry the intent to crash a process, and the intention is for
%% the calling process to handle the error. They can also be used for control
%% flow, when deep in the bowels of a recursive computation it can be used to
%% abort the computation.
%% exceptions:throws(fun() -> throw(thrown) end).
%% {throw,caught,thrown}
%% exceptions:throws(fun() -> erlang:error(pang) end).
%% ** exception error: pang
%%      in function  exceptions:throws/1 (exceptions.erl, line 5)
throws(F) ->
    try F() of
        _ -> ok
    catch
        Throw -> {throw, caught, Throw}
    end.

%% Errors end the execution of the current process and includes a stack trace.
%% There is no expectation on the calling code to handle the error.
%% erlang:error(badarith).
%% ** exception error: bad argument in an arithmetic expression
%%
%% exceptions:errors(fun() -> erlang:error(pang) end).
%% {error,caught,pang}
errors(F) ->
    try F() of
        _ -> ok
    catch 
        error: Error -> {error, caught, Error}
    end.

%% Internal exit/1 vs external exits exit/2 are similar to errors ande are used 
%% to communicate erorrs to other monitoring and linked processes.
%% These are communicated over a special channel, before the process dies.
%% exceptions:exits(fun() -> exit(goodbye) end).
%% {exit,caught,goodbye}
exits(F) ->
    try F() of 
        _ -> ok
    catch 
        exit: Exit -> {exit, caught, Exit}
    end.

%% It is possible to catch all the types of exceptions in a single try:
sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).
talk() -> "blah blah".

%% is_function gaurd BIF can also test arity of the function arg
black_knight(Attack) when is_function(Attack, 0) ->
    try Attack() of 
        _ -> "None shall pass"
    catch 
        throw:slice   -> "It is but a scratch.";
        error:cut_arm -> "I have had worse.";
        exit:cut_leg  -> "Come on you pansy!";
        _:_           -> "Just a flesh wound." %% be wary of catch all patterns
    after
        "This S/E code is an opportunity to clean up, cannot get a return value 
        from here"
    end.

%% the try construct can test more than one expression
whoa() ->
    try 
        %% recursive calls should be avoided in the protected part
        talk(),
        _Knight = "none shall pass",
        _Doubles = [ X || X <- lists:seq(1,100)],
        throw(up),
        _WillReturnThis = tequila %% the value of the entire clause is the last expression
    of %% the of part is optional, since sometimes we just wish to recover from an error
        tequila -> "hey this worked"
    catch
        Exception:Reason -> {caught, Exception, Reason}
    end.

%% catch construct is an alternate error handling stucture.  It can capture all
%% types of exceptions but does change the representation of the exceptions.
%% E.g:
%% catch throw(woah).
%% whoa
%% catch exit(die).
%% {'EXIT', die}
%% catch 1/0.
%% {EXIT, badarith, [...]}
%%
%% Problems with catch are:
%%   Has a lower operator precedence
%%   Obscure stack traces
%%   Can't tell difference between error and exit
catcher(X,Y) ->
    case catch X/Y of 
        {'EXIT', {badarith, _}} -> "uh oh";
        N -> N
    end.

%% An example of the ambiguity between values and errors is as follows:
%% exceptions:one_or_two(1).
%% return
%% exceptions:one_or_two(2).
%% return
one_or_two(1) -> return;
one_or_two(2) -> throw(return).
