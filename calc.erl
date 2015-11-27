%% Prefix notation or Polish notation places the operator before the operand in
%% an arithmetic expression. Reverse Polish notation or RPN is a notation where
%% the operator follows the operand.  This is useful as for efficiently parsing 
%% expressions and applying operator precedence correctly.
-module(calc).
-export([rpn/1, rpn_test/0]).

%% Erlang lists act like a stack, and using the Cons (|) operator is suitable
%% for pushing the Head back onto the stack.  The string:tokens/2 library
%% function converts a string into a list of characters and correctly splits on
%% whitespace.
rpn(L) when is_list(L) ->
    X = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    case X of
        [Res] -> Res;
        _     -> badarith
    end.

%% Apply the operator to the first two values on the stack, push the result
rpn("+", [N1,N2|Stack]) -> [N1+N2|Stack];
rpn("-", [N1,N2|Stack]) -> [N2-N1|Stack];
rpn("/", [N1,N2|Stack]) -> [N2/N1|Stack];
rpn("*", [N1,N2|Stack]) -> [N1*N2|Stack];
rpn("^", [N1,N2|S]) -> [math:pow(N2,N1)|S];
rpn("ln", [N|S])    -> [math:log(N)|S];
rpn("log10", [N|S]) -> [math:log10(N)|S];
%% Add an operand to the stack
rpn(X, Stack) -> [read(X)|Stack].

%% read is a function that converts a string to a floating point value or integer.
read(N) ->
    case string:to_float(N) of 
        {error, no_float} -> list_to_integer(N);
        {F, _} -> F
    end.

%% Erlang's = operator acts as an assertion function.
%% Frameworks such as CommonTest and EUnit are test frameworks designed for
%% this purpose.
rpn_test() ->
    5 = rpn("2 3 +"),
    87 = rpn("90 3 -"),
    -4 = rpn("10 4 3 + 2 * -"),
    -2.0 = rpn("10 4 3 + 2 * - 2 /"),
    ok = try
             rpn("90 34 12 33 55 66 + * - +")
         catch
             %% error:badarith -> ok; ??
             error:{badmatch,[_|_]} -> ok
         end,
    4037 = rpn("90 34 12 33 55 66 + * - + -"),
    8.0 = rpn("2 3 ^"),
    true = math:sqrt(2) == rpn("2 0.5 ^"),
    true = math:log(2.7) == rpn("2.7 ln"),
    true = math:log10(2.7) == rpn("2.7 log10"),
    ok.
