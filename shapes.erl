%%%  
%%% module shapes - count perimeter and enclose of given shape
%%% pavelw, 2017
%%% shapes: rectangle, square, triangle, cicle
%%%
-module(shapes).
-author('pavel.west@gmail.com').

-export([perimeter/1, enclose/1, test_shapes_perimeter/0, test_shapes_enclose/0, test_all/0,  maximum/1]).

%% check if triangle exists
check_triangle(A,B,C) when (A+B > C), (B+C > A), (A+C > B) ->
    true;
check_triangle(_,_,_) ->
    false.

%%
%% calculate a perimeter of a shapes
%% rectangle, square, circle, triangle
%%
perimeter({rectangle, X, Y}) when X>0, Y>0 ->
    2*(X+Y);

perimeter({square, X}) when X>0 ->
    perimeter({rectangle, X, X});

perimeter({circle, R}) when R>0 ->
    2*math:pi()*R;

perimeter({triangle, A, B, C}) ->
    case check_triangle(A,B,C) of
            true -> ( A + B + C );
            false -> { error, "Non existing triangle"}
    end.
%%
%% calculate enclose of a shape
%%
enclose({rectangle, X, Y}) when X>0, Y>0 ->
    {rectangle, X,Y};

enclose({square, X}) when X>0 ->
    enclose({rectangle, X, X});

enclose({circle, R}) when R>0 ->
    enclose({square, 2*R});

enclose({triangle, A, B, C}) ->
    case check_triangle(A,B,C) of
            true -> 
                P = ( A + B + C )/2.0,
                L = maximum([A,B,C]),
                V = (2/L)*math:sqrt(P*(P-A)*(P-B)*(P-C)),
                enclose({rectangle,L,round(V,2)});
            false -> { error, "Non existing triangle"}
    end.
%%
%% helpers functions
%%

%%
%% find max in list
%% I can use lists:max too, but to practise recursion ... 
%%
maximum(List) ->
    maxT(List, 0).

maxT([],Val) ->
    Val;

maxT(List, Val) ->
    Max = case Val =< hd(List) of
              true -> hd(List);
              false -> Val
        end,
    maxT(tl(List), Max).

round(Number, Precision) ->
        P = math:pow(10, Precision),
            round(Number * P) / P.

%%
%% Test of shapes 
%%
test_shapes_perimeter() ->
    io:format("**** TEST OF PERIMETER ****~n"),
    io:format("rectangle [2,3] = ~p~n",[perimeter({rectangle,2,3})]),
    io:format("square [2] = ~p~n",[perimeter({square,2})]),
    io:format("triangle [2,3,4] = ~p~n",[perimeter({triangle,2,3,4})]),
    io:format("triangle [4,5,1] = ~p~n",[perimeter({triangle,4,5,1})]),
    io:format("**** TEST OF PERIMETER ****~n").



test_shapes_enclose() ->
    io:format("**** TEST OF ENCLOSE ****~n"),
    io:format("rectangle [2,3] = ~p~n",[enclose({rectangle,2,3})]),
    io:format("square [2] = ~p~n",[enclose({square,2})]),
    io:format("triangle [2,3,4] = ~p~n",[enclose({triangle,2,3,4})]),
    io:format("triangle [4,5,1] = ~p~n",[enclose({triangle,4,5,1})]),
    io:format("**** TEST OF ENCLOSE ****~n").

%% 
%% run all tests
%%
test_all() ->
    test_shapes_perimeter(),
    io:format("~n"),
    test_shapes_enclose().
