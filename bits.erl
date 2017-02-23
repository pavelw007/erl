%%% 
%%% Sum of all 1's in binary representation of decimal number
%%% pavelw, 2017
%%%
%%%
-module(bits).
-author('pavel.west@gmail.com').

-export([bits/1, dec2bin/1, revlist/1, bitsO/1, test_bits/1, bin2str/1, bitsR/1]).

%%
%% sum all 1's in binary representation of decimal number
%%
bits(N) ->
    bitsT(N,bits:dec2bin(N),0).

bitsT(_,[],Acc) ->
    % io:format("Acc return~n",[]),
    Acc;
bitsT(N,List,Acc) ->
    bitsT(N ,tl(List), Acc + hd(List)).

%%
%% convert decimal to binary number
%% ( list of 0 and 1 ) 
%%
dec2bin(N) ->
    dec2binT(N,[]).

dec2binT(N,List) when N =< 0 ->
    List;
dec2binT(N,List) when N > 0-> 
    % debug list
    % io:format("N = ~p, List = ~p ~p~n",[N,List, N div 2]),
    dec2binT(N div 2,[N rem 2 | List]).


%% reverse list
revlist(List) when is_list(List) ->
    revlistT(List,[]).

revlistT([], Array) ->
    Array;
revlistT(List, Array ) ->
    revlistT(tl(List), [hd(List) | Array]).

%%
%% another solution recursion
%% during conversion from dec to bin just sum 1's 
%% 
bitsO(N) ->
    bitsOT(N,0).

bitsOT(N,Acc) when N =< 0 ->
    Acc;
bitsOT(N,Acc) when N >0 ->
    % io:format("~p -> ~p~n",[(N div 2), (N rem 2)]),
    bitsOT(N div 2, Acc + ( N rem 2 )).

%%
%% convert bin to printable string
%% for debug logs
bin2str(List) ->
    bin2strT(List, [] ).

bin2strT([], Ret) -> 
    revlist(Ret);

%% 
%% change 1 to $1 an 0 to $0 ASCII code
%%
bin2strT(List, Ret) when is_list(List) ->
    X = case hd(List) of
        1 -> 49;
        0 -> 48
    end,
    bin2strT(tl(List),[X|Ret]).


%%
%% recursive sum of 1's in bin representation
%% no tail rec. version
%%
bitsR(0) -> 0;
bitsR(N) when N > 0 ->
    ( N rem 2 ) + bitsR(N div 2).


%%
%% test all version from 1 to number N
%% just simple version, better would be to generate random numbers
%% by random:uniform(num)
%% 
test_bits(N) ->
    Y = [ {X, bits(X) == bitsO(X), bits(X), bitsO(X), bitsR(X), bin2str(dec2bin(X)) } || X <- lists:seq(1,N)],
    [ io:format("Result for ~p is ~p ( ~p == ~p/~p ) ~s~n",[A, B, B1, B2, B3, C]) ||  {A,B, B1, B2, B3,  C} <- Y ].
