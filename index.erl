-module(index).
-export([get_file_contents/1,show_file_contents/1, line_index/1, label_word/1, label_all_lines/1, print_words/1, main/0]).
% -compile(export_all).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
    lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.

%% 
%% helper functions
%% pavelw
%% various test functions
%%
is_member(_,[]) -> false;
is_member(M, [M|_Ls]) -> true;
is_member(M, [_L|Ls]) ->
    is_member(M,Ls).

%%
%% create list of word without repeating words
%%
nub(List) ->
    nub(List, []).

nub([], Ret) -> Ret;
nub([L|Ls], Ret) ->
    case is_member(L, Ret) of
        true -> nub(Ls, Ret);
        false -> nub(Ls, [L|Ret])
    end.

%%
%% index lines of file
%%
line_index(Lines) ->
    line_index(Lines, 1, []).

line_index([],_Idx,Ret) ->
    lists:reverse(Ret);
line_index([L|Ls], Idx, Ret ) ->
    io:format("~p: ~p ~n",[Idx, L]),
    % line_index(Ls,Idx+1,[{Idx, nub(string:tokens(L," "))}|Ret]).
    line_index(Ls,Idx+1,[{Idx, nub(lowerList(string:tokens(L," ")))}|Ret]).


%% test join function
join_([X|Xs],Ys) ->
        % io:format("~p~n",[[X|Ys]]),
        join_(Xs, [X | Ys]);
join_([], Ys) ->
        Ys.

%% test reverse function, not used at last
reverse(Xs) ->
        lists:foldl(fun (E,A) -> [E|A] end, [], Xs).

join2(Xs, Ys) ->
        join_(reverse(Xs), Ys).

%%
%% label word by line number
%%
label_word({_LineNo, []}) ->
    [];

label_word({LineNo, [W|Ws]}) ->
    [{W,LineNo} | label_word({LineNo, Ws})].

label_all_lines([]) ->
    [];
label_all_lines([L|Ls]) ->
    join2(label_word(L),label_all_lines(Ls)).

%% 
%% remove punctuation from word
%%
removepunct([]) -> [];
removepunct([L|Ls]) ->
    case is_member(L," .,;\:!?'[]`\"") of
        true -> removepunct(Ls);
        false -> [L|removepunct(Ls)]
    end.


%%
%% lower case word
lowCase(L) ->
    case $A =< L andalso L =< $Z of
        true -> L + 32;
        false -> L
    end.

lowercase([]) -> [];
lowercase([C|Cs]) ->
   [lowCase(C) | lowercase(Cs)].

%% 
%% lowercase list of words
%% 
lowerList([]) ->
    [];
lowerList([L|Ls]) ->
    [lowercase(removepunct(L)) | lowerList(Ls)].


%% print all labels
print_words([])->
    [];
print_words([W|Ws]) ->
    {Word, LineNo} = W,
    io:format("~p:~p~n",[Word, LineNo]),
    print_words(Ws).

%%
%% put the indexes to Map #{ "key", [line numbers] }
%% create an empty maps for index
%% create Map with just keys  ( without values )
%%
make_map(List) ->
    make_map(List, maps:new()).

make_map([], Map) ->
    Map;
make_map([L|Ls], Map ) ->
    {Key,_} = L,
    % io:format("Key = ~p~n",[Key]),
    make_map(Ls,maps:put(Key,[],Map)).

%%
%% create complete index
%% assign line number of word ( key ) to map
%%
words_index([], Map) ->
    Map;

words_index([L|Ls],Map) ->
    {Key,Val} = L,
    % io:format("Key = ~p, Val = ~p ~n",[Key,Val]),
    % get old value from map
    Vold = maps:get(Key,Map),
    % create new value for key
    NewVal = [Val|Vold],
    % io:format("Vold = ~p, Val = ~p ~n",[Vold, Val]),
    % put new values to Map
    NMap = maps:update(Key, NewVal, Map),
    % io:format("Nmap = ~p ~n",[NMap]),
    words_index(Ls,NMap).

%%
%% convert all index to tuple and print it
%% print result
%%
print_all_index([],[]) ->
    ok;
print_all_index([K|Ks],[V|Vs]) ->
    io:format("~p -> ~p~n",[K,list_to_tuple(lists:reverse(V))]),
    print_all_index(Ks,Vs).

%% print whole index of file
print_index(Map) ->
    Keys = maps:keys(Map),
    Vals = maps:values(Map),
    print_all_index(Keys, Vals).

%% main function, for file
main() ->
    % Words = label_all_lines(line_index(get_file_contents("gettysburg-address.txt"))),
    Words = label_all_lines(line_index(get_file_contents("dickens-christmas.txt"))),
    % print_words(Words),
    MainIndex = words_index(Words, make_map(Words)),
    io:format("=================== INDEX ==========================~n",[]),
    print_index(MainIndex).
