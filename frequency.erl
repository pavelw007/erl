%% 
%% Frequency server - basic version
%%
-module(frequency).
-author("pavel.west@gmail.com").

-export([start/0, allocate/1, deallocate/1, stop/1]).
-export([init/0]).
% for testing purposes only
% -compile(export_all).

%% 
%% functional interface
%%

%% start server
start() ->
    io:format("Starting frequency server ..."),
    register(?MODULE, spawn(?MODULE,init,[])),
    io:format(" started!~n").

%% alocate frequnecy
allocate(Timeout) ->
    frequency ! { request, self(), allocate},
    receive
        {reply, Msg } ->
            io:format("Response: ~p~n",[Msg]),
            Msg
        after Timeout ->
            io:format("Client timeout in allocation after ~p~n",[Timeout])
    end.

%% deallocate frequency    
deallocate({Timeout, Freq}) ->
    frequency ! { request, self(), {deallocate, Freq}},
    receive
        {reply, Msg } ->
            io:format("Response: ~p~n",[Msg])
        after Timeout ->
            io:format("Client timeout in deallocation after ~p~n",[Timeout])
    end.

%% stop server 
stop(Timeout) ->
    frequency ! { requeste, self(), stop},
    receive
        {reply, Msg} ->
            Msg
        after Timeout ->
            io:format("Server timeout ~n")
    end.
              
% generate frequencies for server
gen_frequencies() ->
    [10,11,12,13,14,15].

% initialize server
init() ->
    Frequencies = {gen_frequencies(),[]},
    loop(Frequencies).

%% server's main loop
loop(Freqs) ->
    receive
        % stop server
        {request, Pid, stop} ->
            % io:format("Server stopped~n"),
            Pid ! { reply, stopped};
        % for test purposeses only
        { request, _Pid, info} ->
            io:format("Statistic: ~p~n",[Freqs]),
            loop(Freqs);
        % allocate frequency
        {request, Pid, allocate} ->
            timer:sleep(2000),
            {NewFreqs, Reply} = allocate(Freqs, Pid),
            Pid ! { reply, Reply},
            loop(NewFreqs);
        % deallocate frequency
        {request, Pid, {deallocate, Freq}} ->
            NewFreqs = deallocate(Freqs, Freq),
            Pid ! { reply, ok},
            loop(NewFreqs);
        % code reloading
        {request, _Pid, code_reload } ->
            io:format("Reloading code ...~n"),
            frequency:init();
        { request, Pid, clear } ->
            clear(),
            Pid ! { ok, queue_cleared},
            loop(Freqs)
    after 20000  ->
          io:format("Waiting for client ...~n"),
          loop(Freqs)
    end.


%%%
%%% Help functions 
%%%

clear() ->
    receive
        _Msg -> clear() 
        after 0 ->
            io:format("Message queue cleared")
    end.

%% check_allocation
%% list() -> boolean()
is_allocated(P, Allocated, pid) ->
    length([ Pid || {_Fr, Pid} <- Allocated, Pid == P]) > 0;

is_allocated(Freq, Allocated, frequency) ->
    length([ Fr || {Fr, _Pid} <- Allocated, Fr == Freq]) > 0.

%% allocate frequency for client

% there is no freqvency to allocate
allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};

allocate({[Freq|Free], Allocated}, Pid) ->
    % io:format("FreeFrqs: ~p, Allocated: ~p~n",[Free, Allocated]),
    % check if client has allocated freq for himself ?
    case is_allocated(Pid, Allocated, pid) of
        true -> {{[Freq|Free], Allocated}, { forbidden, already_allocated}};
        false -> {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq }}
    end.


deallocate({Free, Allocated},Freq) ->
    case is_allocated(Freq, Allocated, frequency) of
        true -> 
            NewAllocated = lists:keydelete(Freq,1,Allocated),
            % io:format("FreeFrqs: ~p, Allocated: ~p~n",[Free, Allocated]),
            {[Freq|Free], NewAllocated};
        false ->
            % io:format("Could not deallocate frequency!~n~p~n~p",[Free,Allocated]),
            % {Free, Allocated}
            {error, not_deallocated}
    end.
    
