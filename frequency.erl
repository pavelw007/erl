%% 
%% Frequency server
%% hardened version with linked client
%%
-module(frequency).
-author("pavel.west@gmail.com").

-export([start/1, allocate/1, deallocate/1, stop/1]).
-export([init/0]).
% for testing purposes only
% -compile(export_all).

-ifdef(debug).
-define(TRACE(Msg), io:fwrite("TRACE: ~p/~p(~p):~p MSG: ~p~n",[?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY,?LINE,Msg])).
-else.
-define(TRACE(Msg), true).
-endif.
%% 
%% functional interface
%%

%% start server
start(Id) ->
    ?TRACE("Starting frequency server ..."),
    Pid = spawn_link(?MODULE,init,[]),
    Ret = register(list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Id)), Pid),
    % io:format("{Ret, Pid}= {~p, ~p}~n",[Ret, Pid]),
    {Ret, Pid}.

%% alocate frequnecy
allocate({Pid, Timeout}) ->
    % io:format("Pid: ~p, timeout: ~p~n",[Pid, Timeout]),
    frequency ! { request, Pid, allocate},
    receive
        {reply, Msg } ->
            % io:format("Response: ~p~n",[Msg]),
            Pid ! Msg
        after Timeout ->
            % io:format("Client timeout in allocation after ~p~n",[Timeout]),
            Pid ! timeout
    end.

%% deallocate frequency    
deallocate({Pid, Timeout, Freq}) ->
    frequency ! { request, Pid, {deallocate, Freq}},
    receive
        {reply, Msg } ->
            % io:format("Response: ~p~n",[Msg]),
            Pid ! Msg
        after Timeout ->
            % io:format("Client timeout in deallocation after ~p~n",[Timeout]),
            Pid ! timeout
    end.

%% stop server 
stop(Timeout) ->
    frequency ! { request, self(), stop},
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
    process_flag(trap_exit,true),
    Frequencies = {gen_frequencies(),[]},
    loop(Frequencies).

%% server's main loop
loop(Freqs) ->
    receive
        % stop server
        {request, Pid, stop} ->
            ?TRACE("Server stopped"),
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
        % code reloading ( test version )
        {request, _Pid, code_reload } ->
            ?TRACE("Code compliling ...."),
            compile:file(?MODULE,[{d,debug}]),
            ?TRACE("Reloading code ..."),
            ?TRACE("Code reloaded!"),
            frequency:init();
        { request, Pid, clear } ->
            clear(),
            Pid ! { ok, queue_cleared},
            loop(Freqs);
        {'EXIT', Pid, killed} ->
            ?TRACE("Supervisor was killed"),
            io:format("Received from Pid: ~p~n",[Pid]),
            self() ! { stop, 200 };
        % handle died client allocation
        {'EXIT', Pid, Reason } ->
            ?TRACE("Frequency server got kill signal !!!"),
            io:format("Got killed signal from pid: ~p~n",[Pid]),
            io:format("REASON: ~p~n",[Reason]),
            NewFreqs = exited(Freqs, Pid),
            loop(NewFreqs)
    after 20000  ->
          ?TRACE("Waiting for client ..."),
          % io:format("~p: Freqs: ~p~n",[self(), Freqs]),
          loop(Freqs)
    end.


%%%
%%% Help functions 
%%%

clear() ->
    receive
        _Msg -> clear() 
        after 0 ->
            ?TRACE("Message queue cleared")
    end.

%% check_allocation
%% list() -> boolean()
is_allocated(_,[],_) ->
    false;

is_allocated(P, Allocated, pid) ->
    ?TRACE(Allocated),
    ?TRACE(P),
    length([ Pid || {_Fr, Pid} <- Allocated, Pid == P]) > 0;

is_allocated(Freq, Allocated, frequency) ->
    ?TRACE(Allocated),
    ?TRACE(Freq),
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
        false -> 
            link(Pid),
            io:format("Process ~p linked~n",[Pid]),
            {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq }}
    end.


deallocate({Free, Allocated},Freq) ->
    case is_allocated(Freq, Allocated, frequency) of
        true -> 
            {value, {Freq, Pid}} = lists:keysearch(Freq,1,Allocated),
            io:format("Unlink Pid: ~p~n",[Pid]),
            unlink(Pid),
            NewAllocated = lists:keydelete(Freq,1,Allocated),
            % io:format("FreeFrqs: ~p, Allocated: ~p~n",[Free, Allocated]),
            {[Freq|Free], NewAllocated};
        false ->
            % io:format("Could not deallocate frequency!~n~p~n~p",[Free,Allocated]),
            {Free, Allocated}
            % {error, not_deallocated}
    end.

%% client died !
exited({Free,Allocated}, Pid ) ->
    case lists:keysearch(Pid,2,Allocated) of
        {value, {Freq, Pid}} ->
            unlink(Pid),
            NewAllocated = lists:keydelete(Freq, 1, Allocated),
            {[Freq|Free],NewAllocated};
        false ->
            {Free, Allocated}
    end.
    
