%%% --------------------------------------------------------------------------------
%%% Supervisor module for Frequency server
%%% compile: c(server_supervisor). - normal
%%%          c(server_supervisor,{d,debug}). - compile for debug
%%% usage: server_supervisor:run_app(NumberOfWorkers). -  start workers
%%%        svisor ! { stop, self() }. - stop all workers and supervisor server
%%%                                   - atom 'svisor' = registered name of supervisor
%%%        server_supervisor:random_kill(Number, NumberOfWorkers). - randomly kill workers
%%% --------------------------------------------------------------------------------
-module(server_supervisor).
-author("pavel.west@gmail.com").
-export([run_app/1, start_link/2, init/1]).
-export([random_kill/2]).
% -compile(export_all).

%%% debug macros
-define(SRV_TIMEOUT, 20000).
-ifdef(debug).
-define(NONE, "\33[0m").
-define(TRACE(Msg), io:fwrite("\33[1;31mTRACE: \33[0m~p/~p(~p):~p \33[1;33m~p\33[0m~n",[?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY,?LINE,Msg])).
-else.
-define(TRACE(Msg), ok).
-endif.

%%% 
%%% --------------------------------------------------------------------------
%%% Supervisor functions 
%%% --------------------------------------------------------------------------

%% Start appropriate numbes of workers
%% helper function
generate_start_seq(Number) ->
    generate_start_seq(Number, []).

generate_start_seq(0,Args) ->
    Args;

generate_start_seq(Number, Args) ->
    Name = list_to_atom("s" ++ integer_to_list(Number)),
    Param = {frequency, start,[Name] },
    [Param| generate_start_seq(Number-1, Args)].

%% start application
%% run_app(Num) -> atom()
%% Num - number of workers to start
run_app(Num) ->
    ?TRACE("Starting ..."),
    % start_link(svisor,[{frequency, start, [s1]},{frequency, start, [s2]}]),
    start_link(svisor,generate_start_seq(Num)),
    ?TRACE("Started").

start_link(Name, ServList) ->
    register(Name, spawn_link(?MODULE, init, [ServList])), 
    ok.

% init all workers/servers
init(ServList) ->
    process_flag(trap_exit, true),
    loop(start_server(ServList)).

%% start workers from list of arguments
start_server([]) ->
    [];
start_server([{Mod, Func, Args} | ServList]) ->
    ?TRACE("Starting server ..."),
    case (catch apply(Mod, Func, Args)) of
        {true, Pid} ->
            % io:format("Pid: ~p~n",[Pid]),
            [{Pid, {Mod, Func, Args}} | start_server(ServList)];
        _ ->
            ?TRACE("No {true, Pid} found"),
            start_server(ServList)
    end.

%% terminate all workers
terminate([]) -> [];
terminate([{Pid,_} | ServList]) ->
    exit(Pid, kill),
    terminate(ServList).

%% main loop of Supervisor server
loop(ServList) ->
    receive
        { stop, From } ->
            ?TRACE("Recieved stop signal, stoping all workers ..."),
            From ! { reply, terminate(ServList)},
            ?TRACE("Supervisor terminated.");
        info ->
            io:format("WrkList: ~p~n",[ServList]),
            loop(ServList);

        {'EXIT', Pid, _Reason } ->
            ?TRACE("Worker process was KILLED !!!"),
            % io:format("From pid: ~p -> ~p~n",[Pid, ServList]),
            % process died, start it again !
            case lists:keyfind(Pid, 1, ServList) of 
                % {Pid, {Mod, Func, Args}} = lists:keyfind(Pid,1,ServList),
                {Pid,{Mod,Func,Args}} -> 
                                % io:format("Pid: ~p, Mod: ~p, Func: ~p, Args: ~p~n",[Pid,Mod,Func, Args]),
                                {true, NewPid } = apply(Mod, Func, Args), 
                                NewServList = [{NewPid, {Mod, Func, Args}} | lists:keydelete(Pid,1, ServList)],
                                % io:format("Newlist = ~p~n",[NewServList]),
                                loop(NewServList);
                _ -> ?TRACE("Pid not found")
            end 
        after ?SRV_TIMEOUT ->
                  % once pre SRV_TIMEOUT show the status of server 
                  io:format("***** Supervisor server alive tick *****~n"),
                  io:format("~p: Supervisor: ~p~n~n",[self(), ServList]),
                  loop(ServList)
    end.

            
%%%
%%% test killing workers, randomly
%%% Num - number() - number of workers to kill
%%% TotalNumberOfWorkers - number() - number of all workers ( should be same as number in run_app() )
%%%
random_kill(0, _TotalNumberOfWorkers) ->
    finished;
random_kill(Num, TotalNumberOfWorkers) ->
    % io:format("Num: ~p, Total: ~p~n",[Num, TotalNumberOfWorkers]),
    NumProc = rand:uniform(TotalNumberOfWorkers),
    ProcName = list_to_atom("frequency_s" ++ integer_to_list(NumProc)),
    % io:format("ProcName = ~p~n",[ProcName]),
    exit(whereis(ProcName), kill),
    timer:sleep(1000 * rand:uniform(5)),
    random_kill(Num-1, TotalNumberOfWorkers).

