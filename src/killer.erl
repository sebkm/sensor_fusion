-module(killer).

-export([launch/2]).
-export([hera_measure/2, hera_data/2, hera_com/2]).
-export([hera_sync_measure/1, hera_sub/1, hera_sync/1]).
-export([hera_synchronization/0]).


launch(Name, Args) ->
    rpc:multicall(?MODULE, Name, Args).


% starts N measure_counter and kills M of them after 5 [s]
hera_measure(N, M) when M =< 10 andalso M =< N ->
    Seq = lists:seq(1, N),
    Names = [list_to_atom("hera_measure_" ++ integer_to_list(Id)) || Id <- Seq],
    Starter = fun(Id) ->
        {ok, _} = hera:start_measure(measure_counter, {Id,Names,false, 1000})
    end,
    lists:foreach(Starter, Seq),
    timer:sleep(5000),
    Pids = lists:sublist(supervisor:which_children(hera_measure_sup), M),
    Killer = fun({_,Pid,_,_}) -> exit(Pid, killed) end,
    lists:foreach(Killer, Pids),
    timer:sleep(10000),
    sensor_fusion:stop_all().


% starts N measure_counter and kills hera_data after 5 [s] M times
% with 1 [s] between each kill
hera_data(N, M) when M =< 6 ->
    Seq = lists:seq(1, N),
    Names = [list_to_atom("hera_data_" ++ integer_to_list(Id)) || Id <- Seq],
    Starter = fun(Id) ->
        {ok, _} = hera:start_measure(measure_counter, {Id,Names,false, 1000})
    end,
    lists:foreach(Starter, Seq),
    timer:sleep(5000),
    Killer = fun(_) ->
        exit(whereis(hera_data), killed),
        timer:sleep(1000)
    end,
    lists:foreach(Killer, lists:seq(1, M)),
    timer:sleep(10000),
    sensor_fusion:stop_all().


% starts N measure_counter and kills hera_com M times after 1.5 [s]
% with 100 [ms] between each kill
hera_com(N, M) when M =< 6 ->
    Seq = lists:seq(1, N),
    Names = [list_to_atom("hera_com_" ++ integer_to_list(Id)) || Id <- Seq],
    Starter = fun(Id) ->
        {ok, _} = hera:start_measure(measure_counter, {Id,Names,false, 300})
    end,
    lists:foreach(Starter, Seq),
    timer:sleep(1500),
    Killer = fun(_) ->
        exit(whereis(hera_com), killed),
        timer:sleep(100)
    end,
    lists:foreach(Killer, lists:seq(1, M)),
    timer:sleep(5000),
    sensor_fusion:stop_all().


% starts a measure_counter and kill it after 5 [s] if node() is in Nodes
hera_sync_measure(Nodes) ->
    Names = [hera_sync_measure],
    {ok, Pid} = hera:start_measure(measure_counter, {1,Names,true,100}),
    case lists:member(node(), Nodes) of
        true ->
            timer:sleep(5000),
            exit(Pid, killed),
            timer:sleep(5000),
            sensor_fusion:stop_all();
        false ->
            ok
    end.


% starts N measure_counter and kill hera_sub after 5 [s]
hera_sub(N) ->
    Seq = lists:seq(1, N),
    Names = [list_to_atom("hera_sub_" ++ integer_to_list(Id)) || Id <- Seq],
    Starter = fun(Id) ->
        {ok, _} = hera:start_measure(measure_counter, {Id,Names,true, 100})
    end,
    lists:foreach(Starter, Seq),
    if
        node() == sensor_fusion@sebastien ->
            timer:sleep(5000),
            exit(global:whereis_name(hera_sub), killed),
            timer:sleep(5000),
            sensor_fusion:stop_all();
        true ->
            ok
    end.
    

% starts N measure_counter and kill each hera_sync after 5 [s]
hera_sync(N) ->
    Seq = lists:seq(1, N),
    Names = [list_to_atom("hera_sync_" ++ integer_to_list(Id)) || Id <- Seq],
    Starter = fun(Id) ->
        {ok, _} = hera:start_measure(measure_counter, {Id,Names,true, 100})
    end,
    lists:foreach(Starter, Seq),
    if
        node() == sensor_fusion@sebastien ->
            Killer = fun(Name) ->
                {ok, Pid} = hera_sub:subscribe(Name),
                exit(Pid, killed)
            end,
            timer:sleep(5000),
            lists:foreach(Killer, Names),
            timer:sleep(5000),
            sensor_fusion:stop_all();
        true ->
            ok
    end.


% starts a measure_counter and tell where hera_synchronization is running
hera_synchronization() ->
    Names = [hera_synchronization],
    {ok, _} = hera:start_measure(measure_counter, {1,Names,true, 500}),
    Pred = fun({App,_,_}) -> App == hera_synchronization end,
    case lists:search(Pred, application:which_applications()) of
        false -> ok;
        {value, _} -> node()
    end.

        
