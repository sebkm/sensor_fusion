-module(e8).

-behaviour(hera_measure).

-export([init/1, measure/1]).

-define(VAR_S, 0.0625). % (0.5/2)^2
-define(VAR_A, 0.8).
-define(VAR_G, 0.005).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
    Spec = #{
        name => ?MODULE,
        iter => 1000,
        timeout => 10
    },
    X = [[0], [0], [0], [0.5], [1]],
    P = mat:diag([2,2,math:pi(),1,1]),
    State = {hera:timestamp(), X, P},
    {ok, State, Spec}.


measure({T0, X0, P0}) ->
    DataNav = hera_data:get(nav, sensor_fusion@nav_1),
    DataSonars = hera_data:get(sonar),
    T1 = hera:timestamp(),
    Nav = [Data || {_,_,Ts,Data} <- DataNav, T0 < Ts, T1-Ts < 500],
    Sonars = [Data || {_,_,Ts,Data} <- DataSonars, T0 < Ts, T1-Ts < 500],
    if
        length(Nav) + length(Sonars) == 0 -> % no measure
            {undefined, {T0, X0, P0}};
        true ->
            Dt = (T1 - T0)/1000,
            F = fun([_, _, [O], [W], [Radius]]) -> [
                [Radius*math:cos(O)],
                [Radius*math:sin(O)],
                [O+W*Dt],
                [W],
                [Radius]
            ] end,
            Jf = fun([_, _, [O], _, [Radius]]) -> [
                [0,0,-Radius*math:sin(O),0,math:cos(O)],
                [0,0,Radius*math:cos(O),0,math:sin(O)],
                [0,0,1,Dt,0],
                [0,0,0,1,0],
                [0,0,0,0,1]
            ] end,
            Q = mat:zeros(5,5),
            
            H = fun([[X], [Y], _, [W], [Radius]]) ->
                [[Radius*W*W] || _ <- Nav] ++
                [[W] || _ <- Nav] ++
                [[sonar_range({X,Y}, {Px,Py})] || [_,Px,Py] <- Sonars]
            end,
            Jh = fun([[X], [Y], _, [W], [Radius]]) ->
                [[0,0,0,2*Radius*W,W*W] || _ <- Nav] ++
                [[0,0,0,1,0] || _ <- Nav] ++
                [[dhdx({X,Y}, {Px,Py}), dhdx({Y,X}, {Py,Px}), 0, 0, 0]
                    || [_,Px,Py] <- Sonars]
            end,
            Z = [[-Ay] || [Ay,_] <- Nav] ++
                [[-Gz] || [_,Gz] <- Nav] ++
                [[Range] || [Range,_,_] <- Sonars],
            R = mat:diag(
                [?VAR_A || _ <- Nav] ++
                [?VAR_G || _ <- Nav] ++
                [?VAR_S || _ <- Sonars]
            ),

            {X1,P1} = kalman:ekf({X0,P0}, {F,Jf}, {H,Jh}, Q, R, Z),
            Values = lists:append(X1),
            {ok, Values, {T1, X1, P1}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sonar_range({X,Y}, {Px,Py}) ->
    math:sqrt(math:pow(X-Px,2) + math:pow(Y-Py,2)).


dhdx({X,Y}, {Px,Py}) ->
    R = sonar_range({X,Y}, {Px,Py}),
    (X-Px) * math:sqrt(R) / R.
