-module(nav).

-behaviour(hera_measure).

-export([calibrate/0]).
-export([init/1, measure/1]).

-record(cal, {acc, gyro}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calibrate() ->
    io:format("Calibrating... Do not move the pmod_nav!~n"),
    [Ax,Ay,Az] = calibrate(acc, [out_x_xl, out_y_xl, out_z_xl], 500),
    [Gx,Gy,Gz] = calibrate(acc, [out_x_g, out_y_g, out_z_g], 500),
    #cal{acc=[Ax,Ay,-Az], gyro=[Gx,Gy,-Gz]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Calibration) ->
    Spec = #{
        name => ?MODULE,
        iter => 500
    },
    {ok, Calibration, Spec}.


measure(C) ->
    [Ax,Ay,Az] = pmod_nav:read(acc, [out_x_xl, out_y_xl, out_z_xl]),
    [Gx,Gy,Gz] = pmod_nav:read(acc, [out_x_g, out_y_g, out_z_g]),
    {A,G} = correctRotationY([Ax,Ay,-Az], [Gx,Gy,-Gz], C#cal.acc, C#cal.gyro),
    {ok, [lists:nth(2,A)*9.81, lists:nth(3,G)*math:pi()/180], C}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calibrate(Comp, Registers, N) ->
    Data = [list_to_tuple(pmod_nav:read(Comp, Registers))
        || _ <- lists:seq(1,N)],
    {X, Y, Z} = lists:unzip3(Data),
    [lists:sum(X)/N, lists:sum(Y)/N, lists:sum(Z)/N].


correctRotationY(A, G, A0=[Ax0,_,_], G0) ->
    Sin = -Ax0 / 1, % assuming gravity = 1
    NewA = rotateY(subtract(A, A0), Sin),
    NewG = rotateY(subtract(G, G0), Sin),
    {NewA, NewG}.


subtract([X,Y,Z], [X0,Y0,Z0]) ->
    [X-X0, Y-Y0, Z-Z0].


rotateY([X,Y,Z], Sin) ->
    Cos = math:sqrt(1 - math:pow(Sin, 2)),
    [Cos*X + Sin*Z, Y, -Sin*X + Cos*Z].
