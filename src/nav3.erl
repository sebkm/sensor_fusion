-module(nav3).

-behaviour(hera_measure).

-export([calibrate/0]).
-export([init/1, measure/1]).

-record(cal, {gyro, mag}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calibrate() ->
    io:format("Place de pmod_nav flat and still!~n"),
    [Gx,Gy,Gz] = calibrate(acc, [out_x_g,out_y_g,out_z_g], 300),
    [Mx1,My1,Mz1] = calibrate(mag, [out_x_m, out_y_m, out_z_m], 10),
    _ = io:get_line("Turn the pmod_nav 180° around the z axis then press enter"),
    [Mx2,My2,_] = calibrate(mag, [out_x_m, out_y_m, out_z_m], 10),
    _ = io:get_line("Turn the pmod_nav 180° around the x axis then press enter"),
    [_,_,Mz2] = calibrate(mag, [out_x_m, out_y_m, out_z_m], 10),
    BiasX = 0.5*(Mx1+Mx2),
    BiasY = 0.5*(My1+My2),
    BiasZ = 0.5*(Mz1+Mz2),
    #cal{gyro={Gx,Gy,Gz}, mag={BiasX,BiasY,BiasZ}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(C) ->
    Spec = #{
        name => ?MODULE,
        iter => infinity,
        timeout => 100
    },
    {ok, C, Spec}.


measure(C=#cal{gyro={GBx,GBy,GBz}, mag={MBx,MBy,MBz}}) ->
    [Ax,Ay,Az, Gx,Gy,Gz] = pmod_nav:read(acc, [
        out_x_xl,out_y_xl,out_z_xl,
        out_x_g,out_y_g,out_z_g]),
    Acc = [Ax,Ay,-Az],
    Gyro = [Gx-GBx,Gy-GBy,-(Gz-GBz)],
    [Mx,My,Mz] = pmod_nav:read(mag, [out_x_m, out_y_m, out_z_m]),
    Data = lists:append([
        scale(Acc, 9.81),
        scale(Gyro, math:pi()/180),
        [-(Mx-MBx),My-MBy,-(Mz-MBz)]
    ]),
    {ok, Data, C}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scale(List, Factor) ->
    [X*Factor || X <- List].


calibrate(Comp, Registers, N) ->
    Data = [list_to_tuple(pmod_nav:read(Comp, Registers))
        || _ <- lists:seq(1,N)],
    {X, Y, Z} = lists:unzip3(Data),
    [lists:sum(X)/N, lists:sum(Y)/N, lists:sum(Z)/N].
