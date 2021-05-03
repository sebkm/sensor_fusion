-module(mag).

-behaviour(hera_measure).

-export([calibrate/0]).
-export([init/1, measure/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calibrate() ->
    [Mx1,My1,_] = calibrate(mag, [out_x_m, out_y_m, out_z_m], 10),
    _ = io:get_line("Turn the pmod_nav 180° around the z axis then press enter"),
    [Mx2,My2,_] = calibrate(mag, [out_x_m, out_y_m, out_z_m], 10),
    MxBias = 0.5*(Mx1+Mx2),
    MyBias = 0.5*(My1+My2),
    _ = io:get_line("Place the pmod_nav at 0° then press enter"),
    Offset = mag_measure({MxBias, MyBias, 0}),
    {MxBias,MyBias,Offset}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Calibration) ->
    Spec = #{
        name => ?MODULE,
        iter => 500,
        timeout => 300
    },
    {ok, Calibration, Spec}.


measure(C) ->
    {ok, [mag_measure(C)], C}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calibrate(Comp, Registers, N) ->
    Data = [list_to_tuple(pmod_nav:read(Comp, Registers))
        || _ <- lists:seq(1,N)],
    {X, Y, Z} = lists:unzip3(Data),
    [lists:sum(X)/N, lists:sum(Y)/N, lists:sum(Z)/N].


mag_measure({MxBias, MyBias, Offset}) ->
    [Mx, My] = pmod_nav:read(mag, [out_x_m, out_y_m]),
    RealMx = Mx - MxBias, 
    RealMy = My - MyBias,
    O = math:atan2(RealMy,RealMx),
    squash(O - Offset).

squash(O) ->
    sign(O) * (math:fmod(abs(O) + math:pi(), 2*math:pi()) - math:pi()).

sign(O) when O >= 0 -> 1;
sign(_) -> -1.
