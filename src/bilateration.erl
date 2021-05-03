-module(bilateration).

-behaviour(hera_measure).

-export([init/1, measure/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
    Spec = #{
        name => ?MODULE,
        iter => 500,
        timeout => 200
    },
    {ok, undefined, Spec}.


measure(State) ->
    L = hera_data:get(sonar), % [{_Node, _Seq, T, [Range,X,Y]}, ...]
    Now = hera:timestamp(),
    RecentData = lists:filter(fun({_,_,T,_}) -> Now-T < 1000 end, L),
    if
        length(RecentData) =/= 2 ->
            {undefined, State};
        true ->
            [T1, T2] = [
                list_to_tuple(element(4, Measure))
                || Measure <- RecentData
            ],
            try bilateration(T1, T2) of
                {{X,Y}, _P2} -> {ok, [X,Y], State}
            catch
                error:_ ->
                    {undefined, State}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bilateration({R1, X1, Y}, {R2, X2, Y}) ->
    U = X2-X1,
    Helper1 = math:pow(R1, 2) - math:pow(R2, 2) + math:pow(U, 2),
    TargetX = Helper1/(2*U),
    TargetY = math:sqrt(math:pow(R1, 2)
        - (math:pow(Helper1, 2)/(4*math:pow(U, 2)))),
    TargetY2 = -TargetY,
    {{TargetX+X1,TargetY+Y}, {TargetX+X1,TargetY2+Y}}.
