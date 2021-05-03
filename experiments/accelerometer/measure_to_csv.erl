-module(measure_to_csv).

-export([nav_to_csv/1]).

lines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).

nav_read_line(<<>>) ->
    <<>>;
nav_read_line(Line) ->
    Packed = binary:replace(Line, [<<"{">>, <<"}">>, <<" ">>], <<"">>, [global]),
    Values = binary:split(Packed, [<<",">>], [global]),
    [_, P, V, A, T] = Values,
    {P, V, A, T}.

nav_write_line(<<>>, _) ->
    ok;
nav_write_line({P, V, A, T}, IoDevice) ->
    C = <<",">>,
    Line = [P, C, V, C, A, C, T, <<"\n">>],
    file:write(IoDevice, Line).

nav_to_csv(Name) ->
    Lines = lines(Name ++ ".1"),
    PVAs = [nav_read_line(Line) || Line <- Lines],
    {ok, IoDevice} = file:open(Name ++ ".csv", write),
    [nav_write_line(PVA, IoDevice) || PVA <- PVAs],
    file:close(IoDevice).

