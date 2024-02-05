% modulo di debug e testing
-module(debug).

-export([
    delete_close_tables/0,
    spawn_reader/2,
    spawn_writer/2,
    test_reading/1,
    test_writing/1,
    test_sequential/2,
    test_mixed/3,
    timing_ms/2,
    test_battery/1,
    average_lookup_time/2,
    average_setup_time/2
]).

delete_close_tables() ->
    mnesia:start(),
    TabelleLocali = mnesia:system_info(tables), 
    lists:foreach(fun(T) -> mnesia:delete_table(T) end, TabelleLocali),
    mnesia:delete_table(owner),
    mnesia:delete_table(policy),
    mnesia:delete_table(format),
    mnesia:stop()
.

loop_call_test_reader(Foglio,Pid) ->
    receive
        {start} ->
            Result = spreadsheet:get(Foglio, 1,1,1),
            Pid!{read_result, Result}, loop_call_test_reader(Foglio,Pid);
        {stop} -> Pid!{stop,node()}
    end
.

spawn_reader(Foglio, Pid) ->
    spawn(fun() ->
        loop_call_test_reader(Foglio,Pid)
    end)
.

loop_call_test_writer(Foglio, Pid) ->
    receive
        {start} ->
            Result = spreadsheet:set(Foglio, 1,1,1, 'WRITE'),
            Pid!{write_result, Result}, loop_call_test_writer(Foglio,Pid);
        {stop} -> Pid!{stop,node()}
    end
.

spawn_writer(Foglio, Pid) ->
    spawn(fun() ->
        loop_call_test_writer(Foglio, Pid)
    end)
.

test_reading(PidLettori) ->
    % faccio partire le letture
    lists:foreach(fun(Pid) -> Pid!{start} end, PidLettori),
    % aspetto i risultati
    lists:map(fun(_I) ->
            receive
                {read_result, Res} -> Res
            after 10000 -> {error}
            end
        end, 
        PidLettori
    )
.

% faccio terminare i nodi
stop_n(ListaPid) -> lists:foreach(fun(Pid) -> Pid!{stop} end, ListaPid).

spawn_n_reader(N, Foglio) ->
    % simulazione della creazione di nodi
    PidLettori = lists:map(fun(_I) -> 
            spawn_reader(Foglio, self())
        end, 
        lists:seq(1, N)
    ),
    % devo dare i permessi di lettura
    lists:foreach(fun(P) -> 
            spreadsheet:share(Foglio, {P, read}) 
        end, 
        PidLettori
    ), 
    PidLettori
.

test_writing(PidScrittori) ->
    % faccio partire le scritture
    lists:foreach(fun(Pid) -> Pid!{start} end, PidScrittori),
    % aspetto i risultati
    lists:map(fun(_I) ->
            receive
                {write_result, Res} -> Res
            after 10000 -> {error}
            end
        end, 
        PidScrittori
    )
.

spawn_n_writer(N, Foglio) ->
    % simulazione della creazione di nodi
    PidScrittori = lists:map(fun(_I) -> 
            spawn_writer(Foglio, self())
        end, 
        lists:seq(1, N)
    ),
    % devo dare i permessi in scrittura
    lists:foreach(fun(P) -> 
            spreadsheet:share(Foglio, {P, write}) 
        end, 
        PidScrittori
    ),
    PidScrittori
.

% N nodi -> 1 lettura e 1 scrittura
test_sequential(N, Foglio) ->
    % spawno i nodi
    PidLettori = spawn_n_reader(N, Foglio),
    PidScrittori = spawn_n_writer(N, Foglio),
    % faccio i test
    TestLetture = test_reading(PidLettori),
    TestScritture = test_writing(PidScrittori),
    % termino i nodi
    stop_n(PidLettori),
    stop_n(PidScrittori),
    {{readers, PidLettori, TestLetture},
     {writers, PidScrittori, TestScritture}}
.

% N nodi -> M letture e scritture
test_mixed(N, M, Foglio) ->
    % spawno i nodi
    PidLettori = spawn_n_reader(N, Foglio),
    PidScrittori = spawn_n_writer(N, Foglio),
    Tests = lists:map(fun(I) ->
            % faccio i test
            TestLetture = test_reading(PidLettori),
            TestScritture = test_writing(PidScrittori), 
            {{test, I}, TestLetture, TestScritture}
        end, 
        lists:seq(1, M)
    ),
    % termino i nodi
    stop_n(PidLettori),
    stop_n(PidScrittori),
    {{readers, PidLettori}, 
     {writers, PidScrittori},
     {results, Tests}}     
.

% MILLISECONDI !!!
timing_ms(TestName, Params) -> 
    {Time, _Value} = timer:tc(debug, TestName, Params, millisecond),
    Time
.

% MILLISECONDI !!!
test_battery(Foglio) ->
    ParamsSequential = [100, Foglio],
    TimeSequential = timing_ms(test_sequential, ParamsSequential),
    ParamsMixed = [100, 10, Foglio],
    TimeMixed = timing_ms(test_mixed, ParamsMixed),
    ParamsStress = [1000, 10, Foglio],
    TimeStress = timing_ms(test_mixed, ParamsStress),
    {{sequential_test, 
        {params, ParamsSequential}, {time_ms, TimeSequential}},
     {mixed_test, 
        {params, ParamsMixed}, {time_ms, TimeMixed}},
     {stress_mixed_test,
        {params, ParamsStress}, {time_ms, TimeStress}}
    }
.

% MICROSECONDI !!!
% suppongo di avere i permessi di lettura
% in modo che vada a buon fine
lookup_time(Foglio) ->
    TimeUnit = microsecond, 
    {Time, _Value} = timer:tc(spreadsheet, get, [Foglio, 1, 1, 1], TimeUnit),
    {TimeUnit, Time}
.

% media aritmetica
average_lookup_time(Foglio, N) -> 
    Times = lists:map(fun(_I) ->
            {_TimeUnit, Time} = lookup_time(Foglio),
            Time 
        end, 
        lists:seq(1, N)
    ),
    SumOfTimes = lists:sum(Times),
    Length = lists:foldl(fun(_X, Acc) -> Acc + 1 end, 0, Times),
    {microsecond, SumOfTimes / Length}
.

% MICROSECONDI !!!
% suppongo di avere i permessi di scrittura
% in modo che vada a buon fine
setup_time(Foglio) ->
    TimeUnit = microsecond, 
    {Time, _Value} = timer:tc(spreadsheet, set, [Foglio, 1, 1, 1, 'SETUP'], TimeUnit),
    {TimeUnit, Time}
.

% media aritmetica
average_setup_time(Foglio, N) -> 
    Times = lists:map(fun(_I) ->
            {_TimeUnit, Time} = setup_time(Foglio),
            Time 
        end, 
        lists:seq(1, N)
    ),
    SumOfTimes = lists:sum(Times),
    Length = lists:foldl(fun(_X, Acc) -> Acc + 1 end, 0, Times),
    {microsecond, SumOfTimes / Length}
.