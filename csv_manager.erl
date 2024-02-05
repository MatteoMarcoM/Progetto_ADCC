% modulo per salvare le tabelle in files CSV
-module(csv_manager).

-export([
    to_csv/2,
    to_csv/3,
    from_csv/1,
    from_csv/2
]).

-record(spreadsheet, {table, riga, colonne}).

% Funzione per ottenere il contenuto della tabella Mnesia e scriverlo su un file CSV
% FileName := "nome_tabella.csv"
to_csv(TableName, FileName) ->
    % Controllo che table name sia nel mio mnesia
    mnesia:start(),
    TabelleLocali = mnesia:system_info(tables),
    case lists:member(TableName, TabelleLocali) of
        false -> {error, invalid_name_of_table};
        true ->
            % Apri il file per la scrittura
            {ok, File} = file:open(FileName, [write]),
            % Estrai i dati dalla tabella Mnesia
            % (una tabella Mnesia in RAM e' una ETS)
            Records = ets:tab2list(TableName),
            % Converti i dati in formato CSV
            CsvContent = records_to_csv(Records),
            % Scrivi il CSV nel file
            file:write(File, CsvContent),
            % Chiudi il file
            file:close(File)
    end
.

% Funzione per convertire i record in formato CSV
records_to_csv(Records) ->
    lists:map(fun({Foglio, Table, Riga, Colonna}) ->
            Row = lists:join(",", [atom_to_list(Foglio), integer_to_list(Table), integer_to_list(Riga), io_lib:format("~p", [Colonna])]),
            Row ++"\n"
        end, Records
    )
.


remove_extension(FileName) ->
    Reverse = string:reverse(FileName),
    SubString = string:sub_string(Reverse, 5, 100),
    Final = string:reverse(SubString),
    Final
.

from_csv(FilePath) ->
    SpreadsheetFields = record_info(fields, spreadsheet),
    TableName = remove_extension(FilePath),
    TabelleLocali = mnesia:system_info(tables),
    case lists:member(list_to_atom(TableName),TabelleLocali) of
        true -> {error, table_is_already_in_mnesia};
        false ->
            NodeList = [node()],
            mnesia:create_schema(NodeList),
            mnesia:create_table(list_to_atom(TableName), [
                {attributes, SpreadsheetFields},
                {disc_copies, NodeList},
                {type, bag}
            ]),
            {ok, File} = file:open(FilePath, [read]),
            read_lines(File),
            file:close(File)
    end
.

read_lines(File) ->
    case io:get_line(File, "") of
        eof -> ok;
        Line ->
            process_line(Line),
            read_lines(File)
    end
.

process_line(Line) ->
    % Rimuove il carattere di newline dalla fine della riga
    TrimmedLine = string:trim(Line),
    NewLine = "[" ++ TrimmedLine ++ "]",
    Parse = fun(S) -> 
        {ok, Ts, _} = erl_scan:string(S),
        {ok, Result} = erl_parse:parse_term(Ts ++ [{dot,1} || element(1, lists:last(Ts)) =/= dot]),
        Result 
    end,
    Prova = Parse(NewLine),
    [Col1|Tail1] = Prova,
    [Col2|Tail2] = Tail1,
    [Col3|Tail3] = Tail2,
    [Col4|_] = Tail3,
    F = fun() ->
        Data = {Col1,Col2,Col3,Col4},
        mnesia:write(Data)     
    end,
    Result = mnesia:transaction(F),
    case Result of
        {aborted, Reason} -> {error, Reason};
        {atomic, Res} -> Res
    end
.

% TIMEOUT VERSIONS
to_csv(TableName, FileName, Timeout) ->
    mia_flush(),
    MioPid = self(),
    % creo un processo timer
    spawn(fun() ->
        receive after Timeout -> MioPid!{timeout} end
    end),
    % creo un processo getter
    spawn(fun() ->
        MioPid!{result, to_csv(TableName, FileName)}
    end),
    ToReturn = receive
        {result, Res} -> Res;
        {timeout} ->
            % DEVO ASPETTARE COMUNQUE CHE IL FILE VENGA SCRITTO 
            % ALTRIMENTI C'E' RACE CONDITION !!!
            receive
                {result, _} -> ok
            end,
            % rimetto le cose come prima (cancello il file)
            Result = file:delete(FileName),
            case Result of
                {error, Reason} -> {error, Reason};
                ok -> timeout
            end
    after 10000 -> {error, no_message_received}
    end,
    mia_flush(),
    ToReturn
.

from_csv(FilePath, Timeout) ->
    mia_flush(),
    MioPid = self(),
    % creo un processo timer
    spawn(fun() ->
        receive after Timeout -> MioPid!{timeout} end
    end),
    % creo un processo getter
    spawn(fun() ->
        MioPid!{result, from_csv(FilePath)}
    end),
    ToReturn = receive
        {result, Res} -> Res;
        {timeout} -> 
            % DEVO ASPETTARE COMUNQUE CHE IL FILE VENGA CARICATO 
            % ALTRIMENTI C'E' RACE CONDITION !!!
            receive
                {result, _} -> ok
            end,
            % rimetto le cose come prima (cancello la tabella)
            TableName = remove_extension(FilePath),
            Result = mnesia:delete_table(list_to_atom(TableName)),
            case Result of
                {aborted, Reason} -> {error, Reason};
                {atomic, ok} -> timeout
            end
    after 10000 -> {error, no_message_received}
    end,
    mia_flush(),
    ToReturn
.

% implemento mia flush
mia_flush() ->
    receive
        % consuma il pattern e va in loop
        _AnyPattern -> mia_flush()
    after
        % se non ha messaggi nella coda 
        % restituisce subito (0 sec) ok
        0 -> ok
    end
.