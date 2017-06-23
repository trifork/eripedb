-module(eripedb_parser2).

%%% **********************************************************************
%%% * PURPOSE: Ripe Network database, for looking up ISP info given an   *
%%% * IP address.                                                        *
%%% **********************************************************************

-compile(export_all). % TODO
-export([]).

-type address_prefix() :: {Address::binary(), Bits::0..128}.
-type object_callback(X) :: fun((Class:: route|route6, Prefix::address_prefix(), Origin::binary()|undefined, X) -> X).

-define(BUFFER_SIZE, (1 bsl 15)). % 32KB

-record(pstate, {
          callback :: object_callback(_),
          file :: file:iodevice(),
          line_re :: re:mp(),
          extension_re :: re:mp(),
          newline_cp :: binary:cp()
         }).

-spec read/3 :: (iodata(), object_callback(FoldState), FoldState) -> {ok,FoldState} | {error,_}.
read(FileName, ObjectCallbackFun, FoldState) when is_function(ObjectCallbackFun, 4) ->
    case file:open(FileName, [binary, read, compressed]) of
        {error,_}=Err ->
            Err;
        {ok, File} ->
            {ok,LineRE} = re:compile("^(\\S+)\\s*:\\s*([^\r\n]*)\r?\n?$"),
            {ok,ExtensionRE} = re:compile("^(?:\\+|\\s)\\s*([^\r\n]*)\r?\n?$"),
            State = #pstate{callback=ObjectCallbackFun,
                            file=File,
                            line_re=LineRE,
                            extension_re=ExtensionRE,
                            newline_cp = binary:compile_pattern(<<"\n">>)
                            },
            try
                read_loop(State)
            after
                file:close(File)
            end
    end.

%% High level: Read large chunks, splitting at newlines;
%% Next level: Find "^origin:" | "^route:" | "^route6:"

%% State machine states: none --{has_header,Class,Name}--> |header| --reported--> skipping --separator--> none.

read_loop(State) ->
    read_lines(State, <<>>, 0, none).

read_lines(State, Buffer, Pos, SMState) ->
    Len = byte_size(Buffer) - Pos,
    case binary:match(Buffer, State#pstate.newline_cp, [{scope,{Pos,Len}}]) of
        nomatch ->
            %% Read some more.
            case file:read_line(State#pstate.file) of
                {error, _}=Err ->
                    Err;
                eof ->
                    <<_:Pos/binary, Line/binary>> = Buffer,
                    SMState2 = handle_line(State, Line, SMState),
                    SMState3 = handle_line(State, <<>>, SMState2),
                    SMState3;
                {ok, Data} ->
                    <<_:Pos/binary, Remaining/binary>> = Buffer,
                    read_lines(State, <<Remaining/binary, Data/binary>>, 0, SMState)
            end;
        {NewlinePos,_} ->
            <<_:Pos/binary, Line:NewlinePos/binary, _/binary>> = Buffer,
            SMState2 = handle_line(State, Line, SMState),
            read_lines(State, Buffer, NewlinePos+1, SMState2)
    end.

handle_line(State, <<"#", _/binary>>, SMState) ->
    %% Comment - ignore.
    SMState;
handle_line(State, <<"route:", Name/binary>>, none) ->
    {has_header, route, Name};
handle_line(State, <<"route6:", Name/binary>>, none) ->
    {has_header, route6, Name};
handle_line(State, <<"origin:", Origin/binary>>, {has_header, Class, Name}) ->
    %% Report!
    %% io:format("DB| ~p\n", [{Class, Name, Origin}]),
    skipping;
handle_line(State, <<"">>, {has_header, Class, Name}) ->
    %% Report with origin=undefined!
    %% %% io:format("DB| ~p\n", [{Class, Name, undefined}]),
    none;
handle_line(State, <<"">>, _) ->
    none;
handle_line(State, Line, SMState) ->
    case SMState of
        none ->
            %% Unexpected class.
            io:format("Unexpected object header: ~p\n", [Line]),
            skipping;
        _ ->
            %% Other field. Ignore.
            SMState
    end,
    SMState.


object_name([{<<"route">>, PrefixStr} | _]) ->
    {route, parse_address_prefix4(PrefixStr)};
object_name([{<<"route6">>, PrefixStr} | _]) ->
    {route6, parse_address_prefix6(PrefixStr)};
object_name([_ | _]) ->
    unknown_class.

%%% object_field(): Only return first matching attribute.
object_field(Name, Attrs) ->
    lists:keyfind(Name, 1, Attrs).


parse_address_prefix4(PrefixStr) ->
    {'TODO', PrefixStr}.

parse_address_prefix6(PrefixStr) ->
    {'TODO', PrefixStr}.
