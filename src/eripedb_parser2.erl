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
                read_loop(State, FoldState)
            after
                file:close(File)
            end
    end.

%% High level: Read large chunks, splitting at newlines;
%% Next level: Find "^origin:" | "^route:" | "^route6:"

%% State machine states: none --{has_header,Class,Name}--> |header| --reported--> skipping --separator--> none.

read_loop(State, FoldState) ->
    read_lines(State, <<>>, 0, none, FoldState).

read_lines(State, Buffer, Pos, SMState, FoldState) ->
    Len = byte_size(Buffer) - Pos,
    case binary:match(Buffer, State#pstate.newline_cp, [{scope,{Pos,Len}}]) of
        nomatch ->
            %% Read some more.
            case file:read(State#pstate.file, ?BUFFER_SIZE) of
                {error, _}=Err ->
                    Err;
                eof ->
                    <<_:Pos/binary, Line/binary>> = Buffer,
                    {SMState2, FS2} = handle_line1(State, Line, SMState, FoldState),
                    {_SMState3, FS3} = handle_line1(State, <<>>, SMState2, FS2),
                    {ok, FS3};
                {ok, Data} ->
                    <<_:Pos/binary, Remaining/binary>> = Buffer,
                    read_lines(State, <<Remaining/binary, Data/binary>>, 0, SMState, FoldState)
            end;
        {NewlinePos,_} ->
            LineLen = (NewlinePos-Pos),
            <<_:Pos/binary, Line:LineLen/binary, _/binary>> = Buffer,
            {SMState2,FS2} = handle_line1(State, Line, SMState, FoldState),
            read_lines(State, Buffer, NewlinePos+1, SMState2, FS2)
    end.

handle_line1(State, Line, SMState, FoldState) ->
    case handle_line(Line, SMState) of
        {report, Class, Name, Origin, NewSMState} ->
            Callback=State#pstate.callback,
            Foldstate2 = Callback(Class, Name, Origin, FoldState),
            {NewSMState, Foldstate2};
        SMState2 ->
            {SMState2, FoldState}
    end.

handle_line(<<"#", _/binary>>, SMState) ->
    %% Comment - ignore.
    SMState;
handle_line(<<"route:", PrefixStr/binary>>, none) ->
    Prefix = parse_address_prefix4(PrefixStr),
    {has_header, route, Prefix};
handle_line(<<"route6:", PrefixStr/binary>>, none) ->
    Prefix = parse_address_prefix6(PrefixStr),
    {has_header, route6, Prefix};
handle_line(<<"origin:", Origin0/binary>>, {has_header, Class, Name}) ->
    Origin = trim_leading_spaces(Origin0),
    %% Report!
    %% io:format("DB| ~p\n", [{Class, Name, Origin}]),
    {report, Class, Name, Origin, skipping};
handle_line(<<"">>, {has_header, Class, Name}) ->
    %% Report with origin=undefined!
    %% %% io:format("DB| ~p\n", [{Class, Name, undefined}]),
    {report, Class, Name, undefined, none};
handle_line(<<"">>, _) ->
    none;
handle_line(Line, SMState) ->
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
    [IPStr,LengthStr] = binary:split(trim_leading_spaces(PrefixStr),<<"/">>),
    {ok, {A,B,C,D}} = inet:parse_ipv4_address(binary_to_list(IPStr)),
    Length = binary_to_integer(LengthStr),
    {ipv4, <<A,B,C,D>>, Length}.

parse_address_prefix6(PrefixStr) ->
    [IPStr,LengthStr] = binary:split(trim_leading_spaces(PrefixStr),<<"/">>),
    {ok, {A,B,C,D,E,F,G,H}} = inet:parse_ipv6_address(binary_to_list(IPStr)),
    Length = binary_to_integer(LengthStr),
    {ipv6, <<A,B,C,D,E,F,G,H>>, Length}.

trim_leading_spaces(<<" ", Rest/binary>>) ->
    trim_leading_spaces(Rest);
trim_leading_spaces(<<"\t", Rest/binary>>) ->
    trim_leading_spaces(Rest);
trim_leading_spaces(Bin) ->
    Bin.
