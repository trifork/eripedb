-module(eripedb_parser).

%%% **********************************************************************
%%% * PURPOSE: Ripe Network database, for looking up ISP info given an   *
%%% * IP address.                                                        *
%%% **********************************************************************

-compile(export_all). % TODO
-export([]).

-type address_prefix() :: {Address::binary(), Bits::0..128}.
-type object_callback(X) :: fun((Class:: route|route6, Prefix::address_prefix(), Origin::binary()|undefined, X) -> X).

-define(BUFFER_SIZE, 8192).

-record(pstate, {
          callback :: object_callback(_),
          file :: file:iodevice(),
          line_re :: re:mp(),
          extension_re :: re:mp()
         }).

-spec read/3 :: (iodata(), object_callback(FoldState), FoldState) -> {ok,FoldState} | {error,_}.
read(FileName, ObjectCallbackFun, InitFoldState) when is_function(ObjectCallbackFun, 4) ->
    case file:open(FileName, [binary, read, compressed]) of
        {error,_}=Err ->
            Err;
        {ok, File} ->
            {ok,LineRE} = re:compile("^(\\S+)\\s*:\\s*([^\r\n]*)\r?\n?$"),
            {ok,ExtensionRE} = re:compile("^(?:\\+|\\s)\\s*([^\r\n]*)\r?\n?$"),
            State = #pstate{callback=ObjectCallbackFun,
                            file=File,
                            line_re=LineRE,
                            extension_re=ExtensionRE
                            },
            try
                read_loop(State, InitFoldState)
            after
                file:close(File)
            end
    end.

read_loop(State, FoldState) ->
    case read_object(State) of
        {ok, Attrs} ->
            case object_name(Attrs) of
                unknown_class ->
                    FoldState2 = FoldState,
                    ok; % Ignore
                {Class, Prefix} ->
                    ObjectCallbackFun = State#pstate.callback,
                    case object_field(<<"origin">>, tl(Attrs)) of
                        {_, Origin} ->
                            ok;
                        false ->
                            Origin=undefined
                    end,
                    FoldState2 = ObjectCallbackFun(Class, Prefix, Origin, FoldState)
            end,
            read_loop(State, FoldState2);
        eof ->
            FoldState
    end.

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

read_object(State) ->
    read_object(State, []).

read_object(State, Attrs) ->
    case read_line(State) of
        {error, _}=Err ->
            Err;
        eof when Attrs==[] ->
            eof;
        eof ->
            %% Return the object.
            {ok, lists:reverse(Attrs)};
        blank_line when Attrs==[] ->
            %% Skip leading seperator.
            read_object(State);
        blank_line ->
            %% Return the object.
            {ok, lists:reverse(Attrs)};
        {ok, Key, Value} ->
            %% io:format("DB| Data: ~p\n", [{Key, Value}]),
            read_object(State, [{Key,Value} | Attrs]);
        {extension, Ext} ->
            [{Key, Value} | Rest] = Attrs,
            Value2 = <<Value/binary, "\n", Ext/binary>>,
            read_object(State, [{Key, Value2} | Rest])
    end.


read_line(State=#pstate{file=File}) ->
    case file:read_line(File) of
        {error, _}=Err ->
            Err;
        eof ->
            eof;
        {ok, <<"#", _/binary>>} ->
            %% Comment - ignore
            read_line(State);
        {ok, <<"\n">>} ->
            blank_line;
        {ok, <<"\r\n">>} ->
            blank_line;
        {ok, Line = <<C, _/binary>>} when C==$o; C==$r ->
            LineRE = State#pstate.line_re,
            case re:run(Line, LineRE, [{capture, all_but_first, binary}]) of
                {match, [Attribute,Value]} ->
                    {ok, Attribute, Value};
                nomatch ->
                    %% case re:run(Line, State#pstate.extension_re, [{capture, all_but_first, binary}]) of
                    %%     {match, [Extension]} ->
                    %%         {extension, Extension};
                    %%     nomatch ->
                            io:format("DB| Bad line: ~p\n", [Line]),
                            {error, {bad_line, Line}}
                    %% end
            end;
        {ok, _Line} ->
            %% Ignore other attributes!
            read_line(State)
    end.
