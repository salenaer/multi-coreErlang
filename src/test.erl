-module(test).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, typical_session_1/1, typical_session_2/1, generate_channels/2]).

initialize_test() ->
    catch unregister(server_handle),
    server_handle:create_server_handle().

register_user_test() ->
    initialize_test(),
    ?assertMatch({_, user_registered}, server:register_user(server_handle, "A")),
    ?assertMatch({_, user_registered}, server:register_user(server_handle, "B")),
    ?assertMatch({_, user_registered}, server:register_user(server_handle, "C")),
    ?assertMatch({_, user_registered}, server:register_user(server_handle, "D")),
    ["A", "B", "C", "D"].

log_in_test() ->
    [UserName1, UserName2 | _] = register_user_test(),
    ?assertMatch({_Server1, logged_in}, server:log_in(server_handle, UserName1)),
    ?assertMatch({_Server2, logged_in}, server:log_in(server_handle, UserName2)).
    % Note: returned pids _Server1 and _Server2 do not necessarily need to be
    % the same.

log_out_test() ->
    [UserName1, UserName2 | _] = register_user_test(),
    {Server1, logged_in} = server:log_in(server_handle, UserName1),
    {Server2, logged_in} = server:log_in(server_handle, UserName2),
    ?assertMatch(logged_out, server:log_out(Server1, UserName1)),
    ?assertMatch(logged_out, server:log_out(Server2, UserName2)).

join_channel_test() ->
    [UserName1 | _] = register_user_test(),
    {Server1, logged_in} = server:log_in(server_handle, UserName1),
    ?assertMatch(channel_joined,
        server:join_channel(Server1, UserName1, "Channel1")),
    ?assertMatch(channel_joined,
        server:join_channel(Server1, UserName1, "Channel2")),
    {UserName1, Server1, "Channel1", "Channel2"}.

send_message_test() ->
    {UserName1, Server1, Channel1, _Channel2} = join_channel_test(),
    ?assertMatch(message_sent,
        server:send_message(Server1, UserName1, Channel1, "Hello!")),
    ?assertMatch(message_sent,
        server:send_message(Server1, UserName1, Channel1, "How are you?")).

channel_history_test() ->
    % Create users, log in, join channels.
    [UserName1, UserName2 | _] = register_user_test(),
    {Server1, logged_in} = server:log_in(server_handle, UserName1),
    {Server2, logged_in} = server:log_in(server_handle, UserName2),
    Channel1 = "Channel1",
    server:join_channel(Server1, UserName1, Channel1),
    server:join_channel(Server2, UserName2, Channel1),

    % Send some messages
    server:send_message(Server1, UserName1, Channel1, "Hello!"),
    server:send_message(Server2, UserName2, Channel1, "Hi!"),
    server:send_message(Server1, UserName1, Channel1, "How are you?"),

     % Check history
    [{message, UserName1, Channel1, "Hello!", Time1},
     {message, UserName2, Channel1, "Hi!", Time2},
     {message, UserName1, Channel1, "How are you?", Time3}] =
        server:get_channel_history(Server1, Channel1),

    ?assert(Time1 =< Time2),
    ?assert(Time2 =< Time3).

typical_session_test() ->
    initialize_test(),
    Session1 = spawn_link(?MODULE, typical_session_1, [self()]),
    Session2 = spawn_link(?MODULE, typical_session_2, [self()]),
    receive
        {Session1, ok} ->
            receive
                {Session2, ok} ->
                    done
            end
    end.

typical_session_1(TesterPid) ->
    {_, user_registered} = server:register_user(server_handle, "Jennifer"),
    {Server, logged_in} = server:log_in(server_handle, "Jennifer"),
    channel_joined = server:join_channel(Server, "Jennifer", "multicore"),
    message_sent = server:send_message(Server, "Jennifer", "multicore", "Hello!"),
    % Wait for reply
    Time2 = receive
        {_, new_message, Message} ->
            ?assertMatch({message, "Janwillem", "multicore", "Hi!", _}, Message),
            {message, _, _, _, Time} = Message,
            Time
    end,
    % Respond
    message_sent = server:send_message(Server, "Jennifer", "multicore", "How are you?"),

    % Check history
    [{message, "Jennifer",  "multicore", "Hello!",       Time1},
     {message, "Janwillem", "multicore", "Hi!",          Time2},
     {message, "Jennifer",  "multicore", "How are you?", Time3}] =
        server:get_channel_history(Server, "multicore"),
    ?assert(Time1 =< Time2),
    ?assert(Time2 =< Time3),

    TesterPid ! {self(), ok}.

typical_session_2(TesterPid) ->
    {_, user_registered} = server:register_user(server_handle, "Janwillem"),
    {Server, logged_in} = server:log_in(server_handle, "Janwillem"),
    channel_joined = server:join_channel(Server, "Janwillem", "multicore"),
    % Wait for first message
    Time1 = receive
        {_, new_message, Message1} ->
            ?assertMatch({message, "Jennifer", "multicore", "Hello!", _}, Message1),
            {message, _, _, _, Time} = Message1,
            Time
    end,
    % Reply
    message_sent = server:send_message(Server, "Janwillem", "multicore", "Hi!"),
    % Wait for response
    Time3 = receive
        {_, new_message, Message3} ->
            ?assertMatch({message, "Jennifer", "multicore", "How are you?", _}, Message3),
            {message, _, _, _, Time_} = Message3,
            Time_
    end,

    % Check history
    [{message, "Jennifer",  "multicore", "Hello!",       Time1},
     {message, "Janwillem", "multicore", "Hi!",          Time2},
     {message, "Jennifer",  "multicore", "How are you?", Time3}] =
        server:get_channel_history(Server, "multicore"),
    ?assert(Time1 =< Time2),
    ?assert(Time2 =< Time3),

    TesterPid ! {self(), ok}.

all()->[initialize_test, register_user_test, log_in_test, log_out_test, join_channel_test
    , send_message_test, channel_history_test, typical_session_test, typical_session_1
    , typical_session_2].

generate_channels(Size, NumberOfChannels)->
    lists:map(fun(_)->rand:uniform(NumberOfChannels) end, lists:seq(1, Size)).

