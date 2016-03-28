-module(client).
-include_lib("eunit/include/eunit.hrl").

-export([create_client/2, client_actor/2]).

create_client(UserName, SID)->
    spawn(?MODULE, client_actor, [SID, UserName]).

%before logging in or registering, the SID is the process id from the server_handle.
%after logging in the SID is the process id from the client mirror. 
client_actor(SID, UserName) ->
    receive
        % client can receive the following messages from the server: 
        {Sender, user_registered}->
            io:fwrite("client registered ~n"),
            client_actor(Sender, UserName);
        {Sender, logged_in} ->
            io:fwrite("client ~s logged in ~n", [UserName]),
            client_actor(Sender, UserName);
        {Sender, logged_out} ->
            io:fwrite("client ~s logged out ~n", [UserName]),
            client_actor(Sender, UserName);
        {_, channel_joined}->
            io:fwrite("client joined channel ~n", []),
            client_actor(SID, UserName);
        {_, new_message, {message, SenderName, ChannelName, Message, Time}} ->
            DateTime = calendar:gregorian_seconds_to_datetime(Time),
            {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
            io:fwrite("new message for actor: ~s message: ~s in channel: ~s from: ~s on:  ~2..0B/~2..0B/~4..0B ~2B:~2.10.0B:~2.10.0B\n", 
                [UserName, Message, ChannelName, SenderName, Day, Month, Year, Hour, Min, Sec]),
            client_actor(SID, UserName);
        {_, message_sent} ->
            client_actor(SID, UserName);
        {_, channel_history, History} ->
            erlang:display(History),
            client_actor(SID, UserName);

        % client can receive the following message from the graphical unit interface / programmer.
        {_, join_channel, ChannelName} ->
            SID ! {self(), join_channel, UserName, ChannelName},
            client_actor(SID, UserName);
        {_, register_user} ->
            SID ! {self(), register_user, UserName},
            client_actor(SID, UserName);
        {_, log_in} ->
            SID ! {self(), log_in, UserName},
            client_actor(SID, UserName);
        {_, log_out} ->
            SID ! {self(), log_out, UserName},
            client_actor(server_handle, UserName);
        {_, get_channel_history, ChannelName}->
            SID ! {self(), get_channel_history, ChannelName},
            client_actor(SID, UserName);
        {_, send_message, ChannelName, Message} ->
            SID ! {self(), send_message, UserName, ChannelName, Message, os:system_time()},
            client_actor(SID, UserName);

        % all other message are simply printed, known message with the wrong PID are also printed.
        Message->
            ?debugFmt("client got unkown message ~s ~n", [Message]),
            client_actor(SID, UserName)
    end.