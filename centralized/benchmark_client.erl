-module(benchmark_client).
-include_lib("eunit/include/eunit.hrl").

-export([create_benchmark_client/2, benchmark_online_client/4, benchmark_offline_client/3]).

create_benchmark_client(UserName, SID)->
    spawn(?MODULE, benchmark_offline_client, [SID, SID, UserName]).

benchmark_online_client(TargetId, ServerId, BenchmarkId, UserName) ->
    receive
        {_, new_message, _Message}->
            BenchmarkId ! {UserName, received_message},
            benchmark_online_client(TargetId, ServerId, BenchmarkId, UserName);
        {SenderId, join_channel, ChannelName} ->
            server:join_channel(TargetId, UserName, ChannelName),
            SenderId ! {UserName, channel_joined},
            benchmark_online_client(TargetId, ServerId, BenchmarkId, UserName);
        {SenderId, log_out} ->
            server:log_out(TargetId, UserName),
            SenderId ! {UserName, logged_out},
            benchmark_offline_client(ServerId, ServerId, UserName);
        {SenderId, get_channel_history, ChannelName}->
            _History = server:get_channel_history(TargetId, ChannelName),
            SenderId ! {UserName, received_history, ChannelName},
            benchmark_online_client(TargetId, ServerId, BenchmarkId, UserName);
        {SenderId, send_message, ChannelName, MessageText} ->
            server:send_message(TargetId, UserName, ChannelName, MessageText),
            SenderId ! {UserName, message_sent},
            benchmark_online_client(TargetId, ServerId, BenchmarkId, UserName);

        % all other message are simply printed, known message with the wrong PID are also printed.
        Message->
            ?debugFmt("benchmark_online_client ~c got unkown message ~s ~n", [UserName, Message]),
             benchmark_online_client(TargetId, ServerId, BenchmarkId, UserName)
    end.

benchmark_offline_client(TargetId, ServerId, UserName) ->
    receive
        {SenderId, register_user} ->
            server:register_user(TargetId, UserName),
            SenderId ! {UserName, registered},
            benchmark_offline_client(TargetId, ServerId, UserName);
        {SenderId, log_in} ->
            {ResponsePid, logged_in} = server:log_in(TargetId, UserName),
            SenderId ! {UserName, logged_in},
            %?debugFmt("client logged in ~c ~n", [UserName]),
            benchmark_online_client(ResponsePid, ServerId, SenderId, UserName);
        Message->
            ?debugFmt("benchmark_offline_client ~c got unkown message ~s ~n", [UserName, Message]),
             benchmark_offline_client(TargetId, ServerId, UserName)
    end.