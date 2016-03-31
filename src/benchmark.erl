-module(benchmark).
-include_lib("eunit/include/eunit.hrl").

-export([send_message_random_sparce/0, send_message_random_dense/0, 
        send_message_structured_sparce/0, send_message_structured_dense/0,
        send_message_worst_case/0, measure_latency/0, measure_channel_history/0]).

%% Benchmark helpers

% Recommendation: run each test at least 30 times to get statistically relevant
% results.
run_benchmark(Name, Fun, Times) ->
    ThisPid = self(),
    lists:foreach(fun (N) ->
        % Recommendation: to make the test fair, each new test run is to run in
        % its own, newly created Erlang process. Otherwise, if all tests run in
        % the same process, the later tests start out with larger heap sizes and
        % therefore probably do fewer garbage collections. Also consider
        % restarting the Erlang emulator between each test.
        % Source: http://erlang.org/doc/efficiency_guide/profiling.html
        spawn_link(fun () ->
            run_benchmark_once(Name, Fun, N),
            ThisPid ! done
        end),
        receive done ->
            ok
        end
    end, lists:seq(1, Times)).

run_benchmark_once(Name, Fun, N) ->
    io:format("Running benchmark ~s: ~p~n", [Name, N]),

    % Start timers
    % Tips:
    % * Wall clock time measures the actual time spent on the benchmark.
    %   I/O, swapping, and other activities in the operating system kernel are
    %   included in the measurements. This can lead to larger variations.
    %   os:timestamp() is more precise (microseconds) than
    %   statistics(wall_clock) (milliseconds)
    % * CPU time measures the actual time spent on this program, summed for all
    %   threads. Time spent in the operating system kernel (such as swapping and
    %   I/O) is not included. This leads to smaller variations but is
    %   misleading.
    statistics(runtime),        % CPU time, summed for all threads
    StartTime = os:timestamp(), % Wall clock time

    % Run
    Fun(),

    % Get and print statistics
    % Recommendation [1]:
    % The granularity of both measurement types can be high. Therefore, ensure
    % that each individual measurement lasts for at least several seconds.
    % [1] http://erlang.org/doc/efficiency_guide/profiling.html
    {_, Time1} = statistics(runtime),
    Time2 = timer:now_diff(os:timestamp(), StartTime),
    io:format("CPU time = ~p ms~nWall clock time = ~p ms~n",
        [Time1, Time2 / 1000.0]),
    io:format("~s done~n", [Name]).

%% Benchmarks

% Creates a server with random channels appointed to every user
initialize_server_random_channels(NumberOfChannels, NumberOfUsers, ChannelsPerUser) ->
    rand:seed_s(exsplus, {0, 0, 0}),
    ChannelNames = lists:seq(1, NumberOfChannels),
    UserNames = lists:seq(1, NumberOfUsers),
    Channels = dict:from_list(lists:map(fun (Name) ->
        Messages = [{message, 5, Name, "Hello!", os:system_time()},
                    {message, 6, Name, "Hi!", os:system_time()},
                    {message, 5, Name, "Bye!", os:system_time()}],
        Channel = {channel, Name, Messages},
        {Name, Channel}
        end,
        ChannelNames)),
    Users = dict:from_list(lists:map(fun (Name) ->
            Subscriptions = lists:map(fun(_)->
                    rand:uniform(NumberOfChannels) end, 
                lists:seq(1, ChannelsPerUser)),
            User = {user, Name, sets:to_list(sets:from_list(Subscriptions))},
            {Name, User} end,
        UserNames)),
    ServerPid = server_handle:create_server_handle_with(Users, Channels),
    %Channels is a list
    %Users is a dict (UserName -> {user, UserName, Channels})
    {ServerPid, Channels, Users}.

% Creates a server with channels appointed based on a function
initialize_server_structured(NumberOfChannels, NumberOfUsers, UserToChannelFunction) ->
    ChannelNames = lists:seq(1, NumberOfChannels),
    UserNames = lists:seq(1, NumberOfUsers),
    Channels = dict:from_list(lists:map(fun (Name) ->
        Messages = [],
        Channel = {channel, Name, Messages},
        {Name, Channel}
        end,
        ChannelNames)),
    Users = dict:from_list(lists:map(fun (Name) ->
            User = {user, Name, UserToChannelFunction(Name)},
            {Name, User} end,
        UserNames)),
    ServerPid = server_handle:create_server_handle_with(Users, Channels),
    {ServerPid, Channels, Users}.

%login all given users and return a map username to clientId and channels
login_users(ServerPid, Users, BenchmarkPid)->
    Clients = dict:map(fun (UserName, {user, UserName, Channels}) ->
        Client = benchmark_client:create_benchmark_client(UserName, ServerPid),
        Client ! {BenchmarkPid, log_in},
        {Client, Channels} end,
        Users),
    dict:map(fun (UserName, _User) ->
            receive {UserName, logged_in} ->
                ok
            end
        end,
        Users),
    %Clients is a dict (UserName -> {BenchmarkClientId, Channels})
    Clients.

test_send_message(InitializeFunction, ActiveUsersFunction, BenchmarkText) ->
    run_benchmark(BenchmarkText,
        fun () ->
            BenchmarkPid = self(),
            {ServerPid, _Channels, Users} = InitializeFunction(),
            Clients = login_users(ServerPid, ActiveUsersFunction(Users), BenchmarkPid),
            %every active user has to send a message
            dict:map(fun(SenderName, {ClientPid, Channels})->
                % select random channel from channel list
                Channel = get_channel(Channels),

                %send message to channel
                ClientPid ! {self(), send_message, Channel, "test"},
                receive
                    {SenderName, message_sent} -> ok
                end,
                
                % all other online users following the channel should get this message
                % remember only those users substribed to the channel
                ClientsSubscribedToChannel = 
                    dict:filter(fun (_UserName, {_ReceiverPid, ReceiverChannels}) ->
                        lists:member(Channel, ReceiverChannels) end, 
                Clients),

                %remove sender from list
                ExpectedReceivers = dict:erase(SenderName, ClientsSubscribedToChannel),
                dict:map(fun (ReceiverName, _ReceiverData)->
                            receive {ReceiverName, received_message} -> ok end
                        end,
                        ExpectedReceivers) end, 
                Clients) end,                
        30).

% Send a message for each of the 1000 active users, and wait for all of them to be broadcast and received
% (repeated 30 times).
%average case, some channels with many users, some channels with few users.
send_message_random_sparce()->
    InitializeFunction=fun()->initialize_server_random_channels(10, 1000, 3) end,
    ActiveUsersFunction=fun(Users)->Users end,
    test_send_message(InitializeFunction, ActiveUsersFunction, "send message random sparce").

send_message_random_dense()->
    InitializeFunction=fun()->initialize_server_random_channels(10, 1000, 8) end,
    ActiveUsersFunction=fun(Users)->Users end,
    test_send_message(InitializeFunction, ActiveUsersFunction, "send message random dense").

%best case, every channel with equal amount of people.
send_message_structured_sparce()->
    UserToChannelFunction=fun(User)->quarter_distribution(User) end,
    InitializeFunction=fun()->initialize_server_structured(12, 1000, UserToChannelFunction) end,
    ActiveUsersFunction=fun(Users)->Users end,
    test_send_message(InitializeFunction, ActiveUsersFunction, "send message structured sparce").

send_message_structured_dense()->
    UserToChannelFunction=fun(User)->half_distribution(User) end,
    InitializeFunction=fun()->initialize_server_structured(12, 1000, UserToChannelFunction) end,
    ActiveUsersFunction=fun(Users)->Users end,
    test_send_message(InitializeFunction, ActiveUsersFunction, "send message structured dense").

%worst case, every user subscribed to every channel. One actor has to answer all users
send_message_worst_case()->
    NumberOfChannels = 12,
    UserToChannelFunction=fun(_User)->lists:seq(1, NumberOfChannels)  end,
    InitializeFunction=fun()->initialize_server_structured(NumberOfChannels, 1000, UserToChannelFunction) end,
    ActiveUsersFunction=fun(Users)->Users end,
    test_send_message(InitializeFunction, ActiveUsersFunction, "send message structured dense").


quarter_distribution(User)->
    SingleDigit = User rem 10,
    if 
        SingleDigit < 4 -> 
            [1,2,3,4];
        SingleDigit < 8 -> 
            [5,6,7,8];
        true -> 
            [9,10,11,12]
    end.

half_distribution(User)->
    SingleDigit = User rem 10,
    if 
        SingleDigit < 4 -> 
            [1,3,5,7,9,11];
        SingleDigit < 8 -> 
            [2,4,6,8,10,12];
        true -> 
            [1,4,5,8,9,12]
    end.

%check how long it takes for one message to arrive at all followers
latency(ActiveUsersFunction, BenchmarkText) ->
    UserToChannelFunction=fun(_User)->[1] end, %every user is subscribed to channel 1
    run_benchmark(BenchmarkText,
        fun () ->
            BenchmarkPid = self(),
            {ServerPid, _Channels, Users} = initialize_server_structured(12, 1000, UserToChannelFunction),
            Clients = login_users(ServerPid, ActiveUsersFunction(Users), BenchmarkPid),
            
            %user 1 sends a message
            {ClientOne, _ClientOneChannels} = dict:fetch(1, Clients),
            ClientOne ! {self(), send_message, 1, "test_latency"},
            
            ExpectedReceivers = dict:erase(1, Clients),
            dict:map(fun (ReceiverName, _ReceiverData)->
                        receive {ReceiverName, received_message} -> ok end
                    end,
                ExpectedReceivers) end,            
        30).

measure_latency()->
    ActiveUsersFunction=fun(Users)->Users end,
    latency(ActiveUsersFunction, "measure latency").    

%check how long it takes to receive the channel history
channel_history(BenchmarkText) ->
    NumberOfChannels = 10000,
    UserToChannelFunction=fun(_User)->lists:seq(1, NumberOfChannels) end, %every user is subscribed to every channel
    run_benchmark(BenchmarkText,
        fun () ->
            BenchmarkPid = self(),
            {ServerPid, _Channels, Users} = initialize_server_structured(NumberOfChannels, 1, UserToChannelFunction),
            Clients = login_users(ServerPid, Users, BenchmarkPid),
            
            %user 1 asks channel history
            {Client, ClientChannels} = dict:fetch(1, Clients),

            Client ! {self(), get_channel_history, 1},
            %ask to get channel history of every channel
            lists:map(fun (ChannelName)->
                        Client ! {self(), get_channel_history, ChannelName}
                    end,
                ClientChannels),  
            %check if every channel has returned history
            lists:map(fun (ChannelName)->
                        receive {1, received_history, ChannelName} -> ok end
                    end,
                ClientChannels) end,       
        30).

measure_channel_history()->
    channel_history("get channel history").

% Helper function: get random channel from the set of channels
get_channel(Channels)->
    lists:nth(rand:uniform(length(Channels)), Channels).