-module(client_mirror).
-include_lib("eunit/include/eunit.hrl").

-export([create_mirror/5, client_mirror/5]).

create_mirror(ClientId, UserName, ChannelService, ClientService, Channels) ->
    Mirror = spawn(?MODULE, client_mirror, [ClientId, UserName, Channels, ChannelService, ClientService]),
    dict:map(fun(ChannelName, ChannelId)->
                ChannelId ! {self(), add_user, ChannelName, ClientId} end, Channels),
    dict:map(fun(ChannelName, ChannelId)->
                receive {ChannelId, channel_joined, ChannelName} -> ok end end, Channels),
    ClientId ! {Mirror, logged_in}.

% ClientId = Id of client
% Channels are tupples {ChannelName, ChannelProcessId}
% ChannelService = process where all channels are connected with their process ids
client_mirror(ClientId, UserName, Channels, ChannelService, ClientService) ->
    receive
        %client wants to join some channel.
        %pass channelName to channel_service.
        {ClientId, join_channel, UserName, ChannelName} ->
            ChannelService ! {self(), join_channel, ChannelName, ClientId},
            client_mirror(ClientId, UserName, Channels, ChannelService, ClientService);

        {ChannelId, channel_joined, ChannelName}->
            NewChannels = dict:store(ChannelName, ChannelId, Channels),
            ClientId ! {self(), channel_joined},
            client_mirror(ClientId, UserName, NewChannels, ChannelService, ClientService);

        {ClientId, log_out, UserName} ->
            ClientService ! {self(), log_out, UserName, Channels},
            dict:map(fun(ChannelName, ChannelId)->
                ChannelId ! {ClientId, remove_user, ChannelName} end, Channels),
            ClientId ! {self(), logged_out};

        {ClientId, send_message, UserName, ChannelName, Message, Time} ->
            case dict:find(ChannelName, Channels) of
                {ok, ChannelId} -> 
                    ChannelId ! {ClientId, send_message, UserName, ChannelName, Message, Time},
                    ClientId ! {self(), message_sent},
                    client_mirror(ClientId, UserName, Channels, ChannelService, ClientService);
                error -> 
                    ClientId ! {self(), non_following_channel},
                    client_mirror(ClientId, UserName, Channels, ChannelService, ClientService)
            end;
        {ClientId, get_channel_history, ChannelName}->
            case dict:find(ChannelName, Channels) of
                {ok, ChannelId} -> 
                    ChannelId ! {self(), get_channel_history},
                    client_mirror(ClientId, UserName, Channels, ChannelService, ClientService);
                error -> 
                    ClientId ! {self(), non_following_channel},
                    client_mirror(ClientId, UserName, Channels, ChannelService, ClientService)
            end;   

        {_Channel, channel_history, History}->
            ClientId ! {self(), channel_history, lists:reverse(History)},
            client_mirror(ClientId, UserName, Channels, ChannelService, ClientService);
        %unkown or wrong sender messages are printed.
        Message ->
            ?debugFmt("client_mirror got unkown message ~s ~n", [Message]),
            client_mirror(ClientId, UserName, Channels, ChannelService, ClientService)         
    end.