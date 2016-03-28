-module(channel_service).
-include_lib("eunit/include/eunit.hrl").

-export([create_channel_service/0, create_channel_service_with/2, channel_service/1]).

create_channel_service()->
    spawn(?MODULE, channel_service, [dict:new()]).

create_channel_service_with(ChannelNames, Users)->
    Channels=dict:map(fun(ChannelName, {channel, ChannelName, History})->
                ChannelId = channel:create_channel_with(History, ChannelName),
                {channel, ChannelId} 
            end,
            ChannelNames),
    ExtendedUsers = dict:map(fun(_UserName, User)->addChannelId(User, Channels) end, Users),
    ChannelService = spawn(?MODULE, channel_service, [Channels]),
    {ChannelService, ExtendedUsers}.

addChannelId({user, UserName, ChannelNames}, Channels)->
    ExtendedChannels = 
        lists:map(fun(ChannelName)->
            case dict:find(ChannelName, Channels) of
                    {ok, {channel, ChannelId}} -> 
                        {ChannelName, ChannelId};
                    error -> 
                       erlang:error("Channel not found")
            end end, ChannelNames),
    {user, UserName, dict:from_list(ExtendedChannels)}.
    
channel_service(Channels) ->
    receive
        {MirrorId, join_channel, ChannelName, ClientId} ->
            case dict:find(ChannelName, Channels) of
        		{ok, {channel, ChannelId}} -> 
        			ChannelId ! {MirrorId, add_user, ChannelName, ClientId},
        			channel_service(Channels);
        		error -> 
        			ChannelId = channel:create_channel(ClientId, ChannelName),
                    NewChannels = dict:store(ChannelName, {channel, ChannelId}, Channels),
                    MirrorId ! {ChannelId, channel_joined, ChannelName},
                    channel_service(NewChannels)
                end;                
        Message->
            ?debugFmt("Channel service got unkown message ~s ~n", [Message]),
            channel_service(Channels)
    end.