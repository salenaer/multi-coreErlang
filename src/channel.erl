-module(channel).
-include_lib("eunit/include/eunit.hrl").

-export([create_channel/2, create_channel_with/2, channel_actor/3]).

create_channel(UserId, ChannelName)->
    spawn(?MODULE, channel_actor, [ChannelName, [], [UserId]]).

create_channel_with(History, ChannelName)->
    spawn(?MODULE, channel_actor, [ChannelName, History, []]).

%ChannelName is used to make sure no messages are send to the wrong channel. 
%History is a list of tripples: {Name of sender, Time of sending, Message}.
%Users is a list of client_mirror ids who are currently online and following the channel.
channel_actor(ChannelName, History, Users)->
	receive
		{MirrorId, add_user, ChannelName, ClientId} ->
			MirrorId ! {self(), channel_joined, ChannelName},
			channel_actor(ChannelName, History, [ClientId|Users]);
		{_, remove_user, ChannelName, UserId} ->
			NewUsers = lists:delete(UserId, Users),
			channel_actor(ChannelName, History, NewUsers);
		{SenderId, send_message, UserName, ChannelName, Message, Time} ->
			lists:foreach(fun(FollowerId)->(
				if 
					FollowerId /= SenderId -> 
						FollowerId ! {self(), new_message, {message, UserName, ChannelName, Message, Time}};
					true -> false
				end) end, Users),
			channel_actor(ChannelName, [{message, UserName, ChannelName, Message, Time}|History], Users);
		{ClientId, get_channel_history} ->
			ClientId ! {self(), channel_history, History},
			channel_actor(ChannelName, History, Users);
		Message ->
            ?debugFmt("Channel got unkown message ~w", [Message]),
            channel_actor(ChannelName, History, Users)
    end.		