-module(server_handle).
-include_lib("eunit/include/eunit.hrl").

-export([create_server_handle/0, create_server_handle_with/2, server_handle/2]).

create_server_handle()->
	ChannelService = channel_service:create_channel_service(),
    ServerPid = spawn(?MODULE, server_handle, [ChannelService, dict:new()]),
    catch unregister(server_handle),
    register(server_handle, ServerPid),
    ServerPid.

create_server_handle_with(Users, Channels)->
    {ChannelService, ExtendedUsers} = channel_service:create_channel_service_with(Channels, Users),
    ServerPid = spawn(?MODULE, server_handle, [ChannelService, ExtendedUsers]),
    catch unregister(server_handle),
    register(server_handle, ServerPid),
    ServerPid.

server_handle(ChannelService, Clients) ->
    receive
        {Client, register_user, UserName} ->
            NewClients = dict:store(UserName, {user, UserName, dict:new()}, Clients),
            Client !  {self(), user_registered},
            server_handle(ChannelService, NewClients); 
        {Client, log_in, UserName} -> 
            case dict:find(UserName, Clients) of
                {ok, {user, UserName, Channels}} -> 
                    spawn(fun()->client_mirror:create_mirror(Client, UserName, ChannelService, self(), Channels) end),
                    NewClients = dict:erase(UserName, Clients),
                    server_handle(ChannelService, NewClients);
                error -> 
                    Client ! {self(), non_existing_username},
                    server_handle(ChannelService, Clients)
        end; 
        {_MirrorId, log_out, UserName, Channels} ->
            NewClients = dict:store(UserName, {user, UserName, Channels}, Clients),
            server_handle(ChannelService, NewClients);
        Message->
            ?debugFmt("server handle got unkown message ~s ~n", [Message]),
            server_handle(ChannelService, Clients)
    end.

