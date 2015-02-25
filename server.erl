-module(server).
-export([main/1, initial_state/1, sendMessage/2]).
-include_lib("./defs.hrl").

main(State) ->
	receive
		{request, From, Ref, Request} ->
			{Response, NextState} = loop(State, Request),
			From ! {result, Ref, Response},
			main(NextState)
	end.

initial_state(ServerName) ->
	%When the server is started we have no connected clients and no channels
    #server_st{connectedClients=[], channels = []}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%Internal functions

%User is attempting to connect to server
loop(State, {connect, {Nick, Id}}) ->	
	case lists:keyfind(Nick, 1, State#server_st.connectedClients) of 
		false->		%If the Nick did not already exist add it to connected
			T = State#server_st.connectedClients,
			NextState = State#server_st{connectedClients=[{Nick, Id}|T]},
			{{connect, ok}, NextState};
		_ ->		%Return error if user is already connected with this nick
			{{connect, user_already_connected}, State}
	end;

%User wants to disconnect from server
loop(State, {disconnect, Id}) ->
	% {Response, NextState}
	case userInChannels(State#server_st.channels, Id) of
		false ->	%If the user is not in any channels
			ConnectedClients = State#server_st.connectedClients,
			%Create a list without the Client that is disconnecting
			UpdatedClients = [{Nick, ClientId} || {Nick, ClientId} <- ConnectedClients, ClientId =/= Id],
			NewState = State#server_st{connectedClients = UpdatedClients},
			{{disconnect, ok}, NewState};
		true ->		%If the user has not left channels, return error
			{{disconnect, leave_channels_first}, State}
	end;


%User wants to join a room
loop(State, {join, {Id, Channel}}) ->
	case lists:keyfind(Channel, 1, State#server_st.channels) of
		false ->		%If the channel does not exists, create it and add the user
			NewChannels = [ {Channel, [Id]} | State#server_st.channels],
			{{join, ok}, State#server_st{channels = NewChannels}};
		_ ->			%Channel exists, add user 
			
			{Name, Users} = lists:keyfind(Channel, 1, State#server_st.channels),
			case contains(Users, Id) of
				false -> 
					CleanedChannels = lists:delete({Name, Users}, State#server_st.channels),
					NewChannel = {Name, [Id|Users]},
					{{join, ok}, State#server_st{channels = [NewChannel|CleanedChannels]}};
				true ->
					{{join, user_already_joined}, State}
				end
		end;

loop(State, {leave, {Id, Channel}}) ->
	case lists:keyfind(Channel, 1, State#server_st.channels) of
		false ->
			{{leave, ok}, State};
		_ ->

			{Name, Users} = lists:keyfind(Channel, 1, State#server_st.channels),
			case contains(Users, Id) of
				true ->
					NewChannel = {Name, lists:delete(Id, Users)},
					CleanedChannels = lists:delete({Name, Users}, State#server_st.channels),
					{{leave, ok}, State#server_st{channels = [NewChannel|CleanedChannels]}};
				false ->
					{{leave, user_not_joined}, State}
			end
		end;


%Recieve message from user
loop(State, {message, {Nick, Id, Channel, Msg}}) ->
	{Name, Users} = lists:keyfind(Channel, 1, State#server_st.channels),
	case contains(Users, Id) of
		false ->
			{{message, user_not_joined}, State};
		true ->
			spawn(server, sendMessage, [Users, {Channel, Nick, Msg, Id}]),
			{{message, ok}, State}
	end.




sendMessage([], _) -> ok;
sendMessage([H|T], {Channel, Name, Msg, H}) -> 
	sendMessage(T, {Channel, Name, Msg, H});
sendMessage([H|T], {Channel, Name, Msg, Id}) ->
	H ! {message, {incoming_msg, Channel, Name, Msg}},
	sendMessage(T, {Channel, Name, Msg, Id}).


contains(List, Element) -> 
	case [X || X <- List, X =:= Element] of
		[] -> false;
		_ -> true
	end.


userInChannels([], _ ) -> false;
userInChannels([{_, Users}|T], Id) ->
	case contains(Users, Id) of
		false ->
			contains(T, Id);
		true ->
			true
	end.