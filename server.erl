-module(server).
-export([main/1, initial_state/1]).
-include_lib("./defs.hrl").

main(State) ->
    % TODO: Receive message, handle it, and loop
	receive
		{request, From, Request} ->
			{Response, NextState} = loop(State, Request),
			From ! {server_response, Response},
			main(NextState)
	end.

initial_state(ServerName) ->
    #server_st{connectedClients=[], channels = []}.

loop(State, {connect, {Nick, Id}}) ->	
	%If nick is already present in connectedClients...
	case lists:keyfind(Nick, 1, State#server_st.connectedClients) of 
		false->
			T = State#server_st.connectedClients,
			NextState = State#server_st{connectedClients=[{Nick, Id}|T]},
			{{connect, ok}, NextState};
		_ ->
			{{connect, user_already_connected}, State}
	end;

loop(State, {disconnect, Id}) ->
	% {Response, NextState}
	case userInChannels(State#server_st.channels, Id) of
		false ->
			ConnectedClients = State#server_st.connectedClients,
			UpdatedClients = [{Nick, ClientId} || {Nick, ClientId} <- ConnectedClients, ClientId =/= Id],
			NewState = State#server_st{connectedClients = UpdatedClients},
			{{disconnect, ok}, NewState};
		_ ->
			{{disconnect, leave_channels_first}, State}
	end;


%If trying to join room that does not exist
loop(State, {join, {Id, Channel}}) ->
	Search = case lists:keyfind(Channel, 1, State#server_st.channels) of
		false ->
			NewChannels = [ {Channel, [Id]} | State#server_st.channels],
			{{join, ok}, State#server_st{channels = NewChannels}};
		_ ->

			%Lägg till användaren i kanalen
			Elem = lists:keyfind(Channel, 1, State#server_st.channels),
			{Name, Users} = Elem,
			case contains(Users, Id) of
				false -> 
					CleanedChannels = lists:delete(Elem, State#server_st.channels),
					NewChannel = {Name, [Id|Users]},
					{{join, ok}, State#server_st{channels = [NewChannel|CleanedChannels]}};
				_ ->
					{{join, user_already_joined}, State}
				end
		end;

loop(State, {leave, {Id, Channel}}) ->
	Search = case lists:keyfind(Channel, 1, State#server_st.channels) of
		false ->
			{{leave, ok}, State};
		_ ->

			{Name, Users} = lists:keyfind(Channel, 1, State#server_st.channels),
			case contains(Users, Id) of
				true ->
					NewChannel = {Name, lists:delete(Id, Users)},
					CleanedChannels = lists:delete({Name, Users}, State#server_st.channels),
					{{leave, ok}, State#server_st{channels = [NewChannel|CleanedChannels]}};
				_ ->
					{{leave, user_not_joined}, State}
			end
		end;


%Recieve message from user
loop(State, {message, {Nick, Id, Channel, Msg}}) ->
	{Name, Users} = lists:keyfind(Channel, 1, State#server_st.channels),
	case contains(Users, Id) of
		false ->
			{{message, user_not_joined}, State};
		_ ->
			{{message, sendMessage(Users, {Channel, Nick, Msg, Id})}, State}
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


	


