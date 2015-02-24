-module(server).
-export([main/1, initial_state/1]).
-include_lib("./defs.hrl").

main(State) ->
    % TODO: Receive message, handle it, and loop
	receive
		{request, From, Request} ->
			{Response, NextState} = loop(State, Request),
			From ! {response, Response},
			main(NextState)
	end.

initial_state(ServerName) ->
    #server_st{connectedClients=[], channels = []}.

loop(State, {connect, {Nick, Id}}) ->	
	%{Response, NextState}

	T = State#server_st.connectedClients,
	NextState = State#server_st{connectedClients=[{Nick, Id}|T]},
	{ok, State};

loop(State, {disconnect, Id}) ->
	% {Response, NextState}
	ConnectedClients = State#server_st.connectedClients,
	UpdatedClients = [{Nick, ClientId} || {Nick, ClientId} <- ConnectedClients, ClientId =/= Id],
	NextState = State#server_st{connectedClients = UpdatedClients},
	{ok, NextState};


%If trying to join room that does not exist
loop(State, {join, {Id, Channel}}) ->
	Search = case lists:keyfind(Channel, 1, State#server_st.channels) of
		false ->
			NewChannels = [ {Channel, [Id]} | State#server_st.channels],
			{ok, State#server_st{channels = NewChannels}};
		_ ->
			%Lägg till användaren i kanalen
			Elem = lists:keyfind(Channel, 1, State#server_st.channels),
			{Name, Users} = Elem,
			CleanedChannels = lists:delete(Elem, State#server_st.channels),
			NewChannel = {Name, [Id|Users]},
			{ok, State#server_st{channels = [NewChannel|CleanedChannels]}}
		end;

%Recieve message from user
loop(State, {message, {Nick, Id, Channel, Msg}}) ->
	{Name, Users} = lists:keyfind(Channel, 1, State#server_st.channels),
	{sendMessage(Users, {Channel, Nick, Msg, Id}), State}.




sendMessage([], _) -> ok;
sendMessage([H|T], {Channel, Name, Msg, H}) -> 
	sendMessage(T, {Channel, Name, Msg, H});
sendMessage([H|T], {Channel, Name, Msg, Id}) ->
	H ! {message, {incoming_msg, Channel, Name, Msg}},
	sendMessage(T, {Channel, Name, Msg, Id}).




	


