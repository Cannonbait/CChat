-module(server).
-export([main/1, initial_state/1]).
-include_lib("./defs.hrl").

main(State) ->
	receive
		{request, From, Ref, {ping, Data}} ->
			%special-case where server shouldnt neccesarily
			%respond back
			loop(State, {ping, Data}),
			main(State);
		{request, From, Ref, {join, Value}} ->
			join(From, Ref, Value),
			main(State);
		{request, From, Ref, Request} ->
			{Response, NextState} = loop(State, Request),
			From ! {result, Ref, Response},
			main(NextState)
	end.

initial_state(ServerName) ->
	%When the server is started we have no connected clients and no channels
    #server_st{connectedClients=[]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%Internal functions

%User is attempting to connect to server
loop(State, {connect, {Nick, Id}}) ->	
	case lists:keymember(Nick, 1, State#server_st.connectedClients) of 
		false->		%If the Nick did not already exist add it to connected
			T = State#server_st.connectedClients,
			NextState = State#server_st{connectedClients=lists:sort([{Nick, Id}|T])},
			{{connect, ok}, NextState};
		true ->		%Return error if user is already connected with this nick
			{{connect, user_already_connected}, State}
	end;

%User wants to disconnect from server
loop(State, {disconnect, Id}) ->
	% {Response, NextState}
	ConnectedClients = State#server_st.connectedClients,
	%Create a list without the Client that is disconnecting
	UpdatedClients = [{Nick, ClientId} || {Nick, ClientId} <- ConnectedClients, ClientId =/= Id],
	NewState = State#server_st{connectedClients = UpdatedClients},
	{{disconnect, ok}, NewState};

%User wants to ping another user
loop(State, {ping, {PongeePid, Name, Time}}) ->
	case lists:keymember(Name, 1, State#server_st.connectedClients) of
		true ->
			{_, Id} = lists:keyfind(Name, 1, State#server_st.connectedClients),
			helper:requestAsync(Id, {sendpong, {PongeePid, Name, Time}}),
			{ok, State};
		false ->
			helper:requestAsync(PongeePid, {pingfail, Name}),
			{ok, State}
	end.


%User wants to join a room
join(From, Ref, {Id, Channel}) ->
	ChannelAtom = list_to_atom(Channel),
	case lists:member(list_to_atom(Channel), registered()) of
		false ->
			helper:start(ChannelAtom, channel:initial_state(Id), fun channel:main/1),
			From ! {result, Ref, {join, ok}},
			ok;
		true ->
			ChannelAtom ! {request, From, Ref, {join, {Id}}},
			ok
	end.
