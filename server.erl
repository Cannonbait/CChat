-module(server).
-export([main/1, initial_state/1]).
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
	case lists:keymember(Nick, 1, State#server_st.connectedClients) of 
		false->		%If the Nick did not already exist add it to connected
			T = State#server_st.connectedClients,
			NextState = State#server_st{connectedClients=[{Nick, Id}|T]},
			{{connect, ok}, NextState};
		true ->		%Return error if user is already connected with this nick
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
	ChannelAtom = list_to_atom(Channel),
	helper:start(ChannelAtom, channel:initial_state(Id), fun channel:main/1),
	%helper:start(to_atom(ClientName), client:initial_state("user01", GUIName), fun client:main/1),
	NewChannels = [ChannelAtom| State#server_st.channels],
	{{join, ok}, State#server_st{channels = NewChannels}}.





%Recursive function to find out if a user is within a channel
userInChannels([], _ ) -> false;
userInChannels([Channel|T], Id) ->
	false.