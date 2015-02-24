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
    #server_st{connectedClients=[]}.

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
	{ok, NextState}.


