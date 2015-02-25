-module(client).
-export([main/1, initial_state/2]).
-include_lib("./defs.hrl").

%% Receive messages from GUI and handle them accordingly
main(State) ->
    receive
        {request, From, Ref, Request} ->
            {Response, NextState} = loop(State, Request),
            From ! {result, Ref, Response},
            main(NextState);
        {message, Info} ->
            {Response, NextState} = loop(State, Info),
            main(NextState)


		%{response, Response} ->

    end.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, connected = false }.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server if unconnected to a server
loop(St, {connect, Server}) when St#cl_st.connected =:= false ->
		
		ServerAtom = list_to_atom(Server),
		RegisteredPids = registered(),
		case lists:member(ServerAtom, RegisteredPids) of
			true ->
				ServerAtom ! {request, self(), {connect, {St#cl_st.nick, self()}}},
				receive
					{server_response, {connect, ok}} ->
						{ok, St#cl_st{connected = ServerAtom}};
					{server_response, {connect, user_already_connected}} ->
						{{error, user_already_connected, "Name taken, change with /nick <username>"}, St}
				after
					1000 ->
						{{error, server_not_reached, "Server timeout"}, St}
				end;
			_ ->
				{{error, server_not_reached, "Could not connect to server"}, St}
		end;

		
% Yell at user if he tries to connect when he is already connected
loop(St, {connect, Server}) ->
	{{error, user_already_connected, "You are already connected"}, St};

% Disconnect from server if already connected
loop(St, disconnect) when St#cl_st.connected =/= false ->
    % {ok, St} ;
	St#cl_st.connected ! {request, self(), {disconnect, self()}},
    receive
        {server_response, {disconnect, leave_channels_first}} ->
            {{error, leave_channels_first, "Leave your channels first"}, St};
		{server_response, {disconnect, ok}} ->
            {ok, St#cl_st{connected = false}}
    end;
	

% Yell at user if trying to disconnect when not connected
loop(St, disconnect) ->
	{{error, user_not_connected, "You are not connected"}, St};

% Join channel
loop(St, {join, Channel}) ->
    St#cl_st.connected ! {request, self(), {join, {self(), Channel}}},
    receive
        {server_response, {join, user_already_joined}} ->
            {{error, user_already_joined, "You are already in channel"}, St};
        {server_response, {join, ok}} ->
            {ok, St}
    end;

%% Leave channel
loop(St, {leave, Channel}) ->
    St#cl_st.connected ! {request, self(), {leave, {self(), Channel}}},
    receive
        {server_response, {leave, user_not_joined}} ->
            {{error, user_not_joined, "You did not join the channel"}, St};
        {server_response, {leave, ok}} ->
            {ok, St}
    end;

% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
    St#cl_st.connected ! {request, self(), {message, {St#cl_st.nick, self(), Channel, Msg}}},
    receive 
        {server_response, {message, user_not_joined}} ->
            {{error, user_not_joined, "You need to join channel"}, St};
        {server_response, {message, ok}} ->
            {ok, St}
    end;

%% Get current nick
loop(St, whoami) ->
    % {"nick", St} ;
	{St#cl_st.nick, St};
	
%% Change nick
loop(St, {nick, Nick}) when St#cl_st.connected =:= false ->
    % {ok, St} ;
	{ok, St#cl_st{nick=Nick}};

%Change nick when connected to server
loop(St, {nick, Nick}) ->
	{{error, cant_change_nick_connected, "You can't change nick when connected to a server"}, St};

%% Incoming message
loop(St = #cl_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.
