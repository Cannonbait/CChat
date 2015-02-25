-module(client).
-export([main/1, initial_state/2]).
-import(helper, [request/2, requestAsync/2]).
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
				requestAsync(ServerAtom, {connect, {St#cl_st.nick, self()}}),
				receive
					{result, Ref, {connect, ok}} ->
						{ok, St#cl_st{connected = ServerAtom}};
					{result, Ref, {connect, user_already_connected}} ->
						{{error, user_already_connected, "Name taken, change with /nick <username>"}, St}
				after
					3000 ->
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
    case request(St#cl_st.connected, {disconnect, self()}) of
        {disconnect, leave_channels_first} ->
            {{error, leave_channels_first, "Leave your channels first"}, St};
		{disconnect, ok} ->
            {ok, St#cl_st{connected = false}}
    end;
	

% Yell at user if trying to disconnect when not connected
loop(St, disconnect) ->
	{{error, user_not_connected, "You are not connected"}, St};

% Join channel
loop(St, {join, Channel}) ->
    case request(St#cl_st.connected, {join, {self(), Channel}}) of
        {join, user_already_joined} ->
            {{error, user_already_joined, "You are already in channel"}, St};
        {join, ok} ->
            {ok, St}
    end;


%% Leave channel
loop(St, {leave, Channel}) ->
    case request(St#cl_st.connected, {leave, {self(), Channel}}) of
        {leave, user_not_joined} ->
            {{error, user_not_joined, "You did not join the channel"}, St};
        {leave, ok} ->
            {ok, St}
    end;


% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
    case request(St#cl_st.connected, {message, {St#cl_st.nick, self(), Channel, Msg}}) of
        {message, user_not_joined} ->
            {{error, user_not_joined, "You need to join the channel"}, St};
        {message, ok} ->
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
	{{error, user_already_connected, "You can't change nick when connected to a server"}, St};

%% Incoming message
loop(St = #cl_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.
