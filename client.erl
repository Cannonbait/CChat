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
    #cl_st { gui = GUIName, nick = "Unknown", connected = false }.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server if unconnected to a server
loop(St, {connect, Server}) when St#cl_st.connected =:= false ->
		
		%TODO handle if atom created from Server is unregistered
		%currently badarg
		ServerAtom = list_to_atom(Server),
		ServerAtom ! {request, self(), {connect, {St#cl_st.nick, self()}}},
		receive
			{response, Response} ->
				{ok, St#cl_st{connected = ServerAtom}}
		after
			1000 ->
				{{error, server_not_reached, "Server timeout"}, St}

		end;
		
% Yell at user if he tries to connect when he is already connected
loop(St, {connect, Server}) ->
	{{error, user_already_connected, "You are already connected"}, St};

% Disconnect from server if already connected
loop(St, disconnect) when St#cl_st.connected =/= false ->
    % {ok, St} ;
	St#cl_st.connected ! {request, self(), {disconnect, self()}},
	{ok, St#cl_st{connected = false}};

% Yell at user if trying to disconnect when not connected
loop(St, disconnect) ->
	{{error, user_not_connected, "You are not connected"}, St};

% Join channel
loop(St, {join, Channel}) ->
    St#cl_st.connected ! {request, self(), {join, {self(), Channel}}},
    {ok, St} ;

%% Leave channel
loop(St, {leave, Channel}) ->
    St#cl_st.connected ! {request, self(), {leave, {self(), Channel}}},
    {ok, St} ;
    

% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
    St#cl_st.connected ! {request, self(), {message, {St#cl_st.nick, self(), Channel, Msg}}},
    {ok, St} ;

%% Get current nick
loop(St, whoami) ->
    % {"nick", St} ;
	{St#cl_st.nick, St};
	
%% Change nick
loop(St, {nick, Nick}) ->
    % {ok, St} ;
	{ok, St#cl_st{nick=Nick}};

%% Incoming message
loop(St = #cl_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.
