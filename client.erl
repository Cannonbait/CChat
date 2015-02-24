-module(client).
-export([main/1, initial_state/2]).
-include_lib("./defs.hrl").

%% Receive messages from GUI and handle them accordingly
main(State) ->
    receive
        {request, From, Ref, Request} ->
            {Response, NextState} = loop(State, Request),
            From ! {result, Ref, Response},
            main(NextState)
		%{response, Response} ->

    end.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = "Unknown", connected = false }.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server
loop(St, {connect, Server}) when St#cl_st.connected =:= false ->
		
		%TODO handle if atom created from Server is unregistered
		%currently badarg
		list_to_atom(Server) ! {request, self(), {connect, St#cl_st.nick}},
		receive
			{response, Response} ->
				{ok, St#cl_st{connected = true}}
		after
			1000 ->
				{{error, server_not_reached, "Server timeout"}, St}

		end;
		

loop(St, {connect, Server}) ->
	{{error, user_already_connected, "You are already connected dumbass"}, St};

%% Disconnect from server
loop(St, disconnect) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

% Join channel
loop(St, {join, Channel}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
loop(St, {leave, Channel}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

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
