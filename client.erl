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
    #cl_st { gui = GUIName, nick = Nick, connected = false, channels = []}.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server if unconnected to a server
loop(St, {connect, Server}) when St#cl_st.connected =:= false ->
		
		ServerAtom = list_to_atom(Server),
		case lists:member(ServerAtom, registered()) of
			true ->
				%Send a connect requist to server
				requestAsync(ServerAtom, {connect, {St#cl_st.nick, self()}}),
				receive
					%If not connected but got response, update client state with connected server
					{result, Ref, {connect, ok}} ->
						{ok, St#cl_st{connected = ServerAtom}};
					%If nickname is already present on the server, tell user to change nick
					{result, Ref, {connect, user_already_connected}} ->
						{{error, user_already_connected, "Name taken, change with /nick <username>"}, St}
				after
					3000 ->
						{{error, server_not_reached, "Server timeout"}, St}
				end;
			false ->
				{{error, server_not_reached, "Could not connect to server"}, St}
		end;

		
% Yell at user if he tries to connect when he is already connected
loop(St, {connect, Server}) ->
	{{error, user_already_connected, "You are already connected"}, St};

% Disconnect from server if already connected
loop(St, disconnect) when St#cl_st.connected =/= false, St#cl_st.channels =:= [] ->
    % {ok, St} ;
	% Tell server to disconnect client
    request(St#cl_st.connected, {disconnect, self()}),
    {ok, St#cl_st{connected = false}};
	

loop(St, disconnect) when St#cl_st.connected =/= false ->
    {{error, leave_channels_first, "Leave your channels first"}, St};


% Yell at user if trying to disconnect when not connected
loop(St, disconnect) ->
	{{error, user_not_connected, "You are not connected"}, St};

% Join channel
loop(St, {join, Channel}) ->
	ChannelAtom = list_to_atom(Channel),

	%If client is already member of channel...
	case lists:member(ChannelAtom, St#cl_st.channels) of 
		%Then tell user that he already joined the channel
		true->
			{{error, user_already_joined, "You have already joined this channel"}, St};
		false->
		case lists:member(ChannelAtom, registered()) of
			true ->
				requestAsync(ChannelAtom, {join, {self()}}),
				{ok, St#cl_st{channels = [ChannelAtom | St#cl_st.channels]}};
			false->
				%Send request to create and join a channel
				{Op, Value} = request(St#cl_st.connected, {join, {self(), Channel}}),
				%The request should either throw exception or return ok
				{Value, St#cl_st{channels = [ChannelAtom | St#cl_st.channels]}}
		  end
	end;


%% Leave channel
loop(St, {leave, Channel}) ->
    ChannelAtom = list_to_atom(Channel),
    case lists:member(ChannelAtom, registered()) of
        true ->
            case request(ChannelAtom, {leave, {self()}}) of
                {leave, ok} ->
                    {ok, St#cl_st{channels = lists:delete(ChannelAtom, St#cl_st.channels)}};
                {leave, user_not_joined} ->
                    {{error, user_not_joined, "You did not join the channel"}, St}
            end;
        false ->
            {{error, user_not_joined, "You did not join the channel"}, St}
    end;


% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
    ChannelAtom = list_to_atom(Channel),
    case lists:member(ChannelAtom, registered()) of
        true ->
            case request(ChannelAtom, {message, {St#cl_st.nick, self(), Channel, Msg}}) of
                {message, ok} ->
                    {ok, St};
                {message, user_not_joined} ->
                    {{error, user_not_joined, "You did not join the channel"}, St}
            end;
        false ->
            {{error, user_not_joined, "You did not join the channel"}, St}
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
