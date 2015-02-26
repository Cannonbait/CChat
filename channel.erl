-module(channel).
-export([main/1, initial_state/1, sendMessage/2]).
-include_lib("./defs.hrl").


main(State) ->
	receive
		{request, From, Ref, Request} ->
			{Response, NextState} = loop(State, Request),
			From ! {result, Ref, Response},
			main(NextState)
	end.

initial_state(Id) ->
	%When the server is started we have no users
    #channel_st{users=[Id]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 		INTERNAL

%Put user in channel
loop(State, {join, {Id}}) ->
	{{join, ok}, State#channel_st{users = lists:sort([Id| State#channel_st.users])}};

%Recieve message from user
loop(State, {message, {Nick, Id, Channel, Msg}}) ->
	%Spawn new process to send messages
	spawn(channel, sendMessage, [State#channel_st.users, {Channel, Nick, Msg, Id}]),
	%Acknowledge that server recieved message and it will be sent out
	{{message, ok}, State};


%User wants to leave a channel
loop(State, {leave, {Id}}) ->
	{{leave, ok}, State#channel_st{users = lists:delete(Id, State#channel_st.users)}}.

%Recursive function to send Message
sendMessage([], _) -> ok;
sendMessage([H|T], {Channel, Name, Msg, H}) -> 
	sendMessage(T, {Channel, Name, Msg, H});
sendMessage([H|T], {Channel, Name, Msg, Id}) ->
	H ! {message, {incoming_msg, Channel, Name, Msg}},
	sendMessage(T, {Channel, Name, Msg, Id}).



