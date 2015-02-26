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


loop(State, {join, {Id}}) ->
	Users = State#channel_st.users,
	case lists:member(Id, Users) of
		false -> %If user is not in channel
			{{join, ok}, State#channel_st{users = [Id| Users]}};
		true ->	%If user is already in channel, return error
			{{join, user_already_joined}, State}
	end;

%Recieve message from user
loop(State, {message, {Nick, Id, Channel, Msg}}) ->
	case lists:member(Id, State#channel_st.users) of
		false ->	%If user is not in channel, return error
			{{message, user_not_joined}, State};
		true ->
			%Spawn new process to send messages
			spawn(channel, sendMessage, [State#channel_st.users, {Channel, Nick, Msg, Id}]),
			%Acknowledge that server recieved message and it will be sent out
			{{message, ok}, State}
	end;


%User wants to leave a channel
loop(State, {leave, {Id}}) ->
	%If the channel contains the user
	case lists:member(Id, State#channel_st.users) of
		true ->
			{{leave, ok}, State#channel_st{users = lists:delete(Id, State#channel_st.users)}};
		false ->
			{{leave, user_not_joined}, State}
	end.




%Recursive function to send Message
sendMessage([], _) -> ok;
sendMessage([H|T], {Channel, Name, Msg, H}) -> 
	sendMessage(T, {Channel, Name, Msg, H});
sendMessage([H|T], {Channel, Name, Msg, Id}) ->
	H ! {message, {incoming_msg, Channel, Name, Msg}},
	sendMessage(T, {Channel, Name, Msg, Id}).



