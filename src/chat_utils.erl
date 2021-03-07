%%%-------------------------------------------------------------------
%%% @author tomasz
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. mar 2021 20:20
%%%-------------------------------------------------------------------
-module(chat_utils).
-author("tomasz").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(utils_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init([]) ->
  {ok, #{}}.

%% @private
%% @doc Handling call messages
handle_call({register, Name, Socket}, _From, Participants) ->
  {Response, NewParticipants} = tryRegister(Name, Socket, Participants),
  {reply, Response, NewParticipants};
handle_call({deregister, Name}, _From, Participants) ->
  {Response, NewParticipants} = tryDeregister(Name, Participants),
  {reply, Response, NewParticipants}.

%% @private
%% @doc Handling cast messages
handle_cast({send, Sender, Message}, Participants) ->
  sendMessage(Sender, Message, maps:values(maps:without(Sender, Participants))),
  {noreply, Participants}.

%% @private
%% @doc Handling all non call/cast messages
handle_info(_Info, State = #utils_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
terminate(_Reason, _State = #utils_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State = #utils_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


tryRegister(Name, Socket, Participants) ->
  case maps:is_key(Name, Participants) of
    true ->
      {taken, Participants};
    false ->
      {welcome, Participants#{Name => Socket}}
  end.

tryDeregister(Name, Participants) ->
  case maps:is_key(Name, Participants) of
    true ->
      {deregistered, maps:without(Name, Participants)};
    false ->
      {no_such_user, Participants}
  end.

sendMessage(Sender, Message, Sockets) ->
  lists:foreach(fun(Socket) -> gen_tcp:send(Socket, "from;" ++ Sender ++ " says : " ++ Message) end, Sockets).
