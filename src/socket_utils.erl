%%%-------------------------------------------------------------------
%%% @author tomasz
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. mar 2021 20:37
%%%-------------------------------------------------------------------
-module(socket_utils).
-author("tomasz").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, accept/1, acceptLoop/1]).

-define(SERVER, ?MODULE).

-record(server_state, {address, port, lsocket}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
start_link(Port, Address) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, #server_state{address = Address, port = Port}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
init(State = #server_state{port = Port}) ->
  {ok, LSocket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
  NewState = State#server_state{lsocket = LSocket},
  accept(NewState),
  {ok, NewState}.

%% @private
%% @doc Handling cast messages
handle_cast(accepted, State = #server_state{}) ->
  {noreply, accept(State)}.



%% @private
%% @doc Handling call messages
handle_call(_Request, _From, State = #server_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling all non call/cast messages
handle_info(_Info, State = #server_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
terminate(_Reason, _State = #server_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State = #server_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
accept(State) ->
  proc_lib:spawn(?MODULE, acceptLoop, [State]),
  State.

acceptLoop(State = #server_state{lsocket = LSocket}) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  gen_server:cast(socket_utils, accepted),
  chat:beginLoop(Socket).