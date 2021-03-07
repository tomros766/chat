%%%-------------------------------------------------------------------
%%% @author tomasz
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. mar 2021 12:00
%%%-------------------------------------------------------------------
-module(udp_utils).
-author("tomasz").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(udp_utils_state, {}).

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
  {ok, #udp_utils_state{}}.

%% @private
%% @doc Handling call messages
handle_call(_Request, _From, State = #udp_utils_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
handle_cast(_Request, State = #udp_utils_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
handle_info(_Info, State = #udp_utils_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
terminate(_Reason, _State = #udp_utils_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, State = #udp_utils_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
