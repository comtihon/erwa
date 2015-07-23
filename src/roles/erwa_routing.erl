%%
%% Copyright (c) 2015 Bas Wegh
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%

-module(erwa_routing).
-behaviour(gen_server).

-include("erwa_model.hrl").
-include("erwa_service.hrl").

%% API
-export([start/0]).
-export([start_link/1]).
-export([shutdown/1]).
-export([stop/1]).

-export([connect/2]).
-export([disconnect/1]).

-export([get_broker/1]).
-export([get_dealer/1]).

-export([enable_metaevents/1]).
-export([disable_metaevents/1]).

%% gen_server
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-record(state, {
  realm_name = unknown,
  broker = unknown,
  dealer = unknown,

  %api_pid = unknown,
  con_ets = none,
  going_down = false,
  timer_ref = none,
  meta_events = enabled,
%%   Former erwa_callee
  sess_id = unknown,
  mapping = #{}
}).

-define(SHUTDOWN_TIMEOUT, 30000).

start() ->
  gen_server:start(?MODULE, [], []).

start_link(RealmName) ->
  gen_server:start_link(?MODULE, RealmName, []).

-spec get_broker(pid()) -> {ok, term()} | {error, going_down}.
get_broker(Pid) ->
  gen_server:call(Pid, get_broker).

-spec get_dealer(pid()) -> {ok, term()} | {error, going_down}.
get_dealer(Pid) ->
  gen_server:call(Pid, get_dealer).

-spec connect(pid(), Session :: term()) -> ok | {error, going_down}.
connect(Pid, Session) ->
  gen_server:call(Pid, {connect, Session}).

-spec disconnect(pid() | none) -> ok | {error, going_down}.
disconnect(none) -> ok;
disconnect(Pid) ->
  gen_server:call(Pid, disconnect).

-spec enable_metaevents(pid()) -> ok.
enable_metaevents(Pid) ->
  gen_server:call(Pid, enable_metaevents).

-spec disable_metaevents(pid()) -> ok.
disable_metaevents(Pid) ->
  gen_server:call(Pid, disable_metaevents).

-spec shutdown(pid()) -> ok | {error, going_down}.
shutdown(Pid) ->
  gen_server:call(Pid, shutdown).

stop(Pid) ->
  gen_server:call(Pid, stop).

%% gen_server.

init(RealmName) ->
  Ets = ets:new(?CONNECTIONS_ETS, [{keypos, #pid_info.pid}, protected]),
  {ok, Broker} = erwa_broker_man:init(),
  {ok, Dealer} = erwa_dealer_man:init(Broker),
  {Mapping, SessionId} = erwa_callee_man:init(Dealer, RealmName),
  {ok, #state{con_ets = Ets, broker = Broker, dealer = Dealer, realm_name = RealmName,
    mapping = Mapping, sess_id = SessionId}}.

handle_call(stop, _From, State) ->
  ok = close_routing(State),
  {stop, normal, {ok, stopped}, State};
handle_call(disconnect, {Pid, _Ref}, #state{con_ets = Ets, going_down = GoDown, timer_ref = TRef} = State) ->
  case ets:lookup(Ets, Pid) of
    [#pid_info{pid = Pid, id = SessionId}] ->
      publish_metaevent(on_leave, SessionId, State);
    _ ->
      ok
  end,
  true = ets:delete(Ets, Pid),
  NewTRef = case {GoDown, ets:info(Ets, size)} of
              {true, 0} ->
                _ = timer:cancel(TRef),
                {ok, TR} = timer:send_after(1, timeout_force_close),
                TR;
              _ -> TRef
            end,
  {reply, ok, State#state{timer_ref = NewTRef}};
handle_call(enable_metaevents, _From, State) ->
  {reply, ok, State#state{meta_events = enabled}};
handle_call(disable_metaevents, _From, State) ->
  {reply, ok, State#state{meta_events = disabled}};
handle_call(_, _, #state{going_down = true} = State) ->
  {reply, {error, going_down}, State};
handle_call({connect, #session{id = SessionId}}, {Pid, _Ref}, #state{con_ets = Ets, realm_name = Realm} = State) ->
  case ets:lookup(Ets, Pid) of
    [] ->
      %TODO: more data should be sent, which are really needed and useful:
      % - authid
      % - authmethod
      % - authprovider
      % - authrole
      % - transport
      publish_metaevent(on_join, #{realm => Realm, session => SessionId}, State);
    _ -> ok
  end,
  true = ets:insert(Ets, #pid_info{pid = Pid, id = SessionId}),
  {reply, ok, State};
handle_call(get_broker, {Pid, _}, #state{broker = Broker, con_ets = Ets} = State) ->
  case ets:lookup(Ets, Pid) of
    [] ->
      {reply, {error, not_connected}, State};
    _ ->
      {reply, {ok, Broker}, State}
  end;
handle_call(get_dealer, {Pid, _}, #state{dealer = Dealer, con_ets = Ets} = State) ->
  case ets:lookup(Ets, Pid) of
    [] ->
      {reply, {error, not_connected}, State};
    _ ->
      {reply, {ok, Dealer}, State}
  end;
handle_call(shutdown, _From, #state{con_ets = Ets} = State) ->
  case ets:info(Ets, size) of
    0 ->
      {ok, TRef} = timer:send_after(1, timeout_force_close),
      {reply, ok, State#state{going_down = true, timer_ref = TRef}};
    _ ->
      ok = send_all_clients(routing_closing, State),
      {ok, TRef} = timer:send_after(?SHUTDOWN_TIMEOUT, timeout_force_close),
      {reply, ok, State#state{going_down = true, timer_ref = TRef}}
  end;
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({erwa, {invocation, _, ProcedureId, Options, Arguments, ArgumentsKw}},
    State = #state{sess_id = SessionId, mapping = Mapping, con_ets = Ets, broker = Broker, dealer = Dealer}) ->
  #{invocation_pid := InvocPid} = Options,
  Fun = maps:get(ProcedureId, Mapping, fun erwa_callee_man:empty_result/4),
  case Fun(Options, Arguments, ArgumentsKw, {Ets, Broker, Dealer}) of
    {ok, OutOptions, OutArguments, OutArgumentsKw} ->
      ok = erwa_invocation:yield(InvocPid, OutOptions, OutArguments, OutArgumentsKw, SessionId);
    {error, ErrDetails, ErrorUri, ErrArguments, ErrArgumentsKw} ->
      ok = erwa_invocation:error(InvocPid, ErrDetails, ErrorUri, ErrArguments, ErrArgumentsKw, SessionId)
  end,
  {noreply, State};
handle_info(timeout_force_close, State) ->
  close_routing(State),
  {stop, normal, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  erwa_sessions_man:unregister_session(),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% @private
close_routing(#state{timer_ref = TRef} = State) ->
  timer:cancel(TRef),
  send_all_clients(shutdown, State),
  ok.

%% @private
send_all_clients(Msg, #state{con_ets = Con}) ->
  % should here erwa_session:send_message_to be used?
  % yet routing should never be sending anything to another router ... so not for now
  ok = ets:foldl(fun(#pid_info{pid = Pid}, ok) -> Pid ! {erwa, Msg}, ok end, ok, Con).

%% @private
publish_metaevent(_, _, #state{broker = unknown}) -> ok;
publish_metaevent(_, _, #state{meta_events = disabled}) -> ok;
publish_metaevent(Event, Arg, #state{broker = Broker}) ->
  MetaTopic = case Event of
                on_join -> <<"wamp.session.on_join">>;
                on_leave -> <<"wamp.session.on_leave">>
              end,
  {ok, _} = erwa_broker_man:publish(MetaTopic, #{}, [Arg], undefined, no_session, Broker),
  ok.