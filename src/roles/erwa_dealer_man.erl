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
%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erwa_dealer_man).

-include("erwa_model.hrl").
-include("erwa_service.hrl").

-export([
  init/1,
  register/4,
  unregister/3,
  unregister_all/2,
  call/7,
  enable_metaevents/1,
  disable_metaevents/1,
  get_registrations/1,
  get_registration/2]).

-spec init(BrokerState :: #data{}) -> {ok, #data{}}.
init(BrokerState) ->
  Ets = ets:new(?RPC_ETS, [{keypos, #id_topic.id}, public, {read_concurrency, true}, {write_concurrency, true}]),
  {ok, #data{ets = Ets, features = ?DEALER_FEATURES, broker = BrokerState}}.

-spec enable_metaevents(#data{}) -> #data{}.
enable_metaevents(Data) ->
  Data#data{meta_events = true}.

-spec disable_metaevents(#data{}) -> #data{}.
disable_metaevents(Data) ->
  Data#data{meta_events = false}.

-spec get_registrations(#data{}) -> map().
get_registrations(#data{ets = Ets}) ->
  ExactUris = ets:match(Ets, #procedure{match = exact, id = '$1', uri = '$2', _ = '_'}),
  Filter =
    fun([Id, Uri], List) ->
      case binary:part(Uri, {0, 5}) == <<"wamp.">> of
        true -> List;
        false -> [Id | List]
      end
    end,
  Exact = lists:foldl(Filter, [], ExactUris),
  Prefix = lists:flatten(ets:match(Ets, #procedure{match = prefix, id = '$1', _ = '_'})),
  Wildcard = lists:flatten(ets:match(Ets, #procedure{match = wildcard, id = '$1', _ = '_'})),
  {ok, #{exact => Exact, prefix => Prefix, wildcard => Wildcard}}.

-spec get_registration(#data{}, non_neg_integer()) -> map().
get_registration(#data{ets = Ets}, Id) ->
  case ets:lookup(Ets, Id) of
    [#id_procedure{uri = Uri}] ->
      [#procedure{uri = Uri, invoke = Invoke, id = Id, match = Match, created = Created}] = ets:lookup(Ets, Uri),
      {ok, #{uri => Uri, invoke => Invoke, id => Id, match => Match, created => Created}};
    [] ->
      {error, not_found}
  end.

-spec register(ProcedureUri :: binary(), Options :: map(), SessionId :: non_neg_integer(), Data :: #data{}) ->
  {ok, RegistrationId :: non_neg_integer()}.
register(ProcedureUri, Options, SessionId, Data) ->
  register_procedure(ProcedureUri, Options, SessionId, Data).

-spec unregister(RegistrationId :: non_neg_integer(), SessionId :: non_neg_integer(), #data{}) -> ok.
unregister(RegistrationId, SessionId, Data) ->
  unregister_procedure(RegistrationId, SessionId, Data).

-spec unregister_all(SessionId :: non_neg_integer(), #data{}) -> ok.
unregister_all(SessionId, Data) ->
  unregister_all_for(SessionId, Data).

-spec call(Uri :: binary(), RequestId :: non_neg_integer(), Options :: map(),
    Arguments :: list(), ArgumentsKw :: map(), Session :: term(), Data :: #data{}) ->
  {ok, pid()} | {error, invocation_failed} | {error, procedure_not_found}.
call(Uri, RequestId, Options, Arguments, ArgumentsKw, SessionId, #data{ets = Ets}) ->
  case ets:lookup(Ets, Uri) of
    [#procedure{uri = Uri, ids = Ids, id = ID}] ->
      CallInfo = #{procedure_id => ID,
        caller_id => SessionId,
        call_req_id => RequestId,
        call_options => Options,
        call_arguments => Arguments,
        call_argumentskw => ArgumentsKw,
        callee_ids => Ids
      },
      case erwa_invocation_sup:start_invocation(CallInfo) of
        {ok, Pid} ->
          {ok, Pid};
        _ -> {error, invocation_failed}
      end;
    [] ->
      {error, procedure_not_found}
  end.


%% @private
-spec register_procedure(ProcedureUri :: binary(), Options :: map(), SessionId :: non_neg_integer(), Data :: #data{}) ->
  {ok, non_neg_integer()} | {error, procedure_already_exists}.
register_procedure(ProcedureUri, Options, SessionId, Data = #data{ets = Ets}) ->
  case ets:lookup(Ets, ProcedureUri) of
    [#procedure{uri = ProcedureUri}] ->
      {error, procedure_already_exists};
    [] ->
      {ok, ProcedureId, ProcDetails} = create_procedure(ProcedureUri, Options, SessionId, Ets),
      ok = add_proc_to_id(ProcedureUri, SessionId, Ets),
      publish_metaevent(on_create, ProcedureUri, SessionId, ProcDetails, Data),
      publish_metaevent(on_register, ProcedureUri, SessionId, ProcedureId, Data),
      {ok, ProcedureId}
  end.

%% @private
-spec create_procedure(Url :: binary(), Options :: map(), SessionId :: non_neg_integer(), Ets :: ets:tid()) ->
  {ok, non_neg_integer()}.
create_procedure(Uri, Options, SessionId, Ets) ->
  ProcedureId = erwa_support:gen_id(),
  Invoke = maps:get(invoke, Options, single),
  Match = maps:get(match, Options, exact),
  Created = erlang:universaltime(),
  case ets:insert_new(Ets, [#id_procedure{id = {proc, ProcedureId}, uri = Uri},
    #procedure{uri = Uri, id = ProcedureId, ids = [SessionId],
      match = Match, invoke = Invoke, options = Options, created = Created}]) of
    true ->
      {ok, ProcedureId, #{id => ProcedureId,
        created => cowboy_clock:rfc1123(Created),
        uri => Uri,
        invoke => Invoke,
        match => Match}};
    _ ->
      create_procedure(Uri, Options, SessionId, Ets)
  end.

%% @private
unregister_procedure(RegistrationId, SessionId, Data = #data{ets = Ets}) when is_integer(RegistrationId) ->
  case ets:lookup(Ets, {proc, RegistrationId}) of
    [] ->
      {error, not_found};
    [#id_procedure{uri = Uri}] ->
      unregister_procedure(Uri, SessionId, Data)
  end;
unregister_procedure(Uri, SessionId, Data = #data{ets = Ets}) when is_binary(Uri) ->
  [#procedure{uri = Uri, id = ID, ids = Ids} = Proc] = ets:lookup(Ets, Uri),
  case lists:member(SessionId, Ids) of
    false ->
      {error, not_registered};
    true ->
      ok = remove_proc_from_id(Uri, SessionId, Data),
      publish_metaevent(on_unregister, Uri, SessionId, ID, Data),
      case lists:delete(SessionId, Ids) of
        [] ->
          true = ets:delete(Ets, {proc, ID}),
          true = ets:delete(Ets, Uri),
          publish_metaevent(on_delete, Uri, SessionId, ID, Data);
        NewIds ->
          true = ets:insert(Ets, Proc#procedure{ids = NewIds})
      end,
      ok
  end.

%% @private
-spec unregister_all_for(Pid :: pid(), State :: #data{}) -> ok | {error, Reason :: term()}.
unregister_all_for(SessionId, Data = #data{ets = Ets}) ->
  case ets:lookup(Ets, {sess, SessionId}) of
    [#id_dialer_info{procs = Procs}] ->
      lists:foreach(fun(Uri) -> ok = unregister_procedure(Uri, SessionId, Data) end, Procs);
    [] ->
      ok
  end.

%% @private
add_proc_to_id(Uri, SessionId, Ets) ->
  case ets:lookup(Ets, {sess, SessionId}) of
    [#id_dialer_info{procs = Procs} = IdInf] ->
      true = ets:insert(Ets, IdInf#id_dialer_info{procs = [Uri | lists:delete(Uri, Procs)]}),
      ok;
    [] ->
      IdInf = #id_dialer_info{id = {sess, SessionId}, procs = [Uri]},
      true = ets:insert_new(Ets, IdInf),
      ok
  end.

%% @private
remove_proc_from_id(Uri, SessionId, #data{ets = Ets}) ->
  [#id_dialer_info{procs = Procs} = IdInf] = ets:lookup(Ets, {sess, SessionId}),
  case lists:delete(Uri, Procs) of
    [] ->
      true = ets:delete(Ets, {sess, SessionId});
    NewProcs ->
      true = ets:insert(Ets, IdInf#id_dialer_info{procs = NewProcs})
  end,
  ok.

%% @private
publish_metaevent(_, _, _, _, #data{broker = undefined}) -> ok;
publish_metaevent(_, _, _, _, #data{meta_events = disabled}) -> ok;
publish_metaevent(Event, ProcedureUri, SessionId, SecondArg, #data{broker = Broker}) ->
  case binary:part(ProcedureUri, {0, 5}) == <<"wamp.">> of
    true ->
      % do not fire metaevents on "wamp.*" uris
      ok;
    false ->
      MetaTopic = case Event of
                    on_create -> <<"wamp.registration.on_create">>;
                    on_register -> <<"wamp.registration.on_register">>;
                    on_unregister -> <<"wamp.registration.on_unregister">>;
                    on_delete -> <<"wamp.registration.on_delete">>
                  end,
      {ok, _} = erwa_broker_man:publish(MetaTopic, #{}, [SessionId, SecondArg], undefined, no_session, Broker)
  end,
  ok.