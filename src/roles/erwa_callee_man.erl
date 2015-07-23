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

-module(erwa_callee_man).

-include("erwa_model.hrl").

-export([init/2, empty_result/4, get_sessions_ids/1]).

-define(PROCEDURES,
  [
    {<<"wamp.subscription.list">>, fun subscription_list/4},
    {<<"wamp.subscription.lookup">>, fun subscription_lookup/4},
    {<<"wamp.registration.list">>, fun registration_list/4},
    {<<"wamp.registration.lookup">>, fun registration_lookup/4},
    {<<"wamp.session.count">>, fun session_count/4},
    {<<"wamp.session.list">>, fun session_list/4}
  ]).

-spec init(Dealer :: #data{}, Realm :: binary()) -> {map(), binary()}.
init(Dealer, Realm) ->
  {ok, SessionId} = erwa_sessions_man:register_session(Realm),
  F =
    fun({Method, Fun}, Map) ->
      {ok, RegId} = erwa_dealer_man:register(Method, #{invoke => single}, SessionId, Dealer),
      maps:put(RegId, Fun, Map)
    end,
  Mapping = lists:foldl(F, #{}, ?PROCEDURES),
  {Mapping, SessionId}.

empty_result(_, _, _, _) ->
  {#{}, undefined, undefined}.

%% Exported to be used in test.
get_sessions_ids(Ets) ->
  ExtractId =
    fun(#pid_info{id = Id}, IdList) ->
      [Id | IdList]
    end,
  ets:foldl(ExtractId, [], Ets).


%% @private
session_count(_, _, _, {Ets, _, _}) ->
  Count = ets:info(Ets, size),
  {ok, #{}, [Count], undefined}.

%% @private
session_list(_Options, _Arguments, _ArgumentsKw, {Ets, _, _}) ->
  Ids = get_sessions_ids(Ets),
  {ok, #{}, Ids, undefined}.

%% @private
subscription_list(_Options, _Arguments, _ArgumentsKw, {_, Broker, _}) ->
  {ok, List} = erwa_broker_man:get_subscriptions(Broker),
  {ok, #{}, [List], undefined}.

%% @private
subscription_lookup(_Options, [SubscriptionId], _ArgumentsKw, {_, Broker, _}) ->
  case erwa_broker_man:get_subscription(Broker, SubscriptionId) of
    {ok, Details} ->
      {ok, #{}, [Details], undefined};
    {error, not_found} ->
      {error, #{}, invalid_argument, undefined, undefined}
  end.

%% @private
registration_list(_Options, _Arguments, _ArgumentsKw, {_, _, Dealer}) ->
  {ok, List} = erwa_dealer_man:get_registrations(Dealer),
  {ok, #{}, [List], undefined}.

%% @private
registration_lookup(_Options, [RegistrationId], _ArgumentsKw, {_, _, Dealer}) ->
  case erwa_dealer_man:get_registration(Dealer, RegistrationId) of
    {ok, Details} ->
      {ok, #{}, [Details], undefined};
    {error, not_found} ->
      {error, #{}, invalid_argument, undefined, undefined}
  end.