%%
%% Copyright (c) 2014-2015 Bas Wegh
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
-module(erwa_broker_test).
-author("tihon").

-include("erwa_service.hrl").
-include("erwa_model.hrl").
-include_lib("eunit/include/eunit.hrl").

un_subscribe_test() ->
  {ok, Data} = erwa_broker_man:init(),
  UData = erwa_broker_man:disable_metaevents(Data),
  SessionId = erwa_support:gen_id(),
  0 = get_tablesize(UData),
  {ok, ID1} = erwa_broker_man:subscribe(<<"topic.test1">>, #{}, SessionId, UData),
  3 = get_tablesize(UData),
  {ok, ID2} = erwa_broker_man:subscribe(<<"topic.test2">>, #{}, SessionId, UData),
  5 = get_tablesize(UData),
  ok = erwa_broker_man:unsubscribe(ID1, SessionId, UData),
  3 = get_tablesize(UData),
  {error, not_found} = erwa_broker_man:unsubscribe(ID1, SessionId, Data),
  ok = erwa_broker_man:unsubscribe(ID2, SessionId, UData),
  0 = get_tablesize(UData),
  {error, not_found} = erwa_broker_man:unsubscribe(ID2, SessionId, UData),
  0 = get_tablesize(UData),
  ets:delete(UData#data.ets).

unsubscribe_all_test() ->
  {ok, Data} = erwa_broker_man:init(),
  UData = erwa_broker_man:disable_metaevents(Data),
  SessionId = erwa_support:gen_id(),
  0 = get_tablesize(UData),
  ok = erwa_broker_man:unsubscribe_all(SessionId, UData),
  0 = get_tablesize(UData),
  {ok, ID1} = erwa_broker_man:subscribe(<<"topic.test1">>, #{}, SessionId, UData),
  3 = get_tablesize(UData),
  {ok, ID2} = erwa_broker_man:subscribe(<<"topic.test2">>, #{}, SessionId, UData),
  5 = get_tablesize(UData),
  ok = erwa_broker_man:unsubscribe_all(SessionId, UData),
  0 = get_tablesize(UData),
  {error, not_found} = erwa_broker_man:unsubscribe(ID1, SessionId, UData),
  0 = get_tablesize(UData),
  {error, not_found} = erwa_broker_man:unsubscribe(ID2, SessionId, UData),
  0 = get_tablesize(UData),
  ok = erwa_broker_man:unsubscribe_all(SessionId, UData),
  0 = get_tablesize(UData),
  ets:delete(UData#data.ets).

multiple_un_subscribe_test() ->
  erwa_sessions_man:init(),
  {ok, Data} = erwa_broker_man:init(),
  UData = erwa_broker_man:disable_metaevents(Data),
  {ok, SessionId} = erwa_sessions_man:register_session(<<"erwa.test">>),
  0 = get_tablesize(UData),
  {ok, ID1} = erwa_broker_man:subscribe(<<"topic.test1">>, #{}, SessionId, UData),
  3 = get_tablesize(UData),
  {ok, ID2} = erwa_broker_man:subscribe(<<"topic.test2">>, #{}, SessionId, UData),
  5 = get_tablesize(UData),
  MyPid = self(),
  F =
    fun() ->
      {ok, S2} = erwa_sessions_man:register_session(<<"erwa.test">>),
      {ok, ID3} = erwa_broker_man:subscribe(<<"topic.test1">>, #{}, S2, UData),
      MyPid ! {first_subscription, ID3},
      timer:sleep(200),
      {ok, ID4} = erwa_broker_man:subscribe(<<"topic.test2">>, #{}, S2, UData),
      MyPid ! {second_subscription, ID4},
      timer:sleep(200),
      ok = erwa_broker_man:unsubscribe_all(S2, UData),
      MyPid ! done,
      ok
    end,
  spawn(F),
  receive
    {first_subscription, ID1} ->
      ok
  end,
  6 = get_tablesize(UData),
  receive
    {second_subscription, ID2} ->
      ok
  end,
  6 = get_tablesize(UData),
  receive
    done ->
      ok
  end,
  5 = get_tablesize(UData),
  ok = erwa_broker_man:unsubscribe(ID1, SessionId, UData),
  3 = get_tablesize(UData),
  ok = erwa_broker_man:unsubscribe_all(SessionId, UData),
  0 = get_tablesize(UData),
  ets:delete(?SESSIONS_ETS),
  ets:delete(UData#data.ets).

publish_test() ->
  {ok, _} = erwa_publications:start(),
  erwa_sessions_man:init(),
  {ok, Data} = erwa_broker_man:init(),
  UData = erwa_broker_man:disable_metaevents(Data),
  {ok, SessionId} = erwa_sessions_man:register_session(<<"erwa.test">>),
  {ok, ID} = erwa_broker_man:subscribe(<<"topic.test1">>, #{}, SessionId, UData),
  MyPid = self(),
  F =
    fun() ->
      {ok, S2} = erwa_sessions_man:register_session(<<"erwa.test">>),
      {ok, ID} = erwa_broker_man:subscribe(<<"topic.test1">>, #{}, S2, UData),
      MyPid ! subscribed,
      receive
        {erwa, {event, ID, PubId, #{}, undefined, undefined}} ->
          MyPid ! {received, PubId}
      end,
      ok = erwa_broker_man:unsubscribe_all(S2, UData),
      ok
    end,
  spawn(F),
  receive
    subscribed -> ok
  end,
  {ok, PublicationID1} = erwa_broker_man:publish(<<"topic.test1">>, #{}, undefined, undefined, SessionId, UData),
  receive
    {received, PublicationID1} -> ok
  end,
  {ok, PublicationID2} = erwa_broker_man:publish(<<"topic.test1">>, #{exclude_me=>false}, undefined, undefined, SessionId, UData),
  ok = receive
         {erwa, {event, ID, PublicationID2, #{}, undefined, undefined}} ->
           ok
       end,
  ets:delete(?SESSIONS_ETS),
  ets:delete(UData#data.ets),
  {ok, stopped} = erwa_publications:stop().

exclude_test() ->
  erwa_sessions_man:init(),
  {ok, _} = erwa_publications:start(),
  {ok, Data} = erwa_broker_man:init(),
  UData = erwa_broker_man:disable_metaevents(Data),
  {ok, SessionId1} = erwa_sessions_man:register_session(<<"erwa.test">>),
  {ok, SessionId2} = erwa_sessions_man:register_session(<<"erwa.test">>),
  {ok, ID} = erwa_broker_man:subscribe(<<"topic.test1">>, #{}, SessionId1, UData),
  MyPid = self(),
  F = fun() ->
    {ok, ID} = erwa_broker_man:subscribe(<<"topic.test1">>, #{}, SessionId2, UData),
    MyPid ! subscribed,
    Received = receive
                 {erwa, {event, ID, _, #{}, undefined, undefined}} ->
                   true;
                 got_something ->
                   MyPid ! nothing,
                   false
               end,
    case Received of
      true ->
        receive
          {got_something} ->
            MyPid ! yes_got_it
        end;
      false ->
        ok
    end,
    ok = erwa_broker_man:unsubscribe_all(SessionId2, UData),
    MyPid ! done,
    ok
  end,
  ClientPid = spawn(F),
  receive
    subscribed -> ok
  end,
  {ok, PubID} = erwa_broker_man:publish(<<"topic.test1">>,
    #{exclude_me => false, exclude => [SessionId2]}, undefined, undefined, SessionId1, UData),
  ok = receive
         {erwa, {event, ID, PubID, #{}, undefined, undefined}} ->
           ok
       end,
  timer:sleep(100),
  ClientPid ! got_something,
  ok = receive
         nothing -> ok;
         yes_got_it -> wrong
       end,
  ets:delete(?SESSIONS_ETS),
  ets:delete(UData#data.ets),
  {ok, stopped} = erwa_publications:stop().

eligible_test() ->
  erwa_sessions_man:init(),
  {ok, _} = erwa_publications:start(),
  {ok, Data} = erwa_broker_man:init(),
  UData = erwa_broker_man:disable_metaevents(Data),
  {ok, SessionId1} = erwa_sessions_man:register_session(<<"erwa.test">>),
  {ok, SessionId2} = erwa_sessions_man:register_session(<<"erwa.test">>),

  {ok, ID} = erwa_broker_man:subscribe(<<"topic.test1">>, #{}, SessionId1, UData),
  MyPid = self(),
  F =
    fun() ->
      {ok, ID} = erwa_broker_man:subscribe(<<"topic.test1">>, #{}, SessionId2, UData),
      MyPid ! subscribed,
      Received = receive
                   {erwa, {event, ID, _, [], undefined, undefined}} ->
                     true;
                   got_something ->
                     MyPid ! nothing,
                     false
                 end,
      case Received of
        true ->
          receive
            {got_something} ->
              MyPid ! yes_got_it
          end;
        false ->
          ok
      end,
      ok = erwa_broker_man:unsubscribe_all(SessionId2, UData),
      MyPid ! done,
      ok
    end,
  ClientPid = spawn(F),
  receive
    subscribed -> ok
  end,
  {ok, PubID} = erwa_broker_man:publish(<<"topic.test1">>,
    #{exclude_me=>false, eligible=>[SessionId1]}, undefined, undefined, SessionId1, UData),
  ok = receive
         {erwa, {event, ID, PubID, #{}, undefined, undefined}} ->
           ok
       end,
  timer:sleep(100),
  ClientPid ! got_something,
  ok = receive
         nothing -> ok;
         yes_got_it -> wrong
       end,
  ets:delete(?SESSIONS_ETS),
  ets:delete(UData#data.ets),
  {ok, stopped} = erwa_publications:stop().


%% @private
get_tablesize(#data{ets = Ets}) ->
  ets:info(Ets, size).