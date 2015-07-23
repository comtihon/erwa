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
-module(erwa_dialer_tests).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").
-include("erwa_model.hrl").
-include("erwa_service.hrl").

un_register_test() ->
  {ok, Data} = erwa_dealer_man:init(undefined),
  SessionId = 123,
  0 = get_tablesize(Data),
  {ok, ID1} = erwa_dealer_man:register(<<"proc.test1">>, #{}, SessionId, Data),
  3 = get_tablesize(Data),
  {ok, ID2} = erwa_dealer_man:register(<<"proc.test2">>, #{}, SessionId, Data),
  5 = get_tablesize(Data),
  ok = erwa_dealer_man:unregister(ID1, SessionId, Data),
  3 = get_tablesize(Data),
  {error, not_found} = erwa_dealer_man:unregister(ID1, SessionId, Data),
  ok = erwa_dealer_man:unregister(ID2, SessionId, Data),
  0 = get_tablesize(Data),
  {error, not_found} = erwa_dealer_man:unregister(ID2, SessionId, Data),
  0 = get_tablesize(Data),
  ets:delete(Data#data.ets).

unregister_all_test() ->
  {ok, Data} = erwa_dealer_man:init(undefined),
  SessionId = 123,
  0 = get_tablesize(Data),
  ok = erwa_dealer_man:unregister_all(SessionId, Data),
  0 = get_tablesize(Data),
  {ok, ID1} = erwa_dealer_man:register(<<"proc.test1">>, #{}, SessionId, Data),
  3 = get_tablesize(Data),
  {ok, ID2} = erwa_dealer_man:register(<<"proc.test2">>, #{}, SessionId, Data),
  5 = get_tablesize(Data),
  ok = erwa_dealer_man:unregister_all(SessionId, Data),
  0 = get_tablesize(Data),
  {error, not_found} = erwa_dealer_man:unregister(ID1, SessionId, Data),
  0 = get_tablesize(Data),
  {error, not_found} = erwa_dealer_man:unregister(ID2, SessionId, Data),
  0 = get_tablesize(Data),
  ok = erwa_dealer_man:unregister_all(SessionId, Data),
  0 = get_tablesize(Data),
  ets:delete(Data#data.ets).

multiple_un_register_test() ->
  erwa_test_utils:flush(),
  {ok, Data} = erwa_dealer_man:init(undefined),
  SessionId = 123,
  0 = get_tablesize(Data),
  {ok, ID1} = erwa_dealer_man:register(<<"proc.test1">>, #{}, SessionId, Data),
  % procedure       x 1
  % id_procedure    x 1
  % id_info         x 1
  3 = get_tablesize(Data),
  {ok, ID2} = erwa_dealer_man:register(<<"proc.test2">>, #{}, SessionId, Data),
  % procedure       x 2
  % id_procedure    x 2
  % id_info         x 1
  5 = get_tablesize(Data),
  MyPid = self(),
  F =
    fun() ->
      {error, procedure_already_exists} = erwa_dealer_man:register(<<"proc.test1">>, #{}, 456, Data),
      MyPid ! error_received,
      ok = receive
             try_again -> ok
           end,
      {ok, _} = erwa_dealer_man:register(<<"proc.test1">>, #{}, 456, Data),
      MyPid ! second_subscription_passed,
      ok = receive
             clean -> ok
           end,
      ok = erwa_dealer_man:unregister_all(456, Data),
      MyPid ! done,
      ok
    end,
  CPid = spawn(F),
  receive
    error_received ->
      ok
  end,
  % procedure       x 2
  % id_procedure    x 2
  % id_info         x 1
  5 = get_tablesize(Data),
  ok = erwa_dealer_man:unregister(ID1, SessionId, Data),
  % procedure       x 1
  % id_procedure    x 1
  % id_info         x 1
  3 = get_tablesize(Data),
  CPid ! try_again,
  ok = receive
         second_subscription_passed -> ok
       end,
  % procedure       x 2
  % id_procedure    x 2
  % id_info         x 2
  6 = get_tablesize(Data),
  CPid ! clean,
  ok = receive
         done -> ok
       end,
  % procedure       x 1
  % id_procedure    x 1
  % id_info         x 1
  ok = ensure_tablesize(3, Data, 1000),
  ok = erwa_dealer_man:unregister(ID2, SessionId, Data),
  % procedure       x 0
  % id_procedure    x 0
  % id_info         x 0
  0 = get_tablesize(Data),
  ok = erwa_dealer_man:unregister_all(SessionId, Data),
  % procedure       x 0
  % id_procedure    x 0
  % id_info         x 0
  0 = get_tablesize(Data),
  erwa_test_utils:flush(),
  ets:delete(Data#data.ets).

call_test() ->
  erwa_sessions_man:init(),
  erwa_invocation_sup:start_link(),
  erwa_test_utils:flush(),
  {ok, Data} = erwa_dealer_man:init(undefined),
  Realm = <<"erwa.test">>,
  MyPid = self(),
  F =
    fun() ->
      {ok, SessionId} = erwa_sessions_man:register_session(Realm),
      {ok, ProcId} = erwa_dealer_man:register(<<"proc.sum">>, #{}, SessionId, Data),
      MyPid ! subscribed,
      {ok, A, B, InvocationPid} = receive
                                    {erwa, {invocation, set_request_id, ProcId, #{invocation_pid := InvPid}, [In1, In2], undefined}} ->
                                      {ok, In1, In2, InvPid}
                                  end,
      ok = erwa_invocation:yield(InvocationPid, #{}, [A + B], undefined, SessionId),
      ok = erwa_dealer_man:unregister_all(SessionId, Data),
      ok
    end,
  CPid = spawn(F),
  {ok, SessionId} = erwa_sessions_man:register_session(Realm),
  monitor(process, CPid),
  ok = receive
         subscribed -> ok
       end,
  RequestId = erwa_support:gen_id(),
  A = erwa_support:gen_id(),
  B = erwa_support:gen_id(),
  C = A + B,
  {ok, InvocationPid} = erwa_dealer_man:call(<<"proc.sum">>, RequestId, #{}, [A, B], undefined, SessionId, Data),
  monitor(process, InvocationPid),
  ok = receive
         {erwa, {result, RequestId, #{}, [C], undefined}} -> ok
       end,
  ok = receive
         {'DOWN', _, process, CPid, normal} ->
           ok
       end,
  ok = receive
         {'DOWN', _, process, InvocationPid, _} ->
           ok
       end,
  erwa_test_utils:flush(),
  ets:delete(?SESSIONS_ETS),
  ets:delete(Data#data.ets).

caller_identification_test() ->
  erwa_sessions_man:init(),
  erwa_invocation_sup:start_link(),
  erwa_test_utils:flush(),
  Realm = <<"erwa.test">>,
  {ok, Data} = erwa_dealer_man:init(undefined),
  {ok, SessionId} = erwa_sessions_man:register_session(Realm),
  MyPid = self(),
  F =
    fun() ->
      {ok, LocalSessId} = erwa_sessions_man:register_session(Realm),
      {ok, ProcId} = erwa_dealer_man:register(<<"proc.sum">>, #{}, LocalSessId, Data),
      MyPid ! subscribed,
      {ok, A, B, InOptions} = receive
                                {erwa, {invocation, set_request_id, ProcId, Opts, [In1, In2], undefined}} ->
                                  {ok, In1, In2, Opts}
                              end,
      SessionId = maps:get(caller, InOptions),
      InvocationPid = maps:get(invocation_pid, InOptions),
      ok = erwa_invocation:yield(InvocationPid, #{}, [A + B], undefined, LocalSessId),
      ok = erwa_dealer_man:unregister_all(LocalSessId, Data),
      timer:sleep(100),
      MyPid ! done,
      ok
    end,
  spawn(F),

  ok = receive
         subscribed -> ok
       end,
  RequestId = erwa_support:gen_id(),
  A = erwa_support:gen_id(),
  B = erwa_support:gen_id(),
  C = A + B,
  {ok, InvocationPid} = erwa_dealer_man:call(<<"proc.sum">>, RequestId, #{disclose_me => true}, [A, B], undefined, SessionId, Data),
  monitor(process, InvocationPid),
  ok = receive
         {erwa, {result, RequestId, #{}, [C], undefined}} -> ok
       end,
  ok = receive
         {'DOWN', _, process, InvocationPid, _} ->
           ok
       end,
  ok = receive
         done -> ok
       end,
  erwa_test_utils:flush(),
  ets:delete(?SESSIONS_ETS),
  ets:delete(Data#data.ets).

call_cancel_test() ->
  erwa_sessions_man:init(),
  erwa_invocation_sup:start_link(),
  erwa_test_utils:flush(),
  Realm = <<"erwa.test">>,
  {ok, Data} = erwa_dealer_man:init(undefined),

  MyPid = self(),
  F =
    fun() ->
      {ok, SessionId} = erwa_sessions_man:register_session(Realm),
      {ok, ProcId} = erwa_dealer_man:register(<<"proc.sum">>, #{}, SessionId, Data),
      MyPid ! subscribed,
      {ok, InOptions} = receive
                          {erwa, {invocation, set_request_id, ProcId, Opts, _, _}} ->
                            {ok, Opts}
                        end,
      InvocationPid = maps:get(invocation_pid, InOptions),
      ok = receive
             {erwa, {interrupt, set_request_id, #{invocation_pid := InvocationPid}}} ->
               ok
           end,
      ok = erwa_invocation:error(InvocationPid, #{}, canceled, undefined, undefined, SessionId),
      ok = erwa_dealer_man:unregister_all(SessionId, Data),
      timer:sleep(100),
      MyPid ! done,
      ok
    end,
  spawn(F),
  {ok, SessionId} = erwa_sessions_man:register_session(Realm),
  ok = receive
         subscribed -> ok
       end,
  RequestId = erwa_support:gen_id(),
  A = erwa_support:gen_id(),
  B = erwa_support:gen_id(),
  {ok, InvocationPid} = erwa_dealer_man:call(<<"proc.sum">>, RequestId, #{}, [A, B], undefined, SessionId, Data),
  monitor(process, InvocationPid),
  timer:sleep(100),
  erwa_invocation:cancel(InvocationPid, []),
  ok = receive
         {erwa, {error, call, _, _, _, _, _}} -> ok
       end,
  ok = receive
         {'DOWN', _, process, InvocationPid, _} ->
           ok
       end,
  ok = receive
         done -> ok
       end,
  erwa_test_utils:flush(),
  ets:delete(?SESSIONS_ETS),
  ets:delete(Data#data.ets).

call_progressive_test() ->
  erwa_sessions_man:init(),
  erwa_invocation_sup:start_link(),
  erwa_test_utils:flush(),
  Realm = <<"erwa.test">>,
  {ok, Data} = erwa_dealer_man:init(undefined),

  MyPid = self(),
  F =
    fun() ->
      {ok, SessionId} = erwa_sessions_man:register_session(Realm),
      {ok, ProcId} = erwa_dealer_man:register(<<"proc.sum">>, #{}, SessionId, Data),
      MyPid ! subscribed,
      {ok, InOptions} = receive
                          {erwa, {invocation, set_request_id, ProcId, Opts, _, _}} ->
                            {ok, Opts}
                        end,
      InvocationPid = maps:get(invocation_pid, InOptions),
      ok = erwa_invocation:yield(InvocationPid, #{progress => true}, [234], undefined, SessionId),
      timer:sleep(50),
      ok = erwa_invocation:yield(InvocationPid, #{}, [567], undefined, SessionId),
      ok = erwa_dealer_man:unregister_all(SessionId, Data),
      ok
    end,
  spawn(F),
  {ok, SessionId} = erwa_sessions_man:register_session(Realm),
  ok = receive
         subscribed -> ok
       end,
  RequestId = erwa_support:gen_id(),
  A = erwa_support:gen_id(),
  B = erwa_support:gen_id(),
  {ok, InvocationPid} = erwa_dealer_man:call(<<"proc.sum">>, RequestId, #{receive_progress => true}, [A, B], undefined, SessionId, Data),
  monitor(process, InvocationPid),
  ok = receive
         {erwa, {result, RequestId, #{progress := true}, [234], undefined}} -> ok
       end,
  ok = receive
         {erwa, {result, RequestId, #{}, [567], undefined}} -> ok
       end,
  ok = receive
         {'DOWN', _, process, InvocationPid, _} ->
           ok
       end,
  erwa_test_utils:flush(),
  ets:delete(?SESSIONS_ETS),
  ets:delete(Data#data.ets).


%% @private
get_tablesize(#data{ets = Ets}) ->
  ets:info(Ets, size).

%% @private
ensure_tablesize(_Number, _Data, MaxTime) when MaxTime =< 0 ->
  timeout;
ensure_tablesize(Number, Data, MaxTime) ->
  case get_tablesize(Data) of
    Number -> ok;
    _ ->
      timer:sleep(10),
      NewTime = MaxTime - 10,
      ensure_tablesize(Number, Data, NewTime)
  end.