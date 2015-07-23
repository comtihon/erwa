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
-module(erwa_routing_test).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").
-include("erwa_service.hrl").
-include("erwa_model.hrl").

start_stop_test() ->
  erwa_sessions_man:init(),
  {ok, Pid} = erwa_routing:start(),
  {ok, stopped} = erwa_routing:stop(Pid),
  ets:delete(?SESSIONS_ETS).

simple_routing_test() ->
  erwa_sessions_man:init(),
  {ok, Pid} = erwa_routing:start(),
  Session = #session{id = 234},
  ok = erwa_routing:connect(Pid, Session),
  {ok, _} = erwa_routing:get_dealer(Pid),
  {ok, _} = erwa_routing:get_broker(Pid),
  ok = erwa_routing:disconnect(Pid),
  {ok, stopped} = erwa_routing:stop(Pid),
  ets:delete(?SESSIONS_ETS).

forced_connection_test() ->
  erwa_sessions_man:init(),
  {ok, Pid} = erwa_routing:start(),
  {error, not_connected} = erwa_routing:get_broker(Pid),
  {error, not_connected} = erwa_routing:get_dealer(Pid),
  {ok, stopped} = erwa_routing:stop(Pid),
  ets:delete(?SESSIONS_ETS).

meta_api_test() ->
  erwa_sessions_man:init(),
  {ok, Pid} = erwa_routing:start(),
  Session = #session{id = 234},
  EtsList = erwa_test_utils:get_ets(Pid),
  Ets = proplists:get_value(?CONNECTIONS_ETS, EtsList),
  ok = erwa_routing:connect(Pid, Session),
  1 = ets:info(Ets, size),
  [234] = erwa_callee_man:get_sessions_ids(Ets),
  ok = erwa_routing:disconnect(Pid),
  {ok, stopped} = erwa_routing:stop(Pid),
  ets:delete(?SESSIONS_ETS).

garbage_test() ->
  erwa_sessions_man:init(),
  {ok, Pid} = erwa_routing:start(),
  ignored = gen_server:call(Pid, some_garbage),
  ok = gen_server:cast(Pid, some_garbage),
  Pid ! some_garbage,
  {ok, stopped} = erwa_routing:stop(Pid),
  ets:delete(?SESSIONS_ETS).