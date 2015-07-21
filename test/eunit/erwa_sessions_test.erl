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
-module(erwa_sessions_test).
-author("tihon").

-include_lib("eunit/include/eunit.hrl").
-include("erwa_service.hrl").

simple_test() ->
  erwa_sessions_man:init(),
  0 = get_tablesize(),
  {ok, _} = erwa_sessions_man:register_session(<<"test_realm">>),
  2 = get_tablesize(),
  ok = erwa_sessions_man:unregister_session(),
  0 = get_tablesize(),
  ets:delete(?SESSIONS_ETS).

die_test() ->
  erwa_sessions_man:init(),
  0 = get_tablesize(),
  erwa_sessions_man:register_session(<<"test_realm">>),
  ok = ensure_tablesize(2, 500),
  erwa_sessions_man:unregister_session(),
  ok = ensure_tablesize(0, 5000),
  ets:delete(?SESSIONS_ETS).


%% @private
get_tablesize() ->
  ets:info(?SESSIONS_ETS, size).

%% @private
ensure_tablesize(_Number, MaxTime) when MaxTime =< 0 ->
  timeout;
ensure_tablesize(Number, MaxTime) ->
  case get_tablesize() of
    Number -> ok;
    _ ->
      timer:sleep(10),
      NewTime = MaxTime - 10,
      ensure_tablesize(Number, NewTime)
  end.