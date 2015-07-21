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
-module(erwa_sessions_man).
-author("tihon").

-include("erwa_service.hrl").

%% API
-export([
  register_session/1,
  send_message_to/2,
  preregister_session/1,
  update_registration/3,
  delete_preregistration/1,
  unregister_session/0,
  init/0]).

init() ->
  ets:new(?SESSIONS_ETS, [public, named_table, {read_concurrency, true}, {write_concurrency, true}]).

-spec send_message_to(Msg :: term(), SessionId :: non_neg_integer()) -> ok | {error, unknown}.
send_message_to(Msg, SessionId) ->
  case ets:lookup(?SESSIONS_ETS, SessionId) of
    [{SessionId, unknown, _Realm}] ->
      {error, unknown};
    [{SessionId, Pid, _Realm}] ->
      Pid ! {erwa, Msg},
      ok;
    [] ->
      {error, unknown}
  end.

-spec register_session(binary()) -> {ok, binary()}.
register_session(Realm) ->
  ID = crypto:rand_uniform(0, 9007199254740992),
  case ets:insert_new(?SESSIONS_ETS, {ID, self(), Realm}) of
    true ->
      ets:insert(?SESSIONS_ETS, {self(), ID}),
      {ok, ID};
    false ->
      register_session(Realm)
  end.

-spec preregister_session(Id :: non_neg_integer()) -> true | false.
preregister_session(Id) ->
  ets:insert_new(?SESSIONS_ETS, {Id, unknown, unknown}).

-spec update_registration(Id :: non_neg_integer(), Pid :: pid(), Realm :: binary()) -> ok.
update_registration(Id, Pid, Realm) ->
  ets:insert(?SESSIONS_ETS, {Id, Pid, Realm}),
  ok.

-spec delete_preregistration(Id :: non_neg_integer()) -> ok.
delete_preregistration(Id) ->
  delete_session(Id),
  ok.

%% Should be called in case of session unregistring.
%% Best way is to call it in Process:terminate
-spec unregister_session() -> ok.
unregister_session() ->
  ID = get_id_from_pid(self()),
  delete_session(ID).


%% @private
delete_session(not_found) -> not_found;
delete_session(ID) ->
  case ets:lookup(?SESSIONS_ETS, ID) of
    [{ID, Pid, _Realm}] ->
      true = ets:delete(?SESSIONS_ETS, ID),
      true = ets:delete(?SESSIONS_ETS, Pid),
      ok;
    _ ->
      not_found
  end.

%% @private
get_id_from_pid(Pid) ->
  case ets:lookup(?SESSIONS_ETS, Pid) of
    [{Pid, ID}] ->
      ID;
    _ ->
      not_found
  end.