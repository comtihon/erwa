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
-module(erwa_test_utils).
-author("tihon").

%% API
-export([flush/0, get_ets/1]).

flush() ->
  receive
    _ ->
      flush()
  after 0 ->
    ok
  end.

-spec get_ets(atom() | pid()) -> proplists:proplist().
get_ets(Name) when is_atom(Name) ->
  get_ets(whereis(Name));
get_ets(Pid) ->
  Tables = ets:all(),
  lists:foldl(
    fun(T, Acc) ->
      case ets:info(T, owner) == Pid of
        true ->
          Name = ets:info(T, name),
          [{Name, T} | Acc];
        false -> Acc
      end
    end, [], Tables).