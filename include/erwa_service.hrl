%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2015 15:04
%%%-------------------------------------------------------------------
-author("tihon").

-define(REALMS_SERVICE, erwa_realms).
-define(REALMS_ETS, erwa_realms_tab).
-define(SESSIONS_ETS, erwa_sessions_tab).
-define(EVENTS_ETS, events).  %not named, could be multiple, one per realm
-define(RPC_ETS, rpc).  %not named, could be multiple, one per realm
-define(CONNECTIONS_ETS, connections).  %not named, could be multiple, one per realm