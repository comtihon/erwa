%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2015 12:02
%%%-------------------------------------------------------------------
-author("tihon").

-record(session,
{
	id = none,
	is_auth = false,
	realm_name = none,
	mwl = [],
	version = erwa:get_version(),
	client_roles = unknown,

	routing_pid = none,
	broker = none,
	dealer = none,

	source = unknown,
	peer = unknown,
	ssl = false,

	goodbye_sent = false,

	calls = [],

	invocation_id = 1,
	invocations = []
}).