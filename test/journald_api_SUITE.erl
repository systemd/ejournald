-module(journald_api_SUITE).

-define(Msg, "MESSAGE").

-export([all/0]).

-export([sendv_string/1, sendv_atom/1, sendv_int/1, sendv_float/1, sendv_binary/1, 
         sendv_diverse/1, sendv_badarg1/1, sendv_badarg2/1, sendv_badarg3/1, sendv_badarg4/1]).

all() -> [sendv_string, sendv_atom, sendv_int, sendv_float, sendv_binary,
          sendv_diverse, sendv_badarg1, sendv_badarg2, sendv_badarg3, sendv_badarg4].

sendv_string(_) ->
	ok = journald_api:sendv([{?Msg, "string"}]).

sendv_atom(_) ->
	ok = journald_api:sendv([{?Msg, atom}]).

sendv_int(_) ->
	ok = journald_api:sendv([{?Msg, 42}]). 

sendv_float(_) ->
	ok = journald_api:sendv([{?Msg, 3.14}]).

sendv_binary(_) ->
	ok = journald_api:sendv([{?Msg, <<"notice">>}]).

sendv_diverse(_) ->
	ok = journald_api:sendv([{?Msg,notice},
				 {"PRIORITY", 5},
				 {"CODE_FILE", ?FILE},
				 {"CODE_LINE", ?LINE}]).

sendv_badarg1(_) -> 
	ok = journald_api:sendv([{"Message", notice}]).		%This case is ignored by sd_journal_sendv

sendv_badarg2(_) ->
	ok = journald_api:sendv([{?Msg, [notice]}]).

sendv_badarg3(_) ->
	ok = journald_api:sendv([test]).

sendv_badarg4(_) ->
	error = journald_api:sendv(test). 
