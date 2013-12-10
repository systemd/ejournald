-module(journald_api_write_SUITE).

-export([all/0]).

-export([sendv_string/1, sendv_atom/1, sendv_int/1, sendv_float/1, sendv_binary/1, 
         sendv_diverse/1, sendv_badarg1/1, sendv_badarg2/1, sendv_badarg3/1, sendv_badarg4/1,
	 sendv_parrallel/1]).

-export([send_test/1]).

-define(Msg, "MESSAGE").

all() -> [sendv_string, sendv_atom, sendv_int, sendv_float, sendv_binary,
          sendv_diverse, sendv_badarg1, sendv_badarg2, sendv_badarg3, sendv_badarg4, sendv_parrallel].

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
        {error, "bad argument"} = journald_api:sendv(test). 

sendv_parrallel(_) ->			   
    rpc:pmap({?MODULE, send_test},[],lists:seq(1,100)),
    {node(), nodes()}.

send_test(V1) -> [journald_api:sendv([{"MESSAGE=", "iolist" ++ integer_to_list(V2) ++ "/" ++ integer_to_list(V1)}]) || V2 <- lists:seq(1,10)].

%pmap(F, L) ->
%    pmap(F, L, 3000).

%pmap(F, L, Timeout) ->
%    Parent = self(),
%    Pids = [spawn(fun() -> Parent ! {self(), F(X)} end) || X <- L],
%    lists:map(
%        fun(Pid) ->
%            receive {Pid, Result} -> 
%	            Result 
%            after Timeout ->
%                      {error, timeout}
%            end
%        end, Pids).
