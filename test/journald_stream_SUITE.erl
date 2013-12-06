-module(journald_stream_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([write_string/1, write_binary/1, write_iolist/1]).

-define(value(Key, Config), proplists:get_value(Key,Config)).

all() -> [write_string, write_binary, write_iolist].

init_per_suite(Config) ->
    Fd = journald_api:stream_fd(atom_to_list(node()), 5, 0),
    [{fd, Fd} | Config].

end_per_suite(Config) ->
    Fd = ?value(fd, Config),
    ok = journald_api:close_fd(Fd).

write_string(Config) ->
    Fd = ?value(fd, Config),
    ok = journald_api:write_fd(Fd, "string\n").

write_binary(Config) ->
    Fd = ?value(fd, Config),
    ok = journald_api:write_fd(Fd, <<"binary\n">>).

write_iolist(Config) ->
    Fd = ?value(fd, Config),
    ok = journald_api:write_fd(Fd, ["string ", <<"binary">>, "\n"]).
