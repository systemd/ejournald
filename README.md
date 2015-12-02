ejournald
=========

Ejournald is an Erlang interface for systemd's [journald](http://www.freedesktop.org/software/systemd/man/systemd-journald.service.html). It provides advanced write and read support.

Installation
------------

Ejournald consists of two parts: the first part is a NIF for the communication with journald, the second is an Erlang application using it.

First, you have to compile the sources in c_src/ using make. You will need the systemd development header for that. Next you can compile the erlang source in src/. 

Installation using tetrapak
--------------------------
Execute "tetrapak build".

Installation using rebar
--------------------------
Execute "rebar compile".  

Usage
-----
Ejournald is intended to provide logging support for [journald](http://www.freedesktop.org/software/systemd/man/systemd-journald.service.html). Together with [lager](https://github.com/basho/lager) and the [lager_journald_backend](https://github.com/travelping/lager_journald_backend) it allows to write structured logs with additional metainformation into systemd's journal. Logging directly with ejournald is also possible (via the NIF-API) but it is recommended to use lager. On top of that ejournald provides:

- an **[Erlang I/O-server](http://www.erlang.org/doc/apps/stdlib/io_protocol.html)** for stream-like logging (without lager)
- a high-level API for retrieving logs (possibly by Erlang specific meta information)

The I/O-server is is not capable of reading the journal. It can be used as an IO device together with the [erlang io](http://erlang.org/doc/man/io.html) library. Therefore commands like *io:format()* or *io:write()* can be used in a very convenient way to write stuff into the journal without using lager. 

To test this setup via simply sending error messages from an erlang shell, do the following:
Start ejournald:
```
application:ensure_all_started(ejournald).
{ok,[ejournald]}
```
Set the handler for the lager backend and start lager:
```
application:set_env(lager, handlers, [{lager_journald_backend, []}]).
lager:start().
```
Send an error message (which the backend sends to the journal)
```
lager:error("Some error happened!").
```
Then simply check your journal for the corresponding message (e.g. 'journalctl -f'):
```
# journalctl
...
Jan 16 13:16:18 Host beam.smp[7481]: Some error happened!
```

It is possible to augment the logs with additional meta information (keys are always atoms)
```
lager:info([{current_weather, "Sunny"}, {temperature, 23}], "Hello World!", []).
```
Via 'journalctl -f -o verbose' you should see all log fields including the special ones set by you (not all fields are shown):
```
...
PRIORITY=6
_TRANSPORT=journal
ERL_APPLICATION=my_test_app
ERL_MODULE=my_test_app_mod
ERL_FUNCTION=start
ERL_LINE=20
ERL_PID="<0.36.0>"
ERL_NODE=nonode@nohost
MESSAGE=Hello World!
CURRENT_WEATHER="Sunny"
TEMPERATURE=23
...
```
Erlang specific fields which are set automatically by the backend are prepended with "ERL_".

Note that lager uses parse transform, e.g. you have to add
```
-compile([{parse_transform, lager_transform}]).
```
to your erlang module to compile it. If you want to test your stuff without doing that just use something like
```
lager:log(info, [{meta1, data1}, {meta2, data2}], "Hello World!").
```

See edocs for more information. 
