ejournald
=========

Ejournald is an Erlang interface for systemd's [journald](http://www.freedesktop.org/software/systemd/man/systemd-journald.service.html). It provides advanced write and read support.

Installation
------------

Ejournald consists of two parts: the first part is a NIF for the communication with journald, the second is an Erlang application using it.

First, you have to compile the sources in c_src/ using make. You will need the systemd development header for that. Next you can compile the erlang source in src/. 

Installation using tetrapak
--------------------------
Execute "tetrapak build". If you use tetrapak, you don't need to compile the sources in c_src/, tetrapak will do this for you.

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
> application:ensure_all_started(ejournald).
>
> {ok,[ejournald]}

Set the handler for the lager backend:
> application:set_env(lager, handlers, [{lager_journald_backend, []}]).

Start lager:
> lager:start().

Send an error message (which the backend sends to the journal)
> lager:log(error, [{pid, self()}, {module, module}, {function, function}, {line, 0}], "hello world").

Then simply check your journal for the corresponding message:
> \# journalctl
>
> \.\.\.
>
> Jan 16 13:16:18 tpiadmin-HP-EliteBook-8470p beam.smp[7481]: hello world

See edocs for more information. 