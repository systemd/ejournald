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

See edocs for more information. 
