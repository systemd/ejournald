ejournald
=========

Ejournald is an Erlang interface for systemd's [journald](http://www.freedesktop.org/software/systemd/man/systemd-journald.service.html). It provides advanced write and read support.

Installation
------------

Ejournald consists of two parts: the first part is a nif for the communication with journald, the second is an Erlang application.

First, you have to compile the sources in c_src/ using make. Next you can compile the erlang source in src/. 

Installation using tetrapak
--------------------------
You can build the project by executing "tetrapak build". 

Usage
-----

Ejournald is intended to provide logging support for [journald](http://www.freedesktop.org/software/systemd/man/systemd-journald.service.html). Together with [lager](https://github.com/basho/lager) and the [lager_journald_backend](https://github.com/travelping/lager_journald_backend) it allows to write structured logs with additional metainformation into systemd's journal. Logging directly with ejournald is also possible (via the NIF-API) but it is recommended to use lager. On top of that ejournald provides:

- an **[Erlang I/O-server](http://www.erlang.org/doc/apps/stdlib/io_protocol.html)** for stream-like logging (without lager)
- a high-level API for retrieving logs 

The I/O-server is is not capable of reading the journal. It can be used as an IO device together with the [erlang io](http://erlang.org/doc/man/io.html) library. Therefore commands like *io:format()* or *io:write()* can be used in a very convenient way to write stuff into the journal without using lager. By default an I/O-server named *ejournald_io_server* is started together with ejournald. The log level (by default *info*) and other options are fixed for one I/O-server. Thus if you need other options (e.g. another log level) you need to start your own one. Note that the 'name' option (a string) is mandatory and you have to deliver a unique name for every server. This name will appear as a prefix in the journal.
The high-level API for reading logs consists of the two function get_logs/1 and log_notify/1. The first one will enable you to retrieve logs based on time-frames. The latter one is intended to deliver new logs as they appear in the journal. It is therefore possible to **build simple monitoring systems** using this API. Logs are always delivered in the form

```erlang
 {Timestamp, LogLevel, LogData}
```

where *Timestamp* is of type [calendar:datetime1970()](http://www.erlang.org/doc/man/calendar.html#type-datetime1970), *LogLevel* is one of the eight journald log levels and *LogData* is a string or a list of strings. For example 

```erlang
    LastLogs = ejournald:get_logs([{at_most, 10}, {message, true}]),
    ejournald:log_notify(self(), [{message, true}]),
    flush(). % some new logs might appear here 
```

is roughly equivalent to *'journalctl -f'* giving you the last 10 logs in message-only format (just one string per log) and following the journal if new logs appear. Leaving the 'message'-option would give you whole logs (a list of strings per log). The 'Options' parameter is always intended to filter the choice of logs. Just *ejournald:get_logs([ ])* would reproduce the whole journal. To restrict the time-frame for *get_logs()* you can use the options 'since' and 'until'. The order of logs is always destined by the 'direction'-option (by default 'top' - from newest to oldest). Another example:

```erlang
    Logs = ejournald:get_logs([{direction, bot}, {since, {{2013,12,31},{12,0,0}} }]).
```

This gives you full logs in the order 'oldest to newest' since lunchtime of last silvester. Note that you must use UTC-time. If possible filtering should be done by ejournald since the used C-API in the background is much faster at handling this. You can use as many different log_notify()'s as you want at the same time. Different filters will be handled properly. A process handling new logs has the following layout:

```erlang
	loop() ->
		receive 
            {'EXIT', FromPid, Reason} ->
                % end your process;
                % ejournald automatically links a worker process to your Pid.
                % Thus if you end your process it will also end this worker.
			{Timestamp, LogLevel, LogData} ->
				% do something here
				loop();
			journal_changed ->
				% maybe a new journal file was appended, refresh your monitors!
				loop()
		end.
```

You can also provide a function for working on the logs. Filtering by (Erlang-) applications and other meta-data is planned for the future.
