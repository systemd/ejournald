ejournald
=========

Ejournald is a journald binding for Erlang.

Version 1.1.0 - 10 Dec 2013
---------------------------

* Added write_fd
* Added query functions

Version 1.0.1 - 05 Dec 2013
---------------------------

* Modified sendv() to expect a tuplelist [{Entry, Value},...] where Entry is a string and Value is of type atom, int, float, or iolist
* Set SYSLOG_IDENTIFIER=node() in sendv
