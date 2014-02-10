ejournald
=========

Ejournald is a journald binding for Erlang.

Version 1.3.0 - 7 Feb 2014
---------------------------

* added functionalities to get realtime and monotonic timestamps with boot IDs which can be used to seek entries
* the notifier is now able to distinguish between notifications about added journal entries and added/deleted journal files

Version 1.2.0 - 18 Dec 2013
---------------------------

* added functionalities to get every field of a journal entry
* improved notifier handling
* accept Erlang pid as value in sendv
* removed setting of SYSLOG_IDENTIFIER

Version 1.1.0 - 10 Dec 2013
---------------------------

* Added write_fd
* Added query functions

Version 1.0.1 - 05 Dec 2013
---------------------------

* Modified sendv() to expect a tuplelist [{Entry, Value},...] where Entry is a string and Value is of type atom, int, float, or iolist
* Set SYSLOG_IDENTIFIER=node() in sendv
