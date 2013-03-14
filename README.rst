ejournald
=========

Ejournald is a journal binding for Erlang. At the moment it provides rudimentary write support for systemd's journal.

Installation
------------

Ejournald consists of two parts: the first part is a nif for the communication with journald, the second is an Erlang interface.

First, you have to compile the sources in c_src/ using make. Next you can compile the erlang source in src/. 

Installation using tetrapak
--------------------------
You can build the project by executing "tetrapak build". 

Usage
-----

journald_api:sendv([Parameter1, Parameter2,...,ParameterN]) is equal to sd_journal_sendv().
Please deliver the parameters as a list. You can use io_lists as list elements for parameters with multiline content. 

Example for sendv(): 

	journald_api:sendv(["MESSAGE=TEST", "PRIORITY=1"]). 

You can use these input parameters: http://0pointer.de/public/systemd-man/systemd.journal-fields.html

journald_api:stream_fd(identifier, priority, level_prefix) stream to journal by calling sd_journal_stream_fd(3). 

Example for stream_fd/3: 

	journald_api:stream_fd("TEST",1,2).

stream_fd/3 returns a stream file descriptor you can use to write into the journal.