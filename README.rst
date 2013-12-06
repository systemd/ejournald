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
Please deliver the parameters as a tuple {"VARIABLE", value}. You can use atom, int, float, or iolist (string, binary) as value. 

Example for sendv(): 

    journald_api:sendv([{"MESSAGE", test}, {"PRIORITY", 1}]). 

You can use these input parameters: http://0pointer.de/public/systemd-man/systemd.journal-fields.html

journald_api:stream_fd(identifier, priority, level_prefix) stream to journal by calling sd_journal_stream_fd(3). 
stream_fd/3 returns a stream file descriptor you can use in write_fd to write into the journal. The journal entries SYSLOG_IDENTIFIER = identifier and PRIORITY=priority will be set for all messages send through this file descriptor. level_prefix is a boolean. If true kernel-style log priority prefixes can be interpreted (not implemented yet).

journald_api:write_fd(File_descriptor, Message) writes Message into journald using a file descriptor from stream_fd.
Message must be a string. Messages are sent after "\n" is written.

Example for stream_fd/3 and write_fd/2: 

    Fd = journald_api:stream_fd("id",5,0).
    journald_api:write_fd(Fd, "notice\n").

will produve the following message in the journal:
        
    PRIORITY=5
    SYSLOG_IDENTIFIER=id
    MESSAGE=notice

journald_api:close_fd(File_descriptor) can be used to close File_descriptor manually.
