ejournald
=========

Ejournald is a journal binding for Erlang. It provides advanced write and read support for systemd's journal.

Installation
------------

Ejournald consists of two parts: the first part is a nif for the communication with journald, the second is an Erlang interface.

First, you have to compile the sources in c_src/ using make. Next you can compile the erlang source in src/. 

Installation using tetrapak
--------------------------
You can build the project by executing "tetrapak build". 

Lager integration
-----------------
You can use [lager_journald_backend](https://github.com/travelping/lager_journald_backend) to send your Lager logs to systemd's journal.

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

will produce the following message in the journal:
        
    PRIORITY=5
    SYSLOG_IDENTIFIER=id
    MESSAGE=notice

Reading from the journal: The following command sequence describes a typical workflow. 

    > {ok, Journal} = journald_api:open().
    > {ok, JournalDir} = journald_api:open_directory(<Path>).

    > journald_api:next(Journal).
    > journald_api:get_data(Journal, "MESSAGE").        
    {ok,"MESSAGE= first message"}               

    > journald_api:seek_tail(Journal).         
    > journald_api:previous(Journal).         
    > journald_api:get_data(Journal, "MESSAGE").
    {ok,"MESSAGE= another message"}

    > journald_api:add_match(Journal, "PRIORITY=7").
    > journald_api:previous(Journal). 
    > journald_api:get_data(Journal, "PRIORITY").
    {ok,"PRIORITY=7"}

    > journald_api:add_disjunction(Journal).	
    // the next add_match should be added as a disjunction 
    // (conjunction also available but not working with systemd 199)
    
    > journald_api:add_match(Journal, "PRIORITY=5"). 

    > journald_api:flush_matches(Journal).   
    > journald_api:next(Journal).
    > journald_api:get_data(Journal, "PRIORITY").
    {ok,"PRIORITY=3"}

    > journald_api:seek_head(Journal).                    
    > journald_api:next().                  

    > {ok, Cursor} = journald_api:get_cursor(Journal).
    > journald_api:test_cursor(Journal, Cursor).    
    > journald_api:seek_cursor(Journal, Cursor).
    > journald_api:next(Journal).                  

    > journald_api:enumerate_data(Journal).		
    > journald_api:restart_data(Journal).	

    > journald_api:open_notifier(Journal, self()).
    > journald_api:sendv([{"MESSAGE", test}]).	
    > flush().
    Shell got journal_append

    > journald_api:close_notifier(Journal).	

    > journald_api:close(Journal).         


There can be just one notifier per Journal instance. The notifier itself is no part of the C-API but uses the sd_journal_wait() function.
The notifier will close itself when the receiving Erlang process is not available.
Note that the notifier distinguishes between added journal entries and added/deleted journal files. It returns 'journal_append' or 'journal_invalidate'.

Moving the head against borders (e.g. start and end of the journal or an entry) won't result in a crash, the atom 'eaddrnotavail' will be returned instead. 
But be careful with the following:
	- journald_api:seek_head/1 moves the cursor in front of the first entry and one has to call journald_api:next/1 to get the first entry.
	- journald_api:seek_tail/1 moves the cursor behind the last entry and one has to call journald_api:previous/1 to get the last entry.

Mixing this up will return random entries from the journal. See:
	- https://bugs.freedesktop.org/show_bug.cgi?id=64614
	- https://bugzilla.redhat.com/show_bug.cgi?id=979487
