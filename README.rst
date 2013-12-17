ejournald
=========

Ejournald is a journal binding for Erlang. At the moment it provides advanced write and read support for systemd's journal.

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

Reading from the journal: The following command sequence describes a typical workflow. 

    > {ok, Journal} = journald_api:open().                            // opens ALL local available journals
    > {ok, JournalDir} = journald_api:open_directory(<Path>).    	// open all journals in specific directory

    > journald_api:next(Journal).                        			//After the open command the head needs to be moved
    > journald_api:get_data(Journal, "MESSAGE").        
    {ok,"MESSAGE= first message"}                     				// first entry of my journal

    > journald_api:seek_tail(Journal).                     			// always get the last entry
    > journald_api:previous(Journal).                          		// after setting properties you always need to move the cursor (next, previous)
    > journald_api:get_data(Journal, "MESSAGE").
    {ok,"MESSAGE= another message"}

    > journald_api:add_match(Journal, "PRIORITY=7").   			// just take entries with PRIORITY=7
    > journald_api:previous(Journal).                      			// since the head is at the end of the journal he needs to search backwards
    > journald_api:get_data(Journal, "PRIORITY").
    {ok,"PRIORITY=7"}

    > journald_api:add_disjunction(Journal).               			// the next add_match should be added as a disjunction (conjunction also available 
    														// but not working with systemd 199)
    > journald_api:add_match(Journal, "PRIORITY=5").   			// now all entries with PRIORITY=7 or PRIORITY=5 are considered

    > journald_api:flush_matches(Journal).                 			// get rid of the added matches
    > journald_api:next(Journal).
    > journald_api:get_data(Journal, "PRIORITY").
    {ok,"PRIORITY=3"}

    > journald_api:seek_head(Journal).                    
    > journald_api:next().                            				// go to last available entry

    > {ok, Cursor} = journald_api:get_cursor(Journal).            	// store ID of the current entry in Cursor
    > journald_api:test_cursor(Journal, Cursor).    				// test if entries have same ID
    > journald_api:seek_cursor(Journal, Cursor).
    > journald_api:next(Journal).                                	// move to the Cursor

    > journald_api:enumerate_data(Journal).						// iterate through current entry fields
    > journald_api:restart_data(Journal).						// restart enumeration from the start index

    > journald_api:open_notifier(Journal, self()).				// you can open a notifier, that sends the atom 'journal_changed' to a given PID
    > journald_api:sendv([{"MESSAGE", test}]).					// if something changed in the journal, e.g. a new entry arrived
    > flush().
    Shell got journal_changed

    > journald_api:close(Journal).                    				// close the journal and the notifier (if used)


The upper example consists of all currently implemented methods. 

There can be just one notifier per Journal instance. The notifier itself is no part of the C-API but uses the sd_journal_wait() function.

Moving the head against borders (e.g. start and end of the journal or an entry) won't result in a crash, the atom 'eaddrnotavail' will be returned instead. 
But be careful with the following:
	- journald_api:seek_head/1 moves the cursor in front of the first entry and one has to call journald_api:next/1 to get the first entry.
	- journald_api:seek_tail/1 moves the cursor behind the last entry and one has to call journald_api:previous/1 to get the last entry.

Mixing this up will return random entries from the journal. See:
	- https://bugs.freedesktop.org/show_bug.cgi?id=64614
	- https://bugzilla.redhat.com/show_bug.cgi?id=979487
