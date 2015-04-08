/* 
 Copyright 2015, Travelping GmbH <copyright@travelping.com>

 This is nothing more than a compile- and link-check.
*/

#include <systemd/sd-journal.h>

int main(int argc, char **argv) {
	sd_journal *journal_pointer=NULL;
	return sd_journal_open_directory(&journal_pointer, "/var/empty", 0);
}
