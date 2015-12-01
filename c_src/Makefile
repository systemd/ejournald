PRIVDIR := ../priv
CC ?= gcc
RM ?= rm
CFLAGS ?= -O2

SYSTEMD_CFLAGS=$(shell pkg-config --cflags libsystemd || pkg-config --cflags libsystemd-journal)
override CFLAGS += -std=gnu99  -Wall -fpic -I. $(shell erl -noinput -eval 'io:format("-I~s/erts-~s/include", [code:root_dir(), erlang:system_info(version)]), halt(0).') $(SYSTEMD_CFLAGS)

override LDFLAGS += -fpic
LDLIBS = $(shell pkg-config --libs libsystemd || pkg-config --libs libsystemd-journal)

all : $(PRIVDIR)/journald_api.so

# check if systemd symbols can be resolved
journald_api.o: check-journald

$(PRIVDIR)/journald_api.so : journald_api.o
	mkdir -p $(PRIVDIR)
	$(CC) $^ -shared $(LDFLAGS) $(LDLIBS) -o $@

clean:
	$(RM) -f $(PRIVDIR)/journald_api.so journald_api.o check-journald
