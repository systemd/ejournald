PRIVDIR := ../priv
CC ?= gcc
RM ?= rm
CFLAGS ?= -O2

SYSTEMD_VERSION = $(shell systemctl --version | grep systemd | cut -d " " -f 2)
SYSTEMD_TEST = $(shell [ $(SYSTEMD_VERSION) -lt 209 ] && echo true)
SYSTEMD_FLAGS = -lsystemd-id128

# libsystemd-journal got merged into libsystemd with version 209
ifeq ($(SYSTEMD_TEST), true)
	SYSTEMD_FLAGS += -lsystemd-journal
else
	SYSTEMD_FLAGS += -lsystemd
endif

override CFLAGS += -std=gnu99  -Wall -fpic -I. $(shell erl -noinput -eval 'io:format("-I~s/erts-~s/include", [code:root_dir(), erlang:system_info(version)]), halt(0).')

override LDFLAGS += -shared -fpic $(SYSTEMD_FLAGS)

all : $(PRIVDIR)/journald_api.so

$(PRIVDIR)/journald_api.so : journald_api.o
	mkdir -p $(PRIVDIR)
	$(CC) $^ $(LDFLAGS) -o $@

clean:
	$(RM) -f $(PRIVDIR)/journald_api.so journald_api.o
