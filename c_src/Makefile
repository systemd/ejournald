PRIVDIR := ../priv

CC := gcc

RM := rm

CFLAGS ?= -O2

override CFLAGS += -std=gnu99 -static-libgcc -static-libstdc++  -Wall -fpic -I/usr/lib64/erlang/usr/include

override LDFLAGS += -shared -fpic

all : $(PRIVDIR)/journal.so

$(PRIVDIR)/journal.so : journal.o

	$(CC) $(LDFLAGS) -o $@ $^

clean:

	$(RM) -f $(PRIVDIR)/journal.so journal.o
