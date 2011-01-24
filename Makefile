TARGETS = bsch

default: $(TARGETS)

SOURCES = interp.c types.c read.c gc.c vm.c hashtab.c ffi.c symbols.c
HEADERS = $(subst .c,.h,$(SOURCES))

OBJECTS = $(subst .c,.o,$(SOURCES))

LDFLAGS = -leditline -lffi -ldl -lm -rdynamic

CC = gcc

ifeq ($(NO_READLINE),1)
	EXTFLAGS=-DNO_READLINE
endif

ifeq ($(FAST),1)
	CFLAGS = -O3 -W -Wall -ansi $(EXTFLAGS)
else
ifeq ($(PROF),1)
	CFLAGS = -g -pg -W -Wall -ansi $(EXTFLAGS)
	LDFLAGS += -g -pg
else
	CFLAGS = -g -W -Wall -ansi $(EXTFLAGS)
endif
endif

bsch: $(OBJECTS) bsch.o $(HEADERS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) bsch.o

test: bsch
	echo '(load "run-tests.sch")' | ./bsch

check-syntax:
	$(CC) -o $(CHK_SOURCES).nul -S $(CHK_SOURCES)

TAGS:
	find . -name "*.[chCH]" -print | etags -

clean:
	rm -f *.o $(TARGETS) *.nul

INDENT_FLAGS = -npro -npsl -npcs -nsaf -nsai -nsaw -br -brf -brs -ncs
indent:
	indent $(INDENT_FLAGS) $(SOURCES)
