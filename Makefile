TARGETS = main test

default: $(TARGETS)

SOURCES = interp.c types.c read.c gc.c vm.c hashtab.c
HEADERS = interp.h types.h read.h gc.h vm.h hashtab.h

OBJECTS = $(subst .c,.o,$(SOURCES))

CC = gcc
ifeq ($(FAST),1)
	CFLAGS = -O3 -W -Wall -ansi
else
ifeq ($(PROF),1)
	CFLAGS = -g -pg -W -Wall -ansi
else
	CFLAGS = -g -W -Wall -ansi
endif
endif

main: $(OBJECTS) main.o $(HEADERS)
	$(CC) -o $@ $(OBJECTS) main.o

test: $(OBJECTS) test.o $(HEADERS)
	$(CC) -o $@ $(OBJECTS) test.o

check-syntax:
	$(CC) -o $(CHK_SOURCES).nul -S $(CHK_SOURCES)

TAGS:
	find . -name "*.[chCH]" -print | etags -

clean:
	rm -f *.o $(TARGETS) *.nul

INDENT_FLAGS = -npro -npsl -npcs -nsaf -nsai -nsaw -br -brf -brs -ncs
indent:
	indent $(INDENT_FLAGS) $(SOURCES)
