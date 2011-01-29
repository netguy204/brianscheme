TARGETS = bsch bschsfx

default: $(TARGETS)

SOURCES = interp.c types.c read.c gc.c vm.c hashtab.c ffi.c pool.c
HEADERS = $(subst .c,.h,$(SOURCES))

OBJECTS = $(subst .c,.o,$(SOURCES))

IMAGE = boot.img

LDFLAGS = -leditline -lffi -ldl -lm -rdynamic

CC = gcc

ifeq ($(NO_READLINE),1)
	EXTFLAGS=-DNO_READLINE
endif

ifeq ($(FAST),1)
	CFLAGS = -O3 -W -Wall $(EXTFLAGS)
else
ifeq ($(PROF),1)
	CFLAGS = -g -pg -W -Wall $(EXTFLAGS)
	LDFLAGS += -g -pg
else
	CFLAGS = -g -W -Wall $(EXTFLAGS)
endif
endif

bsch: $(OBJECTS) bsch.o $(HEADERS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) bsch.o

bschsfx: $(OBJECTS) bschsfx.o $(HEADERS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) bschsfx.o
	perl padsfx.pl $@

bschsfx.o: bsch.c
	$(CC) -DSFX $(CFLAGS) -c -o $@ $^

image: bsch
	./bsch save-image.sch $(IMAGE)

test: bsch
	echo '(load "run-tests.sch")' | ./bsch

check-syntax:
	$(CC) -o $(CHK_SOURCES).nul -S $(CHK_SOURCES)

TAGS:
	find . -name "*.[chCH]" -print | etags -

clean:
	rm -f *.o $(TARGETS) *.nul $(IMAGE)

INDENT_FLAGS = -npro -npsl -npcs -nsaf -nsai -nsaw -br -brf -brs -ncs
indent:
	indent $(INDENT_FLAGS) $(SOURCES)

linecount:
	wc -l *.[ch] *.sch clos/*.sch examples/*.sch tests/*.sch
