TARGETS = bsch bsch.preboot bschsfx

default: $(TARGETS)

SOURCES = interp.c types.c read.c gc.c vm.c hashtab.c pool.c socket.c tlsf.c fancystack.c

HEADERS = $(subst .c,.h,$(SOURCES))

OBJECTS = $(subst .c,.o,$(SOURCES))

IMAGE = boot.img

LDFLAGS = -lz -lltdl -lm -rdynamic -L/usr/lib

CC = gcc

ifeq ($(FAST),1)
	CFLAGS = -O3 -W -Wall $(EXTFLAGS)
else
ifeq ($(PROF),1)
	CFLAGS = -g -pg -W -Wall $(EXTFLAGS)
	LDFLAGS += -g -pg
else
ifeq ($(COV),1)
	CFLAGS = -g -fprofile-arcs -ftest-coverage -W -Wall $(EXTFLAGS)
	LDFLAGS += -g -fprofile-arcs -ftest-coverage
else
	CFLAGS = -g -W -Wall $(EXTFLAGS)
endif
endif
endif


bsch: bschsfx $(IMAGE)
	cat $^ > $@
	chmod +x $@

bsch.preboot: $(OBJECTS) bsch.o $(HEADERS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) bsch.o

bschsfx: $(OBJECTS) bschsfx.o $(HEADERS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) bschsfx.o
	perl padsfx.pl $@

bschsfx.o: bsch.c
	$(CC) -DSFX $(CFLAGS) -c -o $@ $^

$(IMAGE): bsch.preboot
	./bsch.preboot save-image.sch $(IMAGE)

image: $(IMAGE)

swank: bsch
	./bsch swank-image.sch

test: bsch
	./bsch run-tests.sch

TAGS: bsch.c $(SOURCES) $(HEADERS)
	find . -name "*.[chCH]" -print | etags -

run: bsch
	./bsch

clean:
	$(RM) *.o $(TARGETS) $(IMAGE) bs swank

bs-clean:
	$(RM) bsch bs swank $(IMAGE)

bs: bsch bs-lib.sch
	./bsch bs-build.sch

INDENT_FLAGS = -npro -npsl -npcs -nsaf -nsai -nsaw -br -brf -brs -ncs
indent:
	indent $(INDENT_FLAGS) $(SOURCES)

linecount:
	wc -l *.[ch] *.sch clos/*.sch examples/*.sch tests/*.sch

.PHONY: test image clean indent linecount run bs-clean
