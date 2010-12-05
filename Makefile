TARGETS = main test

default: $(TARGETS)

SOURCES = interp.c types.c read.c gc.c vm.c
HEADERS = interp.h types.h read.h gc.h vm.h

OBJECTS = $(subst .c,.o,$(SOURCES))

ifeq ($(FAST),1)
	CC = gcc -O3 -W -Wall -ansi
else
	CC = gcc -g -W -Wall -ansi
endif

main: $(OBJECTS) main.o $(HEADERS)
	$(CC) -o $@ $(OBJECTS) main.o

test: $(OBJECTS) test.o $(HEADERS)
	$(CC) -o $@ $(OBJECTS) test.o

check-syntax:
	$(CC) -o $(CHK_SOURCES).nul -S $(CHK_SOURCES)

clean:
	rm -f *.o $(TARGETS) *.nul
