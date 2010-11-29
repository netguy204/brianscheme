TARGETS = main test

default: $(TARGETS)

SOURCES = interp.c types.c

OBJECTS = $(subst .c,.o,$(SOURCES))

CC = gcc -g -Wall -ansi

main: ${OBJECTS} main.o
	$(CC) -o $@ $(OBJECTS) main.o

test: ${OBJECTS} test.o
	$(CC) -o $@ $(OBJECTS) test.o

check-syntax:
	$(CC) -o $(CHK_SOURCES).nul -S $(CHK_SOURCES)

clean:
	rm -f *.o $(TARGETS)
