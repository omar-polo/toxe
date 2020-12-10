CC =		cc
CFLAGS =	`pkg-config --cflags toxcore` -Wall -g
LDFLAGS =	`pkg-config --libs   toxcore`

.PHONY: all clean

all: toxe-core.so TAGS

toxe-core.so: toxe-core.o utf8.o
	${LD} -shared ${LDFLAGS} -o $@ toxe-core.o utf8.o

TAGS: *.c
	-etags *.c || true

clean:
	rm -f toxe-core.so *.o TAGS
