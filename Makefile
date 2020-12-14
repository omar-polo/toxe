EMACS =		emacs
CC =		cc
CFLAGS =	`pkg-config --cflags toxcore` -Wall -g
LDFLAGS =	`pkg-config --libs   toxcore`

.PHONY: all clean compile test

all: toxe compile TAGS

toxe: toxe.o utf8.o
	${CC} ${LDFLAGS} toxe.o utf8.o -o toxe

TAGS: *.c
	-etags *.c || true

clean:
	rm -f toxe *.o *.elc TAGS

compile: toxe.elc toxe-test.elc

toxe-test.elc: toxe.elc

test: toxe-test.elc
	${EMACS} -Q --batch -L . -l toxe-test.elc -f ert-run-tests-batch

.SUFFIXES: .el .elc
.el.elc:
	${EMACS} -Q --batch -L . -f batch-byte-compile $<
