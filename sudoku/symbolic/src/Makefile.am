lib_LIBRARIES = ../libsymbolic.a
include_HEADERS = symbolic.h

___libsymbolic_a_SOURCES = symbolic_private.h symbolic.h approximate.c prop.c simplify.c convert.c variables.c extract.c latex.c relation.c relationlist.c varstack.c
___libsymbolic_a_CFLAGS = -std=c99

noinst_PROGRAMS = ../symbolic
___symbolic_SOURCES = interface.c
___symbolic_CPPFLAGS = -std=c99
___symbolic_LDADD = ../libsymbolic.a -lm

