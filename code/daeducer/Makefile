# vim: noet:ts=4:sts=4:sw=4

target = daeducer

objects = src/main.o src/utils.o src/step.o src/proof.o src/lemma.o src/ruleset.o src/command.o

all: $(target)

$(target): $(objects)
	gcc -Wall -g $^ ../../sudoku/symbolic/libsymbolic.a -o $(target)

$(objects): %.o: %.c
	gcc -Wall -g -c -I../../sudoku/symbolic/src $^ -o $@

clean:
	rm -f $(objects) $(target)
