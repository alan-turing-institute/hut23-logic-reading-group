# vim: noet:ts=4:sts=4:sw=4

target = daeducer

objects = src/main.o src/utils.o src/step.o src/proof.o src/lemma.o src/ruleset.o src/command.o src/budget.o src/config.o src/fileutils.o src/model.o src/sampler.o src/vector.o

LLAMA_INCLUDE = -I llama.cpp/include -I llama.cpp/ggml/include/
LLAMA_LIBS = -L llama.cpp/build/bin/ -lggml -lllama -lggml-base -lggml-cpu

all: $(target)

$(target): $(objects)
	gcc -Wall -g $^ ../../sudoku/symbolic/libsymbolic.a $(LLAMA_LIBS) -o $(target)

$(objects): %.o: %.c
	gcc -Wall -g $(LLAMA_INCLUDE) -c -I../../sudoku/symbolic/src $^ -o $@

clean:
	rm -f $(objects) $(target)
