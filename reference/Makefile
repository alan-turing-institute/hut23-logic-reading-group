pdfs = logic.pdf jg-is-confused-about.pdf

# Check for existence of required LaTeX style file
ttn_package := $(shell kpsewhich ttn.sty)
ifndef ttn_package
$(error Please install ttn.sty from github.com//alan-turing-institute/ttn)
endif

all: $(pdfs)

clean:
	rm -f *.blg *.log *.pdf *.bbl *.aux *.out *.fdb_latexmk *.fls

%.pdf : %.tex $(ttn_package)
	latexmk -lualatex $<




