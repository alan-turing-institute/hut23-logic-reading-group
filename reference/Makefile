pdfs = logic.pdf jg-is-confused-about.pdf

all: $(pdfs)

$(pdfs): %.pdf: %.tex
	latexmk -lualatex $<

clean:
	rm -f *.blg *.log *.pdf *.bbl *.aux *.out *.fdb_latexmk *.fls

