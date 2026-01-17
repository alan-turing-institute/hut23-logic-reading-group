pdfs = logic.pdf

all: $(pdfs)

$(pdfs): %.pdf: %.tex
	latexmk -lualatex -output-format=pdf $<

clean:
	rm -f *.blg *.log *.pdf *.bbl *.aux *.out *.fdb_latexmk *.fls

