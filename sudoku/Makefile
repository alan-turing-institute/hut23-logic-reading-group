all: sudoku.pdf

main.aux: main.tex main.bib
	pdflatex main.tex

main.bbl: main.aux
	bibtex main.aux

main.pdf: main.tex style.tex
	pdflatex main.tex
	pdflatex main.tex

sudoku.pdf: main.pdf
	cp main.pdf sudoku.pdf

clean:
	rm -f *.blg *.log *.pdf *.bbl *.aux *.out *.nav *.snm *.toc *.vrb
