# Addition is commutative in Peano Arithmetic using FOL

Here you'll find a proof that addition is commutative in Peano Arithmetic, using only the proof rules of First Order Logic as found in [forall x: Calgary](https://forallx.openlogicproject.org/).
The LaTeX document gives the proof in Fitch notation with some explanatory notes.
The other files are the proof steps that can be loaded into the [daeducer](../code/daeducer/) proof checker.

To build the PDF file from the LaTeX source `peano_add_comm.tex`, run the following:
```sh
$ pdflatex peano_add_comm.tex
```

This will generate the file `peano_add_comm.pdf` for viewing.
The other files in this directory are proofs that can be loaded into daeducer.

1. `peano_add_comm.tex`: The LaTeX source with the full proof and explanatory text.
2. `peano_add_comm.pdf`: A pre-built PDF with the full proof and explanatory text.
3. `peano_axioms.txt`: The peano axioms for use with daeducer, as used in all of the other proofs.
4. `peano_lemma1.txt`: The proof of Lemma 1 in daeducer format.
5. `peano_lemma2.txt`: The proof of Lemma 2 in daeducer format.
6. `peano_add_comm.txt`: The proof that addition is commutative in daeducer format.
7. `README.md`: This file.

