#lang racket

#|

Convert a sudoko problem to CNF and output in DIMACS format.

|#

(require "prop.rkt")

;; Template for puzzles. Anything by "." or a digit is ignored.
; ...|...|...
; ...|...|...
; ...|...|...
; ---|---|---
; ...|...|...
; ...|...|...
; ...|...|...
; ---|---|---
; ...|...|...
; ...|...|...
; ...|...|...

(define (read-puzzle in)
  ;; Proposition 81 x row + 8 x col + digit
  ;; Says that digit is in row, col
  (for/fold ([nm 0])
            ([known-digits '()]
      ([c (in-input-port-chars in)]))
      
             [row   (in-lines in)])
    (for/
    )









(module+ test


  ;; From The Guardian, 5 February 2026
  (define puzzle #<<EOS
...|...|...
...|.53|.24
...|24.|57.
---|---|---
..3|...|.9.
.79|..5|.8.
.1.|.7.|..6
---|---|---
..7|...|3..
.41|38.|..7
.6.|..4|.1.
EOS
    )

  )
