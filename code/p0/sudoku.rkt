#lang racket

(require racket/list/grouping)

#|

Convert a sudoko problem to CNF and output in DIMACS format.

|#

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

;; The literal 81 * row + 9 * col + digit + 1 is a proposition
;; indicating that digit is in row and col. The negative of this
;; number indicates its negative. (We add 1 to avoid negative 0)

;; Convert to conjunctive normal form
(define (sudoku-cnf props)
  ;; Each of the following is a list of clauses
  (let* ([cell-uniqueness              ; Every cell has a single digit
          (for*/list ([row (in-range 1)]
                      [col (in-range 1)])
            (list
             ;; There is at least one digit in each cell
             (for/list ([digit (in-range 3)])
               (row-col-digit->prop1 row col digit))
             ;; But not more than one
             
             ))
          ])

    
    )


(define (row-col-digit->prop1 row col digit)
  (+ (* 81 row) (* 9 col) digit 1))
  

  (define (read-puzzle in)
    ;; Proposition 81 x row + 9 x col + digit
    ;; Says that digit is in row, col
    (let-values ([(_ digits)
                  (for/fold ([pos            0]
                             [known-digits '()])
                            ([ch (in-input-port-chars in)])
                    (cond
                      [(char=? ch #\. )
                       (values (+ pos 1) known-digits)]
                      [(char-numeric? ch)
                       (let ([prop (+ (char-to-digit ch)
                                      (* 9 pos))])
                         (values (+ pos 1)
                                 (cons prop known-digits)))]
                      [else
                       (values pos known-digits)]))])
      digits)))

(define (puzzle-format props)
  (let ([posv (make-vector 81 #\.)])
    (for ([d (in-list props)])
      (let* ([row (quotient d 81)]
             [col (quotient (remainder d 81) 9)]
             [num (modulo d 9)])
        (vector-set! posv (+ (* row 9) col) (digit-to-char num))))
    (let ([chars 
           (splice-every-three
            (list #\- #\- #\- #\+ #\- #\- #\- #\+ #\- #\- #\-)
            (for/list ([row (in-range 9)])
              (splice-every-three
               #\|
               (for/list ([col (in-range 9)])
                 (vector-ref posv (+ (* row 9) col))))))])
     (string-join
      (map (λ (row) (string-append (list->string row) "\n")) chars))

      )))

(define (splice-every-three x lst)
  (apply append (add-between (windows 3 3 lst) (list x))))

(define (char-to-digit ch)
  (- (char->integer ch) (char->integer #\0)))

(define (digit-to-char d)
  (integer->char (+ d (char->integer #\0))))





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

  (display
   (puzzle-format
    (call-with-input-string puzzle read-puzzle)))
  
  )
