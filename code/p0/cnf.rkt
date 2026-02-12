#lang racket

(require (prefix-in p: "prop.rkt"))

(struct ATOM (s) #:transparent)
(struct NOT (p) #:transparent)
(struct CONJ (lt rt) #:transparent)
(struct DISJ (lt rt) #:transparent)



#|

A cnf is:


   <cnf> ::= (list-of <clause>)    ; Represents the conjunction of the <clause>
<clause> ::= (list-of <lit>)       ; The disjunction of the <lit>
   <lit> ::= <atom>
       |  (NOT <atom>)

|#


(define (prop-to-cnf prop)
  ;; Reduce to and, or, and not
  )
