#lang racket

(require (prefix-in p: "prop.rkt"))

(struct POS (s) #:transparent)
(struct NEG (p) #:transparent)
(struct CONJ (lt rt) #:transparent)
(struct DISJ (lt rt) #:transparent)

#|

A <cnf> is:

   <cnf> ::= (list-of <clause>)    ; Represents the conjunction of the <clause>
<clause> ::= (list-of <lit>)       ; The disjunction of the <lit>
   <lit> ::= <atom>                ; In code, (POS <atom>) and (NEG <atom>)
          |  (neg <atom>)
<atom>   ::= Anything that can be compared with eq?

|#

(define (prop-to-cnf prop)
  ;; Reduce to and, or, and not
  ;; Convert to cnf
  ;; Remove duplicate atoms from clauses
#f
  )

(define (clause-to-prop clause)
  (if (null? clause)
      (p:VALUE #f)
      (let ([esualc (reverse clause)])
        (foldl (λ (l ls) (p:DISJ (lit-to-prop l) ls))
               (car esualc)
               (cdr esualc)))))

(define (lit-to-prop l)
  (match l
    [(POS s) (p:ATOM s)]
    [(NEG s)  (p:NEG (p:ATOM s))])
  )
