#lang racket

;; Manipulate propositions

#|

A propositional formula is:

Φ ::= #t | #f
| <symbol?>
| (¬ Φ)
| (∧ Φ Ψ)
| (∨ φ Ψ)
| (→ Φ Ψ)

|#

(define prop/c
  (flat-rec-contract
   Φ
   (or/c #t #f
         symbol?
         (list/c '¬ Φ)
         (list/c (or/c '∧ '∨ '→) Φ Φ))))

;; Compile the set of all identifiers used in a propositional formula
;; prop/c -> set?
(define (prop-identifiers φ)
  (match φ
    [(or #t #f)    '()]
    [(? symbol? v) (list v)]
    [(list '¬ Φ)   (prop-identifiers Φ)]
    [(list (or '∧ '∨ '→) Φ Ψ)
                   (set-union (prop-identifiers Φ) (prop-identifiers Ψ))]))

