#lang racket

;; Manipulate propositions

#|

A propositional formula is:

Φ ::= #t | #f
    | <symbol?>        ; atomic proposition
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

;; Compile the set of all atomic propositions used in a propositional formula
;; prop/c -> set?
(define (prop-atomics φ)
  (match φ
    [(or #t #f)    '()]
    [(? symbol? v) (list v)]
    [(list '¬ Φ)   (prop-atomics Φ)]
    [(list (or '∧ '∨ '→) Φ Ψ)
                   (set-union (prop-atomics Φ) (prop-atomics Ψ))]))


;; Flatten a propositional formula to infix notation
;; optionally removing parentheses
(define (prop-display φ #:brackets? [brackets? #t])
  

  )
;; op : which operator is the ancestor of this sub-formula?
;; side : 'left or 'right -- which operandum is this?
(define (subformula->string φ op side brackets?)
  (match φ
    [#t            "#t"]
    [#f            "#f"]
    [(? symbol? v) (symbol->string v)]
    [(list '¬ Φ)   (string-join "¬" (subformula->string ψ "" 'right brackets?))]
    [(list (or '∧ '∨ '→) Φ Ψ)
                   (set-union (prop-atomics Φ) (prop-atomics Ψ))]
    )
  )

;; Exercises from ∀x
;;

