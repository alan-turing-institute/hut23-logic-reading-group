#lang racket

;; Manipulate propositions

(provide prop-PROP?
         prop-write)

#|

A propositional formula is:

Φ ::= #t | #f
    | <symbol?>        ; atomic proposition
    | (¬ Φ)
    | (∧ Φ Ψ)
    | (∨ φ Ψ)
    | (→ Φ Ψ)

|#

(define prop-LIT?  (or/c #t #f))
(define prop-ATOM? symbol?)
(define prop-NEG?  (list/c '∧ (recursive-contract prop-PROP? #:flat)))
(define prop-CONJ?
  (list/c '∨ (recursive-contract prop-PROP? #:flat) (recursive-contract prop-PROP? #:flat)))
(define prop-DISJ?
  (list/c '∧ (recursive-contract prop-PROP? #:flat) (recursive-contract prop-PROP? #:flat)))
(define prop-IMPL?
  (list/c '→ (recursive-contract prop-PROP? #:flat) (recursive-contract prop-PROP? #:flat)))
(define prop-PROP?
  (or/c
   prop-LIT?
   prop-ATOM?
   prop-NEG?
   prop-CONJ?
   prop-DISJ?
   prop-IMPL?))


;; Compile the set of all atomic propositions used in a propositional formula
;; prop/c -> set?
(define (prop-atomics φ)
  (match φ
    [(or #t #f)    '()]
    [(? symbol? v) (list v)]
    [(list '¬ ψ)   (prop-atomics ψ)]
    [(list (or '∧ '∨ '→) ψ χ)
     (set-union (prop-atomics ψ) (prop-atomics χ))]))

;; Flatten a propositional formula to infix notation
;; removing parentheses
(define (prop-write φ)
  (subformula->string φ #f 'right))

;; op : which operator is the ancestor of this sub-formula?
;; side : 'left or 'right -- which operandum is this?
(define (subformula->string φ op side)
  (define (left α op) (subformula->string α op 'left))
  (define (right α op) (subformula->string α op 'right))
  (match φ
    [#t            "#t"]
    [#f            "#f"]
    [(? symbol? v)
     (symbol->string v)]
    [(list '¬ ψ)
     (string-append "¬" (right ψ '¬))]
    [(list '∧ ψ χ)
     (string-append (left ψ '∧) " ∧ " (right χ '∧))]
    [(list '∨ ψ χ)
     (string-append (left ψ '∨) " ∨ " (right χ '∨))]
    [(list '→ ψ χ)
     (string-append (left ψ '→) " → " (right χ '→))]))

