#lang racket

;; Manipulate propositions

(provide prop/c
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

(define prop/c
  (flat-rec-contract
   Φ
   (or/c #t #f
         symbol?
         (list/c '¬ Φ)
         (list/c (or/c '∧ '∨ '→) Φ Φ))))

(define prop-LIT? (or/c #t #f))
(define prop-NEG? (list/c ))


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
     (maybe-bracket-unless (or (eq? ))  
      (string-append (left ψ '∧) " ∧ " (right χ '∧)))]
    [(list '∨ ψ χ)
     (maybe-bracket-unless op 
      (string-append (left ψ '∨) " ∨ " (right χ '∨)))]
    [(list '→ ψ χ)
     (string-append (left ψ '→) " → " (right χ '→))]))

