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
    [(list '¬ ψ)   (prop-atomics ψ)]
    [(list (or '∧ '∨ '→) ψ χ)
     (set-union (prop-atomics ψ) (prop-atomics χ))]))

;; Flatten a propositional formula to infix notation
;; optionally removing parentheses
(define (prop-display φ #:brackets? [brackets? #t])
  (subformula->string φ 'top 'right brackets?))

;; op : which operator is the ancestor of this sub-formula?
;; side : 'left or 'right -- which operandum is this?
(define (subformula->string φ op side brackets?)
  (define (left α op) (subformula->string α op 'left brackets?))
  (define (right α op) (subformula->string α op 'right brackets?))
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

;; Exercises from ∀x
;;
;; 5.D.8 ​If John Coltrane played tuba then Miles Davis played neither trumpet nor tuba. 
;; J₃ -> ¬M₁ ∧ ¬M₂
(define/contract D8 prop/c
  '(→ J₃ (∧ (¬ M₁) (¬ M₂))))
