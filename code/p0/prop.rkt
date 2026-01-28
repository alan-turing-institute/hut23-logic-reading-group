#lang racket

;; Manipulate propositions

(provide prop-PROP?
         prop-as-string
         argument-display)

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
(define prop-NEG?  (list/c '¬
                           (recursive-contract prop-PROP? #:flat)))
(define prop-CONJ? (list/c '∨
                           (recursive-contract prop-PROP? #:flat)
                           (recursive-contract prop-PROP? #:flat)))
(define prop-DISJ? (list/c '∧
                           (recursive-contract prop-PROP? #:flat)
                           (recursive-contract prop-PROP? #:flat)))
(define prop-IMPL? (list/c '→
                           (recursive-contract prop-PROP? #:flat)
                           (recursive-contract prop-PROP? #:flat)))
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
(define (prop-as-string φ)
  (subformula->string φ))

;; op : which operator is the ancestor of this sub-formula?
;; side : 'left or 'right -- which operandum is this?
(define (subformula->string φ)
  (define (bracket-unless type? α)
    (let ([s (subformula->string α)])
      (if (type? α)
          s
          (string-append " (" s ") "))))
  (match φ
    [#t            "#t"]
    [#f            "#f"]
    [(? symbol? v)
     (symbol->string v)]
    [(list '¬ ψ)
     (string-append "¬" (subformula->string ψ))]
    [(list '∧ ψ χ)
     (string-append
      (bracket-unless (or/c prop-NEG? prop-CONJ?) ψ)
      " ∧ "
      (bracket-unless (or/c prop-NEG? prop-CONJ?) χ))]
    [(list '∨ ψ χ)
     (string-append
      (bracket-unless (or/c prop-NEG? prop-DISJ?) ψ)
      " ∧ "
      (bracket-unless (or/c prop-NEG? prop-DISJ?) χ))]
  [(list '→ ψ χ)
     (string-append
      (bracket-unless (not/c prop-IMPL?) ψ)
      " → "
      (subformula->string χ))]))

;; Pretty-print an argument
(define (argument-display φs)
  (let-values ([(premises conclusion) (split-at-right φs 1)])
    (for ([φ (in-list premises)])
      (displayln (string-append "  " (prop-as-string φ))))
    (displayln (string-append "∴ " (prop-as-string (car conclusion))))))
