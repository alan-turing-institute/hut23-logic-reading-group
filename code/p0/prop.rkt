#lang racket

;; Manipulate propositions

(provide (struct-out PROP)
         (struct-out LIT)
         (struct-out ATOM)
         (struct-out NEG)
         (struct-out CONJ)
         (struct-out DISJ)
         (struct-out IMPL)
         proposition
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

(struct PROP      ()    #:transparent)
(struct LIT  PROP (val) #:transparent)
(struct ATOM PROP (sym) #:transparent)
(struct NEG  PROP (p)   #:transparent)
(struct CONJ PROP (p q) #:transparent)
(struct DISJ PROP (p q) #:transparent)
(struct IMPL PROP (p q) #:transparent)

;; Input and output
(define (proposition P)
  (match P
    [(or #t #f)       (LIT P)]
    [(? symbol? v)    (ATOM v)]
    [(list 'not ψ)    (NEG (proposition ψ))]
    [(list 'and ψ χ)  (CONJ (proposition ψ) (proposition χ))]
    [(list 'or ψ χ)   (DISJ (proposition ψ) (proposition χ))]
    [(list 'impl ψ χ) (IMPL (proposition ψ) (proposition χ))]
    [_ (raise-argument-error 'proposition "A proposition, as a string." P)]))

;; Flatten a propositional formula to infix notation
;; removing parentheses
(define (prop-as-string φ)
  (subformula->string φ))

(define (subformula->string φ)
  (define (bracket-unless type? α)
    (let ([s (subformula->string α)])
      (if (type? α)
          s
          (string-append " (" s ") "))))
  (match φ
    [(LIT #t)         "#t"]
    [(LIT #f)         "#f"]
    [(ATOM sym)       (symbol->string sym)]
    [(NEG ψ)          (string-append "¬" (subformula->string ψ))]
    [(CONJ ψ χ)
     (string-append
      (bracket-unless (or/c NEG? CONJ?) ψ)
      " ∧ "
      (bracket-unless (or/c NEG? CONJ?) χ))]
    [(DISJ ψ χ)
     (string-append
      (bracket-unless (or/c NEG? DISJ?) ψ)
      " ∧ "
      (bracket-unless (or/c NEG? DISJ?) χ))]
    [(IMPL ψ χ)
     (string-append
      (bracket-unless (not/c IMPL?) ψ)
      " → "
      (subformula->string χ))]))

;; Pretty-print an argument
;; φs : (pair? (listof PROP?) PROP?)
(define (argument-display φs)
  (let ([premises (car φs)]
        [conclusion (cdr φs)])
    (for ([φ (in-list premises)])
      (displayln (string-append "  " (prop-as-string φ))))
    (displayln (string-append "∴ " (prop-as-string conclusion)))))


;; Utilities

;; Compile the set of all atomic propositions used in a propositional formula
;; prop/c -> set?
(define (prop-atomics φ)
  (match φ
    [(or #t #f)    '()]
    [(? symbol? v) (list v)]
    [(list '¬ ψ)   (prop-atomics ψ)]
    [(list (or '∧ '∨ '→) ψ χ)
     (set-union (prop-atomics ψ) (prop-atomics χ))]))

