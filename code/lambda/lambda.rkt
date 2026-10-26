#lang racket

#|

A lambda-calculus interpreter

Grammar
-------

This interpreter accepts lambda-terms written as s-expressions.

- Variables: Any valid Scheme symbol apart from `lambda` and `λ`.
- Application: A list of two elements, e.g., `(f g)`.
- Abstraction: A list of three elements, whose car is `lambda` or `λ`, and whose second element is a list of one element. For example,
  `(λ (x) f)`.

We may generalise later for convenience:

- (λ (x y ...) f) to mean (λ (x) (λ (y) (... f))) 
- (f x y ... ) to mean (((f x) y) ...) 

|#

(struct Term ()               #:transparent)
(struct Var Term (v)          #:transparent)
(struct App Term (rator rand) #:transparent)
(struct Abs Term (v body)     #:transparent)

;; parse : s-expression -> Term?
(define (parse sexp)
  (match sexp
    [(? symbol? v)
     #:when (and (not (eq? v 'lambda)) (not (eq? v 'λ)))
     (Var v)]
    [(list rator rand)
     (App (parse rator) (parse rand))]
    [(list (or 'lambda 'λ) (list (? symbol? v)) body)
     (Abs v (parse body))]
    [_
     (raise-user-error "Invalid form" sexp)]))

;; redex? : Term? -> bool?
;; A redex is (App (Abs ...) ... )
(define (redex? term)
  (match term
    [(App (Abs _ _) _) #t]
    [_                 #f]))


;; free-vars : Term? -> [List-of symbol?]
;; FV[v]         = (list v)
;; FV[(E F)]     = FV[E] U FV[F]
;; FV[(λ (v) E)] = FV[E]\{v}
(define (free-vars term)
  (match term
    [(Var v)          (list v)]
    [(App rator rand) (set-union (free-vars rator) (free-vars rand))]
    [(Abs v body)     (set-remove (free-vars body) v)]))


;; fresh : symbol? -> symbol?
;; Return a symbol that is guaranteed to be new.
;; Appends "_NN" to the end of its input, where NN increments
;; by 1 every time fresh is called with the same input.
(define fresh
  (let ([vars (make-hash)])
    (λ (var)
      (let ([N (add1 (hash-ref vars var 0))])
        (hash-set! vars var N)
        (string->symbol (string-append (symbol->string var) "_" (number->string N)))))))

;; Capture-avoiding substitution E[F/x]
;; Replace x with F in E, when x is free, and without
;; capturing free variables in F that are bound in E
;; x[F/x]      = F
;; v[F/x]      = v (where x ≠ v)
;; (λx.E)[F/x] = λx.E
;; (λv.E)[F/x] = (λw. E[w/v][F/x]) when x ≠ v and where w ∉ FV[(λv.E)] and w ∉ FV[F]
;; (E F)[G/x]  = (E[G/x] F[G/x])

;; substitute : symbol? Term? Term?
;; E[F/x] (or "let x = F in E")
;; This version renames all bound variables without bothering to check whether there is risk of
;; capture. An obvious optimisation would be to do that once and for all before starting to reduce.
(define (substitute x F E)
  (match E
    [(Var v)
     (if (eq? x v) F (Var v))]
    [(App rator rand)
     (App (substitute x F rator) (substitute x F rand))]
    [(Abs v body)
     (if (eq? x v)
         (Abs x body)
         (let ([vv (fresh v)])
           (Abs vv
                (substitute x F (substitute vv v body)))))]))

;; eval/step : Term? -> Tern?
;; Single-step beta-reduction.
;; Search the parse tree left-first, until we find a redex; then reduce

