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

(struct Term ()        #:transparent)
(struct Var Term (v)   #:transparent)
(struct App Term (s t) #:transparent)
(struct Abs Term (v b) #:transparent)

;; parse : s-expression -> Term?
(define (parse sexp)
  (match sexp
    [(? symbol? v)
     #:when (and (not (eq? v 'lambda)) (not (eq? v 'λ)))
     (Var v)]
    [(list s t)
     (App (parse s) (parse t))]
    [(list (or 'lambda 'λ) (list (? symbol? v)) body)
     (Abs v (parse body))]
    [_
     (raise-user-error "Invalid form" sexp)]))

;; redex? : Term? -> Bool?
;; A redex is (App (Abs ...) ... )
(define (redex? term)
  (match term
    [(App (Abs _ _) _) #t]
    [else              #f]))

;; eval/step
;; search the parse tree left-first, until we find a redex; then reduce

;; ((λ (x) (+ x 1)) (f x))

;; ((λ (x) (λ (y) x)) y)
;;                |
;;                bound variables are: x and y
;; => ((λ (x) (λ (y1) x)) y)
