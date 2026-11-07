#lang racket

(provide (struct-out Term)
         (struct-out Var)
         (struct-out App)
         (struct-out Abs)
         parse
         unparse
         eval/step
         eval)

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

(struct Term ()             #:transparent)
(struct Var Term (sym un)   #:transparent) ; un is a number to disambiguate variables
(struct App Term (op arg)   #:transparent)
(struct Abs Term (var body) #:transparent)

(define (var->symbol var)
  (let ([x (Var-sym var)]
        [n (Var-un  var)])
    (if (zero? n)
        x
        (string->symbol
         (string-append
          (symbol->string x) "_" (number->string n))))))


;; parse : s-expression -> Term?
;; All parsed symbols start out with 0 as their uniquefying number
(define (parse sexp)
  (match sexp
    [(? symbol? v) #:when (and (not (eq? v 'lambda)) (not (eq? v 'λ)))
                   (Var v 0)]
    [(list op arg)
     (App (parse op) (parse arg))]
    [(list (or 'lambda 'λ) (list (? symbol? v)) body)
     (Abs (Var v 0) (parse body))]
    [_
     (raise-user-error "Invalid form" sexp)]))

;; unparse : Term? -> s-expression
(define (unparse term)
  (match term
    [(Var _ _)    (var->symbol term)]
    [(App op arg) (list (unparse op) (unparse arg))]
    [(Abs v body) (list 'λ (list (var->symbol v)) (unparse body))]))

;; fresh! : Var? -> Var?
;; Return a variable that is guaranteed to be new.
(define fresh!
  (let ([vars (make-hash)])
    (λ (var)
      (let* ([v (Var-sym var)]
             [N (add1 (hash-ref vars v 0))])
        (hash-set! vars v N)
        (Var v N)))))

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
    [(Var _ _)    (list term)]
    [(App op arg) (set-union (free-vars op) (free-vars arg))]
    [(Abs v body) (set-remove (free-vars body) v)]))

;; Capture-avoiding substitution E[F/x]
;; Replace x with F in E, when x is free, and without
;; capturing free variables in F that are bound in E
;; x[F/x]      = F
;; v[F/x]      = v (where x ≠ v)
;; (λx.E)[F/x] = λx.E
;; (λv.E)[F/x] = (λw. E[w/v][F/x]) when x ≠ v and where w ∉ FV[(λv.E)] and w ∉ FV[F]
;; (E F)[G/x]  = (E[G/x] F[G/x])

;; substitute : Var? Term? Term?
;; E[F/x] (or "let x = F in E")
;; This version renames all bound variables without bothering to check whether there is risk of
;; capture. An obvious optimisation would be to do that once and for all before starting to reduce.
(define (substitute x F E)
  (match E
    [(Var _ _)
     (if (equal? x E) F E)]
    [(App op arg)
     (App (substitute x F op) (substitute x F arg))]
    [(Abs v body)
     (if (equal? x v)
         (Abs x body)
         (let ([vv (fresh! v)])
           (Abs vv
                (substitute x F (substitute vv v body)))))]))

;; eval/step : Term? -> (or/c Term? #f)
;; Single-step reduction
;; Search the parse tree left-first, until we find a redex; then reduce
;; Return #f if no redex is found
(define (eval/step t)
  (match t
    [(Var _ _)              #f]
    [(App (Abs x body) arg) (substitute x arg body)]
    [(App op arg)           (let ([op1 (eval/step op)])
                              (if op1
                                  (App op1 arg)
                                  (let ([arg1 (eval/step arg)])
                                    (if arg1
                                        (App op arg1)
                                        #f))))]
    [(Abs x body)           (let ([body1 (eval/step body)])
                              (and body1 (Abs x body1)))]
    ))

;; eval : Term? -> Term?
;; Big-step reduction.
;; Uses eval/step, so is O(n^2)
(define (eval t)
  (let ([t1 (eval/step t)])
    (if t1
        (eval t1)
        t)))

(define eg1 (parse '(((λ (x) (λ (y) x)) s) t)))
