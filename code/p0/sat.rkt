#lang racket

#|

A basic SAT solver, very much not optimised, especially in space.

The input should be in conjunctive normal form. The representation used is:

<formula> ::= (<clause> ...)
 <clause> ::= (<literal> ...)
<literal> ::= (<var> . #t) | (<var> . #f)
    <var> ::= integer?

(<var> . #f) represents the negation of (<var> . #t)

The output is a list (possibly empty) of solutions. A solution is a list of literals, such that each
variable occurs exactly once.

|#

;; Convenience functions
(define (pos var) (cons var #t))
(define (neg var) (cons var #f))
(define VAR car)
(define VAL cdr)
(define LIT cons)

;; TODO: Add unit propagation
(define (solve problem)
  ;; Remove literals from clauses if the variable occurs twice with opposite polarities.
  ;; Remove duplicate literals
  ;; Order the variables and sort each clause in variable order

  '()
  )

;; solve : vars problem -> solution
;;
;; vars : (non-empty-listof integer?)
;;        The variables in problem in the order in which we should decide them.
;;
;; problem : a problem in which the variables in each clause are ordered by `vars`

(define (solve/recurse vars problem)
  ;; Pick var from vars
  ;; Choose var positive
  ;; Reduce problem and solve
  ;; Choose var negative
  ;; Reduce problem and solve
  (if (null? problem)                   ; Empty conjunct: always true
      '()                               ; TODO: What if we have not run out of variables?
      (let* ([v (car vars)]
             [next-problem (reduce/problem v #t problem)])
        (and next-problem
             (null? next-problem) )


  

  )

;; Reduce a problem by setting var to val
;; `problem` is a non-empty list (otherwise we would have terminated already)
;; Returns a new problem (which may be empty) or #f if any clause was empty

(module+ test
  (define cl1 '((1 . #t) (2 . #f) (3 . #t)))
  (define cl2 '((1 . #f) (3 . #f)))
  (define problem (list cl1 cl2))
  (check-equal? (reduce/clause 1 #t cl1)      #t)
  (check-equal? (reduce/clause 1 #f cl1)      '((2 . #f) (3 . #t)))
  (check-equal? (reduce/problem 1 #t problem) '(((3 . #f))))
  (check-false (reduce/problem 3 #t (reduce/problem 1 #t problem)))
  (check-equal? (reduce/problem 3 #f (reduce/problem 1 #t problem)) '()))

(define (reduce/problem var val problem)
  (for/fold ([out '()])
            ([clause (in-list problem)])
    #:break (eq? out #f)                ; Early termination 
    (let ([reduced-clause (reduce/clause var val clause)])
      (cond
        [(null? reduced-clause)  #f]    ; ^ Early termination if any clause is empty
        [(eq? reduced-clause #t) out]   ; Skip this clause if it is true
        [else (cons reduced-clause out)]))))

(define (reduce/clause var val clause)
  (let ([x (car clause)])               ; TODO (Unit propagation): Might have to replace this with find
    (if (not (eq? (VAR x) var))
        clause                          ; var does not occur in clause  
        (if (eq? (VAL x) val)
            #t                          ; x has the same polarity as val: this disjunction is true
            (cdr clause)                ; x has opposite polarity to val: remove x
            ))))




;; ---------------------------------------------------------------------------------------------------

;; The following example is from Knuth, The Art of Computer Programming, Volume 4, Fasicle 6 (acutally
;; the "pre-fasicle"), eq. 6 ("the shortest interesting formula in 3CNF").

(module+ test
  (require rackunit)

  ;; Solution: (pos 4) (neg 1) (pos 2)
  (define *sat-problem*
    (list
     (list (pos 1) (pos 2) (neg 3))
     (list (pos 2) (pos 3) (neg 4))
     (list (pos 3) (pos 4) (pos 1))
     (list (pos 4) (neg 1) (pos 2))
     (list (neg 1) (neg 2) (pos 3))
     (list (neg 2) (neg 3) (pos 4))
     (list (neg 3) (neg 4) (neg 1))))
  
  ;; No solutions 
  (define *unsat-problem*
    (cons
     (list (neg 4) (pos 1) (neg 2))
     *sat-problem*))
  
  

  )
