#lang racket/base

(require
 racket/set
 racket/string
 (only-in racket/list splitf-at)
 (only-in racket/port port->lines)
 (only-in threading ~> tee~>))

#|

A basic DP SAT solver, very much not optimised, especially in space.

The input should be in conjunctive normal form. The representation used is:

<formula> ::= (<clause> ...)
<clause> ::= (<literal> ...)
<literal> ::= (<var> . #t) | (<var> . #f)
<var> ::= integer?

(<var> . #f) represents the negation of (<var> . #t)

The output is a list (possibly empty) of solutions. A solution is a list of literals, such that each
variable occurs exactly once.

|#

(provide
 solve                                  ; solve a problem
 read-dimacs                            ; read a file in DIMACS format
 )

;; ---------------------------------------------------------------------------------------------------

;; Convenience functions
(define (pos var) (cons var #t))
(define (neg var) (cons var #f))
(define VAR car)
(define VAL cdr)
(define LIT cons)

;; TODO: Add unit propagation
(define (solve problem)
  (let* ([vars     (problem-unique-vars problem)]
         [problem< (problem-sort-clauses problem)])
    (solve/recurse vars problem<))
  ;; TODO:
  ;; Remove literals from clauses if a variable occurs twice with opposite polarities.
  ;; Remove duplicate literals
  
  )

(define (problem-unique-vars problem)
  (sort (set->list
         (apply set-union
                (map (λ (cl) (list->set (map VAR cl)))
                     problem)))                              
        <                           ; TODO: Revisit < as an order
        ))

(define (problem-sort-clauses problem)
  (map (λ (clause) (sort clause < #:key VAR)) problem))


;; solve : vars problem -> solution
;;
;; vars : (non-empty-listof integer?)
;;        The variables in `problem`, in the order in which we should decide them.
;;
;; problem : a problem in which the variables in each clause are ordered by `vars`
;; Returns a list of solutions, or the empty list if there are none.
(define (solve/recurse vars problem)
  (if (null? problem)                   ; Empty conjunct: always true
      (list '())                        ; TODO: What if we have not run out of variables?
      (let* ([v (car vars)]
             [vs (cdr vars)])           
        (filter values                  ; Delete any dead ends ...
                (append
                 (try v #t vs problem)
                 (try v #f vs problem))))))

;; Simplify the problem clauses and then solve the simpler problem
(define (try var val vars problem)
  (let ([next-problem (reduce/problem var val problem)])
    (if (eq? next-problem #f)
        '()
        (map (λ (soln) (cons (LIT var val) soln))
             (solve/recurse vars next-problem)))))


;; reduce/problem : var val problem -> problem?
;; Reduce a problem by setting var to val
;; `problem` is a non-empty list (otherwise we would have terminated already)
;; Returns a new problem (which may be empty) 

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
;; Input and output in DIMACS format

;; TODO: Replace this with a nicer parser.
;; TODO: We do not check that the number of vars is num-vars, and the number of clauses is num-clauses

;; read-dimacs : port? -> problem?
(define (read-dimacs in)
  (let-values ([(num-vars num-clauses) (read-dimacs-problem-size in)])
    (~> (port->lines in)
        (map (λ (ln) (map string->number (string-split ln))) _) 
        (apply append _)
        (tee~> displayln)
        (group-on-zeros)                ; now grouped by clauses
        (map (λ (clause) (map (λ (v) (if (negative? v) (neg (- v)) (pos v)))
                              clause))
             _))))

;; xs : a list of numbers, through of as groups where each group is separated by a 0
(define (group-on-zeros xs)
  (let loop ([acc '()]
             [xs  xs])
  (if (null? xs)
      acc
      (let-values ([(next rest) (splitf-at xs (λ (x) (not (zero? x))))])
        (cond
          [(null? next) (raise-user-error "Error in DIMACS file: perhaps two consecutive zeros?" 'read-dimacs)]
          [(null? rest) (cons next acc)]
          [else         (loop (cons next acc) (cdr rest))]  ; rest starts with 0
          )))))

;; Skip comment lines until we find a "p"
(define (read-dimacs-problem-size in)
  (let loop ()
    (let ([ln (read-line in)])
      (cond
        [(eof-object? ln)
         (raise-user-error "End of file before problem line found." 'read-dimancs)]
        [(and (non-empty-string? ln)
              (char-ci=? (string-ref ln 0) #\p))
         (parse-dimacs-problem-line ln)]
        [else (loop)]))))

;; A DIMACS problem line is:
;; p cnf N-vars N-clauses
(define (parse-dimacs-problem-line ln)
  (let ([toks (string-split ln)])
    (unless (string-ci=? (cadr toks) "cnf")
      (raise-user-error "Only cnf files are supported." 'read-dimancs))
    (values
     (string->number (caddr toks))
     (string->number (cadddr toks)))))


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
     *sat-problem*)))
