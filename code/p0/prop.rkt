#lang racket

;; Manipulate propositions

(provide (struct-out PROP)
         ;; (struct-out LIT)
         ;; (struct-out ATOM)
         ;; (struct-out NEG)
         ;; (struct-out CONJ)
         ;; (struct-out DISJ)
         ;; (struct-out IMPL)
         proposition
         prop-as-string
         argument-display)

#|

A propositional formula is:

Φ ::= #t | #f
    | <symbol?>        ; atomic proposition
    | (not Φ)
    | (and Φ Ψ)
    | (or φ Ψ)
    | (impl Φ Ψ)

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
    [(list '-> ψ χ) (IMPL (proposition ψ) (proposition χ))]
    [_ (raise-argument-error 'proposition "A proposition, as a list." P)]))

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

;; Evaluate propositions

(define (prop-eval φ env)
  (match φ
    [(LIT v)    v]
    [(ATOM s)   (dict-ref env s)]
    [(NEG ψ)    (not (prop-eval ψ env))]
    [(CONJ ψ χ) (and (prop-eval ψ env) (prop-eval χ env))]
    [(DISJ ψ χ) (or  (prop-eval ψ env) (prop-eval χ env))]
    [(IMPL ψ χ) (or (not (prop-eval ψ env)) (prop-eval χ env))]))

;; Utilities

;; Compile the set of all sentence letters used in a propositional formula
;; prop/c -> set?
(define (prop-letters φ)
  (sort
   (match φ
     [(LIT _)    '()]
     [(ATOM v)   (list v)]
     [(NEG ψ)   (prop-letters ψ)]
     [(or (CONJ ψ χ) (DISJ ψ χ) (IMPL ψ χ))
      (set-union (prop-letters ψ) (prop-letters χ))])
   symbol<?))


(define (make-col-widths vars)
  (map (λ (var)
         (+ 1
            (max 2 (string-length (symbol->string var)))))))

(define (format-minimum-width v wd)
  (let ([padding (make-string (- wd (string-length v)) #\space)])
    (string-append padding v)))

;; -> string
(define (tt-header vars col-widths φ-string φ-width)
  (string-append 
   (string-join (map format-minimum-width (map symbol->string vars) col-widths))
   " | "
   (format-minimum-width φ-string φ-width)))

(define (truth-table-display φ)
  (let* ([vars       (prop-letters φ)]
         [φ-string   (prop-as-string φ)]
         [col-widths (make-col-widths (cdr vars))]
         [φ-width (+ 1 (max 2 (string-length φ-string)))])
    
    ;; [rows 
    ;;  (apply cartesian-product
    ;;         (map (λ (_) '(#f #t)) vars))]
    ;; (for ([vals (in-list rows)])
    ;;   (let ([env (map cons vars vals)])
    ;;     (let ([val (prop-eval φ env)])
    ;;       (displayln (format "~a : ~a" vals val)))))))
    ))


;; --------------------------------------------------------------------------------

(module+ week3

  ;; Exercise 11.A.2
  (define exA2
    (proposition '(-> C (not C))))

  (prop-as-string exA2)


  )
