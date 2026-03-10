#lang racket

;; Manipulate propositions

(provide (struct-out PROP)
         (struct-out VALUE)
         (struct-out ATOM)
         (struct-out NEG)
         (struct-out CONJ)
         (struct-out DISJ)
         (struct-out IMPL)
         (struct-out EQV)
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
    | (eqv Φ Ψ)

|#

(struct PROP       ()    #:transparent)
(struct VALUE PROP (val) #:transparent)
(struct ATOM  PROP (sym) #:transparent)
(struct NEG   PROP (p)   #:transparent)
(struct CONJ  PROP (p q) #:transparent)
(struct DISJ  PROP (p q) #:transparent)
(struct IMPL  PROP (p q) #:transparent)
(struct EQV   PROP (p q) #:transparent)

;; Input and output
(define (proposition P)
  (match P
    [(or #t #f)      (VALUE P)]
    [(? symbol? v)   (ATOM v)]
    [(list 'not ψ)   (NEG (proposition ψ))]
    [(list 'and ψ χ) (CONJ (proposition ψ) (proposition χ))]
    [(list 'or ψ χ)  (DISJ (proposition ψ) (proposition χ))]
    [(list '-> ψ χ)  (IMPL (proposition ψ) (proposition χ))]
    [(list '<-> ψ χ) (EQV (proposition ψ) (proposition χ))]
    [_ (raise-argument-error 'proposition "A proposition, as a list." P)]))


;; ---------------------------------------------------------------------------------------------------
;; Pretty printing

;; Flatten a propositional formula to infix notation
;; removing parentheses
(define (prop-as-string φ)
  (subformula->string φ))

(define (subformula->string φ)
  (define (with-brackets α)
    (string-append "(" (subformula->string α) ")"))
  (define (with-brackets-unless type? α)
    (if (type? α)
        (subformula->string α)
        (with-brackets α)))
  (match φ
    [(VALUE #t)         "#t"]
    [(VALUE #f)         "#f"]
    [(ATOM sym)       (symbol->string sym)]
    [(NEG ψ)          (string-append "¬" (subformula->string ψ))]
    [(CONJ ψ χ)
     (string-append
      (with-brackets-unless (or/c ATOM? NEG? CONJ?) ψ)
      " ∧ "
      (with-brackets-unless (or/c ATOM? NEG? CONJ?) χ))]
    [(DISJ ψ χ)
     (string-append
      (with-brackets-unless (or/c ATOM? NEG? DISJ?) ψ)
      " ∨ "
      (with-brackets-unless (or/c ATOM? NEG? DISJ?) χ))]
    [(IMPL ψ χ)
     (string-append
      (with-brackets-unless (or/c ATOM? (not/c IMPL?)) ψ)
      " → "
      (subformula->string χ))]
    [(EQV ψ χ)
     (string-append
      (with-brackets-unless ATOM? ψ) " ↔ " (with-brackets-unless ATOM? χ))]))

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
    [(VALUE v)  v]
    [(ATOM s)   (dict-ref env s)]
    [(NEG ψ)    (not (prop-eval ψ env))]
    [(CONJ ψ χ) (and (prop-eval ψ env) (prop-eval χ env))]
    [(DISJ ψ χ) (or  (prop-eval ψ env) (prop-eval χ env))]
    [(IMPL ψ χ) (or (not (prop-eval ψ env)) (prop-eval χ env))]
    [(EQV ψ χ)  (let ([ψ1 (prop-eval ψ env)]
                      [χ1 (prop-eval χ env)])
                  (or (and ψ1 χ1)
                      (and (not ψ1) (not χ1))))]))

;; Utilities

;; Compile the set of all sentence letters used in a propositional formula
;; prop/c -> set?
(define (prop-letters φ)
  (sort
   (match φ
     [(VALUE _) '()]
     [(ATOM v)  (list v)]
     [(NEG ψ)   (prop-letters ψ)]
     [(or (CONJ ψ χ) (DISJ ψ χ) (IMPL ψ χ) (EQV ψ χ))
      (set-union (prop-letters ψ) (prop-letters χ))])
   symbol<?))

;; Show a truth table
(define (tt φ)
 (let* ([vars (prop-letters φ)]
        [rows  (apply cartesian-product
                      (map (λ (_) '(#f #t)) vars))])
   (for/list ([vals (in-list rows)])
     (let* ([env (map cons vars vals)]
            [val (prop-eval φ env)])
       (cons vals val))))) 



(define (make-col-widths vars)
  (map (λ (var)
         (+ 1
            (max 2 (string-length (symbol->string var)))))
       vars))

(define (format-all/minimum-widths vs wds)
  (string-join
   (map format/minimum-width vs wds)))

(define (format/minimum-width v wd)
  (let ([padding (make-string (- wd (string-length v)) #\space)])
    (string-append padding v)))

;; -> string
(define (tt-header vars col-widths φ-string φ-width)
  (cons
   (format-all/minimum-widths (map symbol->string vars) col-widths)
   (format/minimum-width φ-string φ-width)))

(define (tt-display φ)
  (let* ([vars       (prop-letters φ)]
         [φ-string   (prop-as-string φ)]
         [col-widths (make-col-widths vars)]
         [φ-width (+ 1 (max 2 (string-length φ-string)))])
    (let* ([header (tt-header vars col-widths φ-string φ-width)]
           [var-header (car header)]
           [val-header (cdr header)]
           [var-header-len (string-length var-header)]
           [val-header-len (string-length val-header)])
          (displayln (string-append var-header " |" val-header ))
      (displayln
       (string-append
        (make-string var-header-len #\-)
        "-|"
        (make-string val-header-len #\-))))
    (for ([row (in-list (tt φ))])
      (displayln
       (string-append
        (format-all/minimum-widths
         (map (λ (v) (format "~a" v)) (car row))
         col-widths)
        " |"
        (format/minimum-width
         (format "~a" (cdr row))
         φ-width))))))


;; --------------------------------------------------------------------------------

(module+ week3

  ;; Exercise 11.A.2
  (define exA2
    (proposition '(-> C (not C))))

  (displayln "11.A.2")
  (tt-display exA2)

  (displayln "\n\n11.A.5")
  (tt-display (proposition '(-> (and A B) (or B A))))

  (displayln "\n\n11.B.1")
  (tt-display (proposition '(and (and A B) C)))
  (displayln "and")
  (tt-display (proposition '(and A (and B C))))
  
  (displayln "\n\n11.C.3")
  (tt-display
   (proposition
    '(<-> (-> A B)
          (<-> (not B) (not A)))))



  )
