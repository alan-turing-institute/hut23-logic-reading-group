#lang racket

;; Propositions
;; Maybe also #f, meaning "not yet inferred"?
;; Also, a symbol is a proposition
(struct PROP       ()    #:transparent)
(struct FALSE PROP ()    #:transparent)
(struct CONJ  PROP (p q) #:transparent)
(struct DISJ  PROP (p q) #:transparent)
(struct IMPL  PROP (p q) #:transparent)

;; <name>  ::= symbol?
;;
;; <lambda> ::= (λ (<name> <prop>) <proof>)
;;
;; <deduction> ::= (andI <name> <name>)
;;               | (andE_l <name>)
;;               | (andE_r <name>)
;;               | (orI_l <name> <prop>)
;;               | (orI_r <name> <prop>)
;;               | (orE <name> <lambda> <lambda>)
;;               | (app <name> <name>)
;;               | (raa <lambda>)
;;               | (efq <name> <prop>)
;;
;; <proof> ::= <name>
;;           | <lambda>
;;           | <deduction>
;;           | (let (<name> <deduction>) <proof>) 

;; Deductions
(struct Deduct ()                    #:transparent)
(struct AndI   Deduct (nml nmr)      #:transparent)
(struct AndE-l Deduct (nm)           #:transparent)
(struct AndE-r Deduct (nm)           #:transparent)
(struct OrI-l  Deduct (nm prop)      #:transparent)
(struct OrI-r  Deduct (nm prop)      #:transparent)
(struct OrE    Deduct (nm laml lamr) #:transparent)
(struct App    Deduct (nm1 nm2)      #:transparent)
(struct RAA    Deduct (lam)          #:transparent)
(struct EFQ    Deduct (nm prop)      #:transparent)

; fun (x : P) -> proof
(struct Lam (name prop proof) #:transparent)

; let x = deduct in proof
(struct Let (name deduct proof) #:transparent)

(define name? symbol?)
(define proof? (or/c name? Lam? Deduct? Let?))

;; --------------------------------------------------------------------------------

;; An environment is a list of (<name> . <prop>)
(define (lookup v env)
  (cdr
   (or (assoc v env)
       (raise-user-error 'lookup "Undefined name" v))))

;; Later names shadow earlier ones -- probably this should raise an error
(define (push name prop env)
  (cons (cons name prop) env))

;; --------------------------------------------------------------------------------

;; Finding the proposition proven by a proof

(define (eval-judgement pr)
  (eval-proof '() pr))

;; eval-proof : proof? environment? -> PROP?
(define (eval-proof pr env)
  (cond
    [(name?   pr) (lookup pr env)]
    [(Lam?    pr) (eval-lambda pr env)]
    [(Deduct? pr) (eval-deduction pr env)]
    [(Let?    pr) 
     (let ([prop (eval-deduction (Let-deduct pr) env)])
       (eval-proof (push (Let-name pr) prop env) (Let-proof pr)))]))   ; NB: Tail call

(define (eval-lambda lam env)
  (match-let ([(Lam name prop proof) lam])
    (let ([ret (eval-proof (push name prop env) proof)])
      (IMPL prop ret))))

;; Deductions

(define (eval-deduction env deduct)
  (match deduct
    [(AndI nml nmr) (andI env nml nmr)]
    [(AndE-l nm)    (andE-l env nm)]))

(define (andI env nml nmr)
  (let ([pl (lookup nml env)]
        [pr (lookup nmr env)])
    (CONJ pl pr)))

(define (andE-l env nm)
  (match (lookup env nm)
    [(CONJ p _) p]
    [v          (raise-user-error 'andE-l "Not a conjunction" nm v)]))

(define (andE-r env nm)
  (match (lookup env nm)
    [(CONJ _ q) q]
    [v          (raise-user-error 'andE-r "Not a conjunction" nm v)]))

(define (orI-l env nm prop)
  (let ([p (lookup nm env)])
    (if (and (DISJ? prop)
             (equal? DISJ-p p))
        prop
        (raise-user-error 'orI-l "Invalid disjunction" nm p prop))))

(define (orI-r env nm prop)
  (let ([q (lookup nm env)])
    (if (and (DISJ? prop)
             (equal? DISJ-q q))
        prop
        (raise-user-error 'orI-r "Invalid disjunction" nm q prop))))

(define (orE nm laml lamr)
  ())



(module+ test

  ;; Chapter 20, Excercise A.

  ;; Q1. Prove |- O -> O

  (define q1
    (Lam 'o 'O 'o))

  ;; Q2. Prove |- N \/ ¬N

  (define q2
    (RAA
     (Lam 'notq2 (IMPL (DISJ 'N (IMPL 'N (FALSE))) (FALSE))
          (Let 'not-n 
               (Lam 'n 'N
                    (Let 'n-or-not-n (OrI-l 'n (DISJ 'N (IMPL 'N (FALSE))))
                         (App 'notq2 'n-or-not-n)))
               (Let 'n-or-not-n (OrI-r 'not-n (DISJ 'N (IMPL 'N (FALSE))))
                    (App 'notq2 'n-or-not-n))))))
  
  ;; Q3. Prove |- (J -> [J \/ (L /\ ¬L)]) /\ ([J \/ (L /\ ¬L)] -> J)

  ;; Q3. Prove |- ((A -> B) -> A) -> A

  
  )
