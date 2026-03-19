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
;; <deduction> ::= <name>
;;               | <lambda>
;;               | (andI <name> <name>)
;;               | (andE_l <name>)
;;               | (andE_r <name>)
;;               | (orI_l <name> <prop>)
;;               | (orI_r <name> <prop>)
;;               | (orE <name> <lambda> <lambda>)
;;               | (app <name> <name>)
;;               | (raa <lambda>)
;;               | (efq <name> <prop>)
;;
;; <proof> ::= <deduction>
;;           | (let (<name> <deduction>) <proof>) 

;; Built-in deduction rules
(struct Rule   ()                  #:transparent)
(struct AndI   Rule (nml nmr)      #:transparent)
(struct AndE-l Rule (nm)           #:transparent)
(struct AndE-r Rule (nm)           #:transparent)
(struct OrI-l  Rule (nm prop)      #:transparent)
(struct OrI-r  Rule (nm prop)      #:transparent)
(struct OrE    Rule (nm laml lamr) #:transparent)
(struct App    Rule (nm1 nm2)      #:transparent)
(struct RAA    Rule (lam)          #:transparent)
(struct EFQ    Rule (nm prop)      #:transparent)

; fun (x : P) -> proof
(struct Lam (name prop proof) #:transparent)

; let x = deduct in proof
(struct Let (name deduct proof) #:transparent)

(define name? symbol?)
(define deduction? (or/c name? Lam? Rule?))
(define proof? (or/c deduction? Let?))

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
   [(deduction? pr) (eval-deduction pr env)]
    [(Let?    pr)
     (let ([prop (eval-deduction (Let-deduct pr) env)])
       (eval-proof (Let-proof pr) (push (Let-name pr) prop env)))]))   ; NB: Tail call

(define (eval-deduction d env)
  (cond
    [(name? d) (lookup d env)]
    [(Lam?  d) (eval-lambda d env)]
    [(Rule? d) (eval-rule d env)]))

(define (eval-lambda lam env)
  (match-let ([(Lam name prop proof) lam])
    (let ([ret (eval-proof proof (push name prop env))])
      (IMPL prop ret))))

;; Deductions

(define (eval-rule rule env)
  (match rule
    [(AndI nml nmr)     (andI nml nmr env)]
    [(AndE-l nm)        (andE-l nm env)]
    [(AndE-r nm)        (andE-r nm env)]
    [(OrI-l nm prop)    (orI-l nm prop env)]
    [(OrI-r nm prop)    (orI-r nm prop env)]
    [(OrE nm laml lamr) (orE nm laml lamr env)]
    [(App nm1 nm2)      (app nm1 nm2 env)]
    [(RAA lam)          (raa lam env)]
    [(EFQ nm prop)      (efq nm prop env)]))

(define (andI env nml nmr)
  (let ([pl (lookup nml env)]
        [pr (lookup nmr env)])
    (CONJ pl pr)))

(define (andE-l nm env)
  (match (lookup nm env)
    [(CONJ p _) p]
    [v          (raise-user-error 'andE-l "(~a : ~a) Not a conjunction" nm v)]))

(define (andE-r nm env)
  (match (lookup env nm)
    [(CONJ _ q) q]
    [v          (raise-user-error 'andE-r "(~a : ~a) Not a conjunction" nm v)]))

(define (orI-l nm prop env)
  (let ([p (lookup nm env)])
    (if (and (DISJ? prop)
             (equal? (DISJ-p prop) p))
        prop
        (raise-user-error 'orI-l "Invalid disjunction (~a : ~a) ~a" nm p prop))))

(define (orI-r nm prop env)
  (let ([q (lookup nm env)])
    (if (and (DISJ? prop)
             (equal? (DISJ-q prop) q))
        prop
        (raise-user-error 'orI-r "Invalid disjunction (~a : ~a) ~a" nm q prop))))

(define (orE nm laml lamr env)
  (let ([p (lookup nm env)])
    (unless (DISJ? p)
      (raise-user-error 'orE "OrE must be applied to a disjunction (~a : ~a)" nm p))
    (unless (Lam? laml)
      (raise-user-error 'orE "OrE requires a sub-proof from the left disjunct ~a" laml))
    (unless (Lam? lamr)
      (raise-user-error 'orE "OrE requires a sub-proof from the right disjunct ~a" lamr))
    (match-let ([(DISJ pl pr) p]
                [(Lam _ xprop _) laml]
                [(Lam _ yprop _) lamr])
      (unless (and (equal? pl xprop)
                   (equal? pr yprop))
        (raise-user-error 'orE "Sub-proof assumptions must match disjunctions ~a ~a ~a" p laml lamr))
      (let ([xresult (eval-lambda laml env)]
            [yresult (eval-lambda lamr env)])
        (unless (equal? xresult yresult)
          (raise-user-error 'orE "Sub-proofs must prove the same proposition ~a ~a" laml lamr))
        xresult))))

(define (app nm1 nm2 env)
  (let ([f (lookup nm1 env)]
        [v (lookup nm2 env)])
    (unless (and (IMPL? f)
                 (equal? (IMPL-p f) v))
      (raise-user-error 'app "Apply must discharge an implication whose antecedent is equal to the other proposition (~a : ~a) (~a : ~a)" nm1 f nm2 v))
    (IMPL-q f)))

;; (P -> false) -> false |- P 
(define (raa lam env)
  (unless (Lam? lam)
    (raise-user-error 'raa "RAA requires a sub-proof ~a" lam))
  (let ([res (eval-lambda lam env)])
    (unless (FALSE? (IMPL-q res))
      (raise-user-error 'raa "Sub-proof must prove FALSE ~a" res))
    (let ([notp (IMPL-p res)])
      (unless (and (IMPL? notp)
                   (IMPL-q notp))
        (raise-user-error 'raa "RAA must hypothesise a negation ~a" notp))
      (IMPL-p notp))))

;; false -> X
(define (efq nm prop env)
  (let ([p (lookup nm env)])
    (unless (FALSE? p)
      (raise-user-error 'efq "EFQ must have FALSE as antecedent (~a : ~a)" nm p))
    prop))


;; --------------------------------------------------------------------------------

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

  ;; My proof

  (define q4
    ;; Assume (A -> B) -> A and conclude A
    (Lam 'a->b->a (IMPL (IMPL 'A 'B) 'A)
         ;; For contradiction assume not A and show false
         (RAA
          (Lam 'nota (IMPL 'A (FALSE))
               ;; ~A |- ~(A->B)
               (Let 'not-a->b
                    (Lam 'a->b (IMPL 'A 'B)
                         (Let 'a (App 'a->b->a 'a->b)
                              (App 'nota 'a)))
                    ;; ~B -> ~A
                    (Let 'notb->nota
                         (Lam 'notb (IMPL 'B (FALSE)) 'nota)
                         ;; Now show ~B -> ~A |- A -> B
                         (Let 'a->b
                              (Lam 'a 'A
                                   ;; Assume for contradiction ~B and conclude B
                                   (RAA
                                    (Lam 'notb (IMPL 'B (FALSE))
                                         (Let 'nota (App 'notb->nota 'notb)
                                              (App 'nota 'a)))))
                              ;; Now we have both A->B and ~(A->B)
                              (App 'not-a->b 'a->b))))))))
  
 )
