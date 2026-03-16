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
;; <proof> ::= <name>
;;           | <lambda>
;;           | <deduction>
;;           | (let (<name> <deduction>) <proof>) 

;; Deductions
(struct AndI   (nl nr)   #:transparent)
(struct AndE-l (n)       #:transparent)
(struct AndE-r (n)       #:transparent)
(struct OrI-l  (n prop)  #:transparent)
(struct OrI-r  (n prop)  #:transparent)
(struct OrE    (n ll lr) #:transparent)
(struct App    (n1 n2)   #:transparent)
(struct RAA    (l)       #:transparent)
(struct EFQ    (n prop)  #:transparent)

; fun (x : P) -> proof
(struct Lam (name prop proof) #:transparent)

; let x = deduct in proof
(struct Let (name deduct proof) #:transparent)



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
