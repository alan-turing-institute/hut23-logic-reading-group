#lang racket

;; Propositions
(struct PROP       ()    #:transparent)
(struct FALSE PROP ()    #:transparent)
(struct CONJ  PROP (p q) #:transparent)
(struct DISJ  PROP (p q) #:transparent)
(struct IMPL  PROP (p q) #:transparent)


;; <name>  ::= symbol?

;;          | (lambda <name> <prop> <proof>)

;; <deduction> ::= (AndI <> <>)
;;                 (AndE_l <>)
 
;;

;; Primitive rules
(struct AndI   (tl tr)   #:transparent)
(struct AndE-l (t)       #:transparent)
(struct AndE-r (t)       #:transparent)
(struct OrI-l  (t)       #:transparent)
(struct OrI-r  (t)       #:transparent)
(struct OrE    (t tl tr) #:transparent)
(struct App    (t1 t2)   #:transparent)

(struct Lam () #:transparent)
(struct Let () #:transparent)

;; A proof is either:
;; - a symbol (representing a name)
;; - a deduction
;; 
