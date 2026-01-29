#lang racket

(require "prop.rkt")

;; Exercises from ∀x
;;
;; 5.D.8 ​If John Coltrane played tuba then Miles Davis played neither trumpet nor tuba. 
;; J₃ -> ¬M₁ ∧ ¬M₂
(define V-D8
  (proposition
   '(impl J₃ (and (not M₁) (not M₂)))))

(displayln "5.D.8")
(displayln (prop-as-string V-D8))
(displayln "")

;; 5.G.1 ​
;; If Dorothy plays the piano in the morning [D₁], then Roger wakes up
;; cranky [R]. Dorothy plays piano in the morning unless she is
;; distracted [D₂]. So if Roger does not wake up cranky, then Dorothy must
;; be distracted.
(define V-G1 
  (cons (list
         (proposition '(impl D₁ R))
         (proposition '(impl (not D₂) D₁)))
        (proposition '(impl (not R) D₂))))

(displayln "5.G.1")
(argument-display V-G1)
(displayln "")

;; 5.H.2
;; If Doctor Octopus gets the uranium [U], he will blackmail the city [C]. I
;; am certain of this because if Doctor Octopus gets the uranium, he
;; can make a dirty bomb [B], and if he can make a dirty bomb, he will
;; blackmail the city.

(define V-H2
  (cons (list
         (proposition '(impl U B))
         (proposition '(impl B C )))
        (proposition '(impl U C))))

(displayln "5.H.2")
(argument-display V-H2)
(displayln "")

;; 5.I
;; P ⊕ Q = (P ∨ Q) ∧ ¬(P ∧ Q)
;;        = (P ∨ Q) ∧ (¬P ∨ ¬Q)
;;        = ¬(¬(P ∨ Q) ∨ ¬(¬P ∨ ¬ Q))

;; 6.A.1
;; (A)
;; Not a setence.

;; 6.A.3
;; ¬¬¬¬F
;; Is a sentence.
(prop-as-string
 (proposition '(not (not (not (not F))))))

;; 6.B
;; No. (Unless perhaps you include ⊤ and ⊥ but the book has not.)
