#lang racket

(require "prop.rkt")

;; Exercises

;; 5.D8 If John Coltrane played tuba then Miles Davis played neither trumpet nor tuba.
(define/contract V-D8 prop-PROP?
  '(→ J₃ (∧ (¬ M₁) (¬ M₂))))

(displayln (prop-as-string V-D8))
(displayln)

;; 5.G1 ​
;; If Dorothy plays the piano in the morning [D₁], then Roger wakes up
;; cranky [R]. Dorothy plays piano in the morning unless she is
;; distracted [D₂]. So if Roger does not wake up cranky, then Dorothy must
;; be distracted.
(define/contract V-G1 (listof prop-PROP?)
  (list
   '(→ D₁ R)
   '(→ (¬ D₂) D₁)
   '(→ (¬ R) D₂)))

(argument-display V-G1)
(displayln)

;; 
