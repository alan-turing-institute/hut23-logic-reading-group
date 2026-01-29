#lang racket

(require "prop.rkt")

;; Exercises from ∀x
;;
;; 5.D.8 ​If John Coltrane played tuba then Miles Davis played neither trumpet nor tuba. 
;; J₃ -> ¬M₁ ∧ ¬M₂
(define/contract D8 prop/c
  '(→ J₃ (∧ (¬ M₁) (¬ M₂))))
