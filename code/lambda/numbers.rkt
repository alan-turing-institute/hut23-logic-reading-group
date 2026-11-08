#lang racket

(require (only-in racket/base [eval base-eval]))
(require "lambda.rkt")

;; church : natural? -> sexp?
(define (church n)
  (define (church/body f x n)
    (if (zero? n)
        x
        `(,f ,(church/body f x (- n 1)))))
  `(λ (f)
     (λ (x)
       ,(church/body 'f 'x n))))

;; unchurch : sexp? -> natural?
(define (unchurch sexp)
  (base-eval `((,sexp add1) 0)))

;; λ-succ : Term? 
(define λ-succ
  '(λ (n) (λ (f) (λ (x) (f ((n f) x)))))
  )


;; ------------------------------------------------------------
;; Examples

(define (run1 sexp)
  (unparse (and
            (eval/step (parse sexp))
            sexp)))

(define (run sexp)
  (unparse (eval (parse sexp))))

;; 3

;; (unchurch
;;  (run `(,λ-succ ,(church 3))))

