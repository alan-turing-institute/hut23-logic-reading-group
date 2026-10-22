;; Variables
;; - x, y, foo, bar, zero?, snake-case, ...
;; - NOT lambda, λ, (, ), [, ], ...

;; Application
;; (E F)

;; Abstraction
;; (λ (x) E)

;; Extensions
;; In the λ-calculus E F G means ((E F) G)
;; Maybe we might want (F x y) to mean ((F x) y)

;; λxy.E means λx.λy.E
;; Maybe we could allow (λ (x y) E)

;; let x = 3
;; x + 4

