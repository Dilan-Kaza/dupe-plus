#lang racket
(provide interp)
(require "ast.rkt")
(require "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Value
(define (interp e)
  (match e
    [(Lit d) d]
    [(Prim1 p e)
     (interp-prim1 p (interp e))]
    ;; TODO: Handle cond
    [(Cond cs e) (interp_cond cs e)]
    ;; TODO: Handle case    
    [(Case e1 cs e2) (interp_case e1 cs e2)]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]))

;; Expr -> Value
(define (interp_cond cs e)
  (match cs
    [(list (Clause e1 e2) a ...) (if (interp e1) (interp e2) (interp_cond a e))]
    ['() (interp e)]))
;; Expr -> Value
(define (interp_case e1 cs e2)
  (match cs
    [(list (Clause lst e) a ...) (if (member (interp e1) (interp_list lst)) (interp e) (interp_case e1 a e2))]
    ['() (interp e2)]))
;; Listof Lit -> Listof Value
(define (interp_list lst)
  (match lst
    [(list (Lit s) a ...) (cons s (interp_list a))]
    ['() '()]))