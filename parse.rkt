#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? datum?)          (Lit s)]
    [(list (? op1? o) e) (Prim1 o (parse e))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    ;; TODO: Handle case
    [(list 'cond cs ... (list 'else e)) (Cond (parse_cond cs) (parse e))]
    [(list 'case e1 cs ... (list 'else e2)) (Case (parse e1) (parse_case cs) (parse e2))]
    ;; TODO: Remove this clause once you've added clauses for
    ;; parsing cond and case; it's here just so running the test suite
    ;; doesn't trigger parse errors.
    [_ (error "Parse error")]))

;; S-Expr -> Clause
(define (parse_cond cs)
  (match cs
  [(list (list e1 e2) a ...) (cons (Clause (parse e1) (parse e2)) (parse_cond a))]
  [_ '()]))

;; S-Expr -> Clause
(define (parse_case cs)
  (match cs
  [(list (list lst e) a ...) (cons (Clause (parse_list lst) (parse e)) (parse_case a))]
  [_ '()]))
;; Listof Datum -> Listof Lit
(define (parse_list lst)
  (match lst
  [(list (? datum? d) a ...) (cons (Lit d) (parse_list a))]
  [_ '()]))

;; Any -> Boolean
(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)))

(define (op1? x)
  (memq x '(add1
            sub1
            zero?
            abs
            -
            not)))



