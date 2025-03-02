#lang racket
(provide (all-defined-out))
(require "ast.rkt")
(require "compile-ops.rkt")
(require "types.rkt")
(require a86/ast)

(define rax 'rax)

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Lit d)         (compile-value d)]
    [(Prim1 p e)     (compile-prim1 p e)]
    ;; TODO: Handle cond
    [(Cond cs e)     (compile-cond cs e)]
    ;; TODO: Handle case
    [(Case e1 cs e2) (compile-case e1 cs e2)]
    [(If e1 e2 e3)
     (compile-if e1 e2 e3)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Op1 Expr -> Asm
(define (compile-prim1 p e)
  (seq (compile-e e)
       (compile-op1 p)))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp rax (value->bits #f))
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

;; Listof Expr Expr -> Asm
(define (compile-cond cs e)
  (match cs
    [(list (Clause e1 e2) a ...)
     (let (
           (l1 (gensym 'cond))
           (l2 (gensym 'cond)))
          (seq (compile-e e1)
               (Cmp rax (value->bits #f))
               (Je l1)
               (compile-e e2)
               (Jmp l2)
               (Label l1)
               (compile-cond a e)
               (Label l2)))]
    ['() (compile-e e)]))




