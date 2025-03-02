#lang racket

(provide run-compile
         compile-test-suite)

(require "../compile.rkt"
         "../parse.rkt"
         "../types.rkt"

         "tests.rkt"

         a86/interp)

(define (run-compile e)
  (bits->value (asm-interp (compile (parse e)))))

(define compile-test-suite (make-dupe+-test-suite "compile" run-compile))

(module+ test
  (run-tests compile-test-suite))
