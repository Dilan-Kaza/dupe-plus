#lang racket

(provide run-interp
         interp-test-suite)

(require "../interp.rkt"
         "../parse.rkt"

         "tests.rkt")

(define (run-interp e)
  (interp (parse e)))

(define interp-test-suite (make-dupe+-test-suite "interp" run-interp))

(module+ test
  (run-tests interp-test-suite))
