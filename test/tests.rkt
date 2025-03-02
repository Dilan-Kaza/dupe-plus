#lang racket

(provide make-dupe+-test-suite
         run-tests)

(require rackunit
         rackunit/text-ui)

(define (make-dupe+-test-suite name run-proc)
  (test-suite
   name

   ;; Abscond examples
   (test-suite
    "Abscond"
    (check-equal? (run-proc 7) 7)
    (check-equal? (run-proc -8) -8))

   ;; Blackmail examples
   (test-suite
    "Blackmail"
    (check-equal? (run-proc '(add1 (add1 7))) 9)
    (check-equal? (run-proc '(add1 (sub1 7))) 7))

   ;; Con examples
   (test-suite
    "Con"
    (check-equal? (run-proc '(if (zero? 0) 1 2)) 1)
    (check-equal? (run-proc '(if (zero? 1) 1 2)) 2)
    (check-equal? (run-proc '(if (zero? -7) 1 2)) 2)
    (check-equal? (run-proc '(if (zero? 0)
                                 (if (zero? 1) 1 2)
                                 7))
                  2)
    (check-equal? (run-proc '(if (zero? (if (zero? 0) 1 0))
                                 (if (zero? 1) 1 2)
                                 7))
                  7))

   ;; Con+ examples
   (test-suite
    "Con+"
    (check-equal? (run-proc '(abs 10)) 10)
    (check-equal? (run-proc '(abs -10)) 10)
    (check-equal? (run-proc '(- (abs -10))) -10)
    (check-equal? (run-proc '(- 10)) -10)
    (check-equal? (run-proc '(- -10)) 10)
    (check-equal? (run-proc '(- (- 10))) 10)
    (check-equal? (run-proc '(cond [else 5])) 5)
    (check-equal? (run-proc '(cond [else (- 10)])) -10)
    (check-equal? (run-proc '(cond [(zero? 1) 2] [else 3])) 3)
    (check-equal? (run-proc '(cond [(zero? 0) 2] [else 3])) 2)
    (check-equal? (run-proc '(cond [(zero? 1) 2] [(zero? (sub1 1)) 4] [else 3])) 4)
    (check-equal? (run-proc '(cond [(zero? 1) 1] [(zero? 2) 2] [(zero? 3) 3] [(zero? 4) 4] [(zero? 5) 5] [else 6])) 6))

   ;; Dupe examples
   (test-suite
    "Dupe"
    (check-equal? (run-proc #t) #t)
    (check-equal? (run-proc #f) #f)
    (check-equal? (run-proc '(if (zero? 0)
                                 (if (zero? 1) #t #f)
                                 7))
                  #f))

   ;; Dupe+ examples
   (test-suite
    "Dupe+"
    (check-equal? (run-proc '(not #t)) #f)
    (check-equal? (run-proc '(not #f)) #t)
    (check-equal? (run-proc '(not 7)) #f)
    (check-equal? (run-proc '(cond [else #t])) #t)
    (check-equal? (run-proc '(cond [(not #t) 2] [else 3])) 3)
    (check-equal? (run-proc '(cond [(if #t #t #f) 2] [else 3])) 2)
    (check-equal? (run-proc '(cond [(zero? 1) 2] [(if (not (zero? (sub1 2))) #t #f) 4] [else 3])) 4)
    (check-equal? (run-proc '(cond [#t 1] [else 2])) 1)
    (check-equal? (run-proc '(cond [1 1] [else 2])) 1)

    (check-equal? (run-proc '(case 2 [else 1])) 1)
    (check-equal? (run-proc '(case 2 [() 3] [else 1])) 1)
    (check-equal? (run-proc '(case 2 [(2) 3] [else 1])) 3)
    (check-equal? (run-proc '(case 4 [(2) 3] [else 1])) 1)
    (check-equal? (run-proc '(case 2 [(7 2) 3] [else 1])) 3)
    (check-equal? (run-proc '(case 4 [(7 2) 3] [else 1])) 1)
    (check-equal? (run-proc '(case 2 [(7 2 #t) 3] [else 1])) 3)
    (check-equal? (run-proc '(case 4 [(7 2 #t) 3] [else 1])) 1)
    (check-equal? (run-proc '(case #t [(7 2 #t) 3] [else 1])) 3)
    (check-equal? (run-proc '(case #f [(7 2 #t) 3] [else 1])) 1))

   ;; Student's examples
   (test-suite
    "Student"
    ))

  )
