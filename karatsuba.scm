;; Coursera.org
;;
;; Divide and Conquer, Sorting and Searching, and Randomized Algorithms
;; by Stanford University
;;
;; https://www.coursera.org/learn/algorithms-divide-conquer/home
;;
;; Week 1
;;
;; Karatsuba multiplication
;;
;; Written by Yuri Arapov <yuridichesky@gmail.com>
;;
;; Guile2 (GNU Scheme dialect)
;;

(define (kchar->digit c) (- (char->integer c) (char->integer #\0)))

(define (digit->kchar d) (integer->char (+ d (char->integer #\0))))

(define (string->k s) (map kchar->digit (string->list s)))

(define (k->string n) (list->string (map digit->kchar n)))

(define (digit s) (if (null? s) 0 (car s)))

(define (next s) (if (null? s) s (cdr s)))

;; compute a+b
(define (ksum2 a b)
  (let loop ((a (reverse a)) (b (reverse b)) (carry 0) (res '()))
    (if (and (null? a) (null? b))
        (if (positive? carry) (cons carry res) res)
        (let ((x (+ carry (digit a) (digit b))))
          (loop (next a) (next b) (quotient x 10) (cons (remainder x 10) res))))))

;; compute [a[+b[...]]]
(define (ksum . args)
  (let loop ((args args) (res '(0)))
    (if (null? args) res
        (loop (cdr args) (ksum2 res (car args))))))

;; compute [a[+b[...]]] and print result for debugging purposes
(define (k+ . args)
  (let ((res (apply ksum args)))
    ;;;(format #t "ksum ~a -> ~a\n" args res)
    res))

;; compute a-b
(define (ksub2 a b)
  (let loop ((a (reverse a)) (b (reverse b)) (x 0) (res '()))
    (if (and (null? a) (null? b))
        (if (positive? x) (begin (format #t "ksub2 error: negative result\n") #f)
            res)
        (let ((ac (digit a))
              (bc (digit b)))
          (if (>= ac (+ bc x))
              (loop (next a) (next b) 0 (cons (- ac (+ bc x)) res))
              (loop (next a) (next b) 1 (cons (- (+ 10 ac) (+ bc x)) res)))))))

;; compute a[-b[-c[...]]]
(define (ksub n . rest)
  (let loop ((args rest) (res n))
    (if (null? args) res
        (loop (cdr args) (ksub2 res (car args))))))

;; compute a[-b[-c[...]]] and print result for debugging purposes
(define (k- n . args)
  (let ((res (apply ksub n args)))
    ;;;(format #t "ksub ~a ~a -> ~a\n" n args res)
    res))

;; add 'len' zeros on the left (leading zeros)
(define (pad-left s len) (append (make-list len 0) s))

;; add 'len' zeros on the right (trailing zeros)
(define (pad-right s len) (append s (make-list len 0)))

;; get 'len' high digits (high part of the number)
(define (get-high s len) (take s len))

;; drop 'len' high digits and return rest of the digits (low part of the number)
(define (get-low s len) (drop s len))

;; drop leading zeros
(define (knorm n)
  (cond ((positive? (car n)) n)
        ((null? (cdr n)) n)
        (else (knorm (cdr n)))))

;; determine max of len1 len2, return "1" or even value
(define (max-len len1 len2)
  (let ((len (max len1 len2)))
    (cond ((= 1 len) len)
          ((even? len) len)
          (else (+ 1 len)))))

;; Karatsuba multiplication
;; n1 and n2 are either strings or lists of decimal digits
(define (karatsuba n1 n2)
  (if (string? n1)
    (k->string (karatsuba (string->k n1) (string->k n2)))
    (let* ((len1 (length n1))
           (len2 (length n2))
           (len (max-len len1 len2))
           (n1 (pad-left n1 (- len len1)))
           (n2 (pad-left n2 (- len len2))))
      (knorm
        (if (= 1 len)
          (let ((p (* (car n1) (car n2))))
            (list (quotient p 10) (remainder p 10)))
          (let* ((m2 (quotient len 2))
                 (high1 (get-high n1 m2))
                 (low1 (get-low n1 m2))
                 (high2 (get-high n2 m2))
                 (low2 (get-low n2 m2))
                 (z0 (karatsuba low1 low2))
                 (z1 (karatsuba (k+ low1 high1) (k+ low2 high2)))
                 (z2 (karatsuba high1 high2)))
            (k+ (pad-right z2 (* m2 2))
                (pad-right (k- z1 z2 z0) m2)
                z0)))))))


(define pi "3141592653589793238462643383279502884197169399375105820974944592")
(define e "2718281828459045235360287471352662497757247093699959574966967627")


(define (test)
  (karatsuba e pi))


