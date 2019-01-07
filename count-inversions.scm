

(define (count-inversions a)

  (define (merge-and-count-split-inversions b c)
    ;;;(format #t "merge-and-count-split-inversions: ~a ~a\n" b c)
    (let loop ((b b) (c c) (d '()) (count 0))
      (cond ((and (null? b) (null? c)) (cons (reverse d) count))
            ((null? c) (loop (cdr b) c       (cons (car b) d) count))
            ((null? b) (loop b       (cdr c) (cons (car c) d) count))
            (else
              (if (< (car b) (car c))
                  (loop (cdr b) c       (cons (car b) d) count)
                  (loop b       (cdr c) (cons (car c) d) (+ count (length b))))))))

  (define (sort-and-count-inversions a)
    ;;;(format #t "sort-and-count-inversions: ~a\n" a)
    (let ((len (length a)))
      (if (<= 0 len 1) (cons a 0)
        (let* ((half-len (quotient len 2))
               (b (sort-and-count-inversions (take a half-len)))
               (c (sort-and-count-inversions (drop a half-len)))
               (d (merge-and-count-split-inversions (car b) (car c))))
          ;;;(format #t "b=~a c=~a d=~a\n" b c d)
          (cons (car d) (+ (cdr b) (cdr c) (cdr d)))))))

  (cdr (sort-and-count-inversions a)))


;; Compute inversions count of numbers array read from file
;; Result: 2407905288
(define (week2)
  (count-inversions (map string->number (read-file "_bcb5c6658381416d19b01bfc1d3993b5_IntegerArray.txt"))))
