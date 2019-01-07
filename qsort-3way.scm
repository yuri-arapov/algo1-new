
(define *dbg* #f)

(define (debug-print . args)
  (if *dbg*
      (apply format #t args)))


(define *pivot-fn-first-count* 0)
(define *pivot-fn-median-count* 0)
(define *pivot-fn-last-count* 0)


(define (reset-pivot-counts)
  (set! *pivot-fn-first-count* 0)
  (set! *pivot-fn-last-count* 0)
  (set! *pivot-fn-median-count* 0))


(define (pivot-fn-first a l r)
  (set! *pivot-fn-first-count* (+ *pivot-fn-first-count* (- r l)))
  l)


(define (pivot-fn-last a l r)
  (set! *pivot-fn-last-count* (+ *pivot-fn-last-count* (- r l)))
  r)


(define (pivot-fn-median a l r)
  (set! *pivot-fn-median-count* (+ *pivot-fn-median-count* (- r l)))
  (let ((m (quotient (+ l r) 2)))
    (debug-print "pivot-fn-median: l=~a r=~a m=~a\n" l r m)
    (let ((ll (vector-ref a l))
          (rr (vector-ref a r))
          (mm (vector-ref a m)))
      (cond ((< ll rr mm) r)
            ((< mm rr ll) r)
            ((< mm ll rr) l)
            ((< rr ll mm) l)
            (else m)))))


(define (qsort a pivot-fn)

  (define (inc n) (+ n 1))
  (define (dec n) (- n 1))

  (define (get x)
    (vector-ref a x))

  (define (set2! i i-val j j-val)
    (vector-set! a i i-val)
    (vector-set! a j j-val))

  (define (swap i j)
    (if (not (= i j))
        (set2! i (get j) j (get i))))

  (define (partition l r p)
    (debug-print "partition: l=~a r=~a p=~a\n" l r p)
    (let ((pp (get p)))
      (swap l p)
      (let loop ((i l)
                 (j (inc l)))
        (cond ((> j r) ; end of scan
               (swap l i)
               i)

              ((< (get j) pp) ; a[j] is less than pivot
               (swap (inc i) j)
               (loop (inc i) (inc j)))

              (else ; a[j] is greater than pivot
                (loop i (inc j)))))))

  (let qs ((l 0) (r (dec (vector-length a))))
    (debug-print "qs: l=~a r=~a\n" l r)
    (if (> r l)
        (let ((p (partition l r (pivot-fn a l r))))
          (qs l (dec p))
          (qs (inc p) r))))

  a) ;; return sorted array



(define test-array (list->vector (iota 10)))


;; (162085 164123 138382)
(define (week3)
  (let ((data (map string->number (read-file "qsort.txt"))))
    (format #t "qsort.txt: ~a numbers\n" (length data))
    (reset-pivot-counts)
    (qsort (list->vector data) pivot-fn-first)
    (qsort (list->vector data) pivot-fn-last)
    (qsort (list->vector data) pivot-fn-median)
    (list *pivot-fn-first-count* *pivot-fn-last-count* *pivot-fn-median-count*)))


