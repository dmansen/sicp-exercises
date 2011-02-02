(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a s)
     (cons a (delay
               ((memo-proc (lambda () s))))))))

(define (stream-null? s)
  (not (pair? s)))

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-take s n)
  (if (= n 0)
      the-empty-stream
      (cons (stream-car s) (stream-take (stream-cdr s) (- n 1)))))
(define (stream-take-while p s)
  (if (p (stream-car s))
      (cons (stream-car s) (stream-take-while p (stream-cdr s)))
      the-empty-stream))
(define (stream-count-while p s)
  (define (acc s n)
    (if (p (stream-car s))
        (acc (stream-cdr s) (+ 1 n))
        n))
  (acc s 0))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define the-empty-stream
  '())

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (show x)
  (display-line x)
  x)

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (constant-stream n)
  (define stream
    (cons-stream n stream))
  stream)

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (* (stream-car ps) (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (sub-streams s1 s2)
  (stream-map - s1 s2))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define factorials
  (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

(define (partial-sums s)
  (define stream
    (cons-stream (stream-car s) (add-streams (stream-cdr s) stream)))
  stream)

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (merge (scale-stream integers 2) (scale-stream integers 3)) (scale-stream integers 5))))

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define (neg-stream s)
  (sub-streams (constant-stream 0) s))

(define (integrate-series s)
  (mul-streams (div-streams ones integers) s))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))
(define sine-series
  (cons-stream 0 (neg-stream (integrate-series cosine-series))))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2))))

(define sinpluscos
  (add-streams (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)))

(define (invert-unit-series s)
  (define stream
    (cons-stream 1 (neg-stream (mul-series (stream-cdr s) stream))))
  stream)

(define (div-series s1 s2)
  (if (= 0 (stream-car s2))
      (error "Can't divide if denominator stream has zero constant term")
      (mul-series s1 (invert-unit-series s2))))

(define tan-series
  (div-series sine-series cosine-series))

(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (square x)
  (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (stream-limit s tolerance)
  (define (helper stream)
    (let ((this (stream-car stream))
          (next (stream-car (stream-cdr stream))))
      (if (< (abs (- this next)) tolerance)
          next
          (helper (stream-cdr stream)))))
  (helper s))

(define (alternate-series s)
  (define (stream s sign)
    (cons-stream (* sign (stream-car s)) (stream (stream-cdr s) (* -1 sign))))
  (stream s 1))

(define (sqrt-me x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define ln2-1-series
  (partial-sums (alternate-series (div-streams ones integers))))

(define ln2-2
  (stream-limit (accelerated-sequence euler-transform ln2-1-series) 0.000001))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream 
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (p) (cons (stream-car s) p)) (pairs (stream-cdr t) (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define int-pairs
  (pairs integers integers))

(define prime-pairs 
  (stream-filter (lambda (pair)
                   (prime? (+ (car pair) (cadr pair))))
                 int-pairs))

(define (all-pairs s t)
  (cons-stream 
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-filter 
     (lambda (x)
       (not (= (car x) (cadr x))))
     (stream-map (lambda (x y) (list x y)) (stream-cdr s) t))
    (interleave
     (stream-map (lambda (x y) (list x y)) s (stream-cdr t))
     (all-pairs (stream-cdr s) (stream-cdr t))))))

(define pythag-triples
  (stream-filter
   (lambda (t)
     (let ((i (car t))
           (j (list-ref t 1))
           (k (list-ref t 2)))
       (= (+ (* i i) (* j j)) (* k k))))
   (triples integers integers integers)))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2))
               (w1 (weight s1car))
               (w2 (weight s2car))
           (

(define (pairs-w s t weight)
  (merge-weighted
   
