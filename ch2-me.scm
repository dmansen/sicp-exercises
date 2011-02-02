(define (ex1-1 n)
  (if (< 3 n)
    n
    (+ (ex1-1 (- 1 n)) (* 2 (ex1-1 (- 2 n))) (* 3 (ex1-1 (- 3 n))))))

(define (fast-expt b n)
  (define (f b n a)
    (cond
      ((= 0 n) a)
      ((even? n) (f (* b b) (/ n 2) a))
      (else (f b (- n 1) (* a b)))))
  (f b n 1))

(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car z)
  (z (lambda (p q) p)))

(define (my-cdr z)
  (z (lambda (p q) q)))

(define (i-cons x y)
  (* (fast-expt 2 x) (fast-expt 3 y)))

(define (i-car x)
  (define (car-h y acc)
    (if (= 0 (remainder y 2))
      (car-h (/ y 2) (+ 1 acc))
      acc))
  (car-h x 0))

(define (i-cdr x)
  (define (cdr-h y acc)
    (if (= 0 (remainder y 3))
      (cdr-h (/ y 3) (+ 1 acc))
      acc))
  (cdr-h x 0))

(define (foldl op init seq)
  (if (null? seq)
    init
    (foldl op (op (car seq) init) (cdr seq))))

(define (foldr op init seq)
  (if (null? seq)
      init
      (op (car seq) (foldr op init (cdr seq)))))

(define (m-append s1 s2)
  (foldr cons s2 (reverse s1)))

(define (m-length xs)
  (foldr (lambda (x y) (+ 1 y)) 0 xs))

(define (horner-eval x coefficient-sequence)
  (foldr (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(define (alt-horner x coefficient-sequence)
  (foldl (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms))) 
         0 
         (reverse coefficient-sequence)))

(define nil '())

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (foldr op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define accumulate foldr)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define (filter p xs)
  (foldr (lambda (x rest)
           (if (p x)
               (cons x rest)
               rest))
         nil
         xs))

(define (flatmap f xs)
  (foldr append nil (map f xs)))

(define (enumerate-interval min max)
  (if (> min max)
      nil
      (cons min (enumerate-interval (+ 1 min) max))))

(define (reverse-1 sequence)
  (foldr (lambda (x y) (append y (list x))) nil sequence))
(define (reverse-2 sequence)
  (foldl (lambda (x y) (cons x y)) nil sequence))

(define (unique-pairs n)
  (flatmap (lambda (i) 
             (map (lambda (j) (list i j)) 
                  (enumerate-interval 1 (- i 1)))) 
           (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap 
   (lambda (x)
     (flatmap
      (lambda (y)
        (map
         (lambda (z)
           (list x y z))
         (enumerate-interval 1 (- y 1))))
      (enumerate-interval 1 (- x 1))))
   (enumerate-interval 3 n)))
              
(define (make-pair-sum p)
  (list (car p) (cadr p) (+ (car p) (cadr p))))

(define (prime-sum? p)
  (prime? (+ (car p) (cadr p))))

(define (prime? n)
  (foldl (lambda (x p?) 
           (if (= 0 (remainder n x))
               #f
               p?))
         #t
         (enumerate-interval 2 (floor (sqrt n)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(define (all-triple-sums n s)
  (filter (lambda (x) (= s (foldl + 0 x))) (unique-triples n)))

(define empty-board nil)

(define (adjoin-position row k rest-of-queens)
  (cons (list k row) rest-of-queens))

(define (find p? xs)
  (foldr
   (lambda (x y)
     (if (and (null? y) (p? x))
         x
         y))
   nil xs))

(define (pos-at-col k positions)
  (cadr (find (lambda (x) (= k (car x))) positions)))

(define (positions-safe col1 row1 col2 row2)
  (not (or
        (= row1 row2)
        (= (abs (- col1 col2)) (abs (- row1 row2))))))

(define (safe? k positions)
  (let ((k-row (pos-at-col k positions)))
   (foldr 
    (lambda (x y)
      (and y
           (positions-safe k k-row (car x) (cadr x)))) #t (filter (lambda (pos) (not (= (car pos) k))) positions))))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; Image processing game

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (counterclockwise-270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (counterclockwise-270 (beside (rotate90 painter2) (rotate90 painter1))))

(define (split orig-placer split-placer)
  (lambda (painter n)
    (if (= 0 n)
      painter
      (let ((smaller ((split orig-placer split-placer) painter (- n 1))))
        (orig-placer painter (split-placer smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

; Vectors: 2.46

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v u)
  (make-vect
   (+ (xcor-vect v) (xcor-vect u))
   (+ (ycor-vect v) (ycor-vect u))))

(define (sub-vect v u)
  (make-vect
   (- (xcor-vect v) (xcor-vect u))
   (- (ycor-vect v) (ycor-vect u))))

(define (scale-vect s v)
  (make-vect
   (* (xcor-vect v) s)
   (* (ycor-vect v) s)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

; 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (list-ref f 0))
(define (edge1-frame f)
  (list-ref f 1))
(define (edge2-frame f)
  (list-ref f 2))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (cddr f))

; Leading up to 2.48

(define (make-segment start-vector end-vector)
  (cons start-vector end-vector))
(define (start-segment line)
  (car line))
(define (end-segment line)
  (cdr line))

(define (draw-line p1 p2)
  (display p1)
  (display " -> ")
  (display p2)
  (display "\n"))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

; 2.49

; a
(define outline-frame
  (segments->painter 
   (list 
    (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
    (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0))
    (make-segment (make-vect 1.0 1.0) (make-vect 0.0 1.0))
    (make-segment (make-vect 0.0 1.0) (make-vect 0.0 0.0)))))

(define fr (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1)))

; b
(define pretty-print
  (segments->painter 
   (list 
    (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
    (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0)))))

; Transforming and combining

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

; Exercise 2.50.  Define the transformation flip-horiz, which flips painters 
; horizontally, and transformations that rotate painters counterclockwise 
; by 180 degrees and 270 degrees.

; Standard co-ordinates
; O: (0, 0)
;E1: (1, 0)
;E2: (0, 1)

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (counterclockwise-180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (below painter1 painter2)
  (lambda (frame)
    (let ((bottom (transform-painter 
                   painter1
                   (make-vect 0.0 0.0)
                   (make-vect 1.0 0.0)
                   (make-vect 0.0 0.5)))
          (top (transform-painter 
                painter2
                (make-vect 0.0 0.5)
                (make-vect 1.0 0.5)
                (make-vect 0.0 1.0))))
      (bottom frame)
      (top frame))))

(define weird-painter
  (segments->painter
   (list
    (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0))
    (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.8)))))
