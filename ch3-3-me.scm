; 3.13

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

; 3.16

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define endless-count
  (let ((x (list 1 2 3)))
    (set-cdr! (cddr x) x)
    x))

(define four-count
  (let ((x (list 1 2 3)))
    (set-car! x (cddr x))
    x))

(define seven-count
  (let ((x (list 1 2 3)))
    (set-car! x (cdr x))
    (set-car! (cdr x) (cddr x))
    x))

; 3.17

(define (elem-eq? e set)
  (cond
    ((null? set) #f)
    ((eq? (car set) e) #t)
    (else (elem-eq? e (cdr set)))))

(define (count-pairs x)
  (define (count x counted-set)
    (cond
      ((not (pair? x)) 0)
      ((elem-eq? x counted-set) 0)
      (else
       (let ((new-set (cons x counted-set)))
         (+ 1 
            (count (car x) new-set)
            (count (cdr x) (cons (car x) new-set)))))))
  (count x '()))

(define (contains-cycle? x)
  (define (check-cycle x already-visited)
    (cond
      ((null? x) #f)
      ((elem-eq? (cdr x) already-visited) #t)
      (else (check-cycle (cdr x) (cons x already-visited)))))
  (check-cycle x '()))

(define (contains-cycle?-2 x)
  (define (check-cycle t1 t2)
    (cond
      ((or (null? t2) (null? (cdr t2))) #f)
      ((eq? t1 t2) #t)
      (else (check-cycle (cdr t1) (cddr t2)))))
  (if (null? x)
      #f
      (check-cycle x (cdr x))))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (begin
           (set-front-ptr! queue (cdr (front-ptr queue)))
           (if (null? (front-ptr queue))
               (set-rear-ptr! queue '()))
           queue))))

(define (print-queue queue)
  (front-ptr queue))

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (set-front-ptr! p) (set! front-ptr p))
    (define (set-rear-ptr! p) (set! rear-ptr p))
    (define (insert-queue! x)
      (let ((new-pair (cons x '())))
        (if (empty-queue?)
            (begin
              (set-front-ptr! new-pair)
              (set-rear-ptr! new-pair)
              dispatch)
            (begin
              (set-cdr! rear-ptr new-pair)
              (set-rear-ptr! new-pair)
              dispatch))))
    
    (define (delete-queue!)
      (if (empty-queue?)
          (lambda (x) "Error: called delete on empty queue")
          (begin
            (set-front-ptr! (cdr front-ptr))
            (if (null? front-ptr)
                (set-rear-ptr! '()))
            dispatch)))
    
    (define (front-queue)
      (if (empty-queue?)
          (lambda (x) "Error: called front-queue on empty queue")
          (car front-ptr)))
    
    (define (dispatch m)
      (cond
        ((eq? 'empty-queue? m) empty-queue?)
        ((eq? 'insert-queue! m) insert-queue!)
        ((eq? 'front-queue m) front-queue)
        ((eq? 'delete-queue! m) delete-queue!)
        ((eq? 'print-queue m) (lambda () front-ptr))
        (else "Unknown queue function")))
    dispatch))

(define (empty-queue? q)
  ((q 'empty-queue?)))
(define (insert-queue! q x)
  ((q 'insert-queue!) x))
(define (delete-queue! q)
  ((q 'delete-queue!)))
(define (front-queue q)
  ((q 'front-queue)))
(define (print-queue q)
  ((q 'print-queue)))

(define (front-deque-ptr queue) (car queue))
(define (rear-deque-ptr queue) (cdr queue))
(define (triple-next x) (list-ref x 1))
(define (triple-prev x) (list-ref x 2))
(define (triple-element x) (list-ref x 0))
(define (make-triple elem next prev)
  (list elem next prev))
(define (set-next! x item)
  (set-cdr! x (list item (triple-prev x))))
(define (set-prev! x item)
  (set-cdr! (cdr x) item))
(define (set-front-deque-ptr! queue item) (set-car! queue item))
(define (set-rear-deque-ptr! queue item) (set-cdr! queue item))

(define (empty-deque? queue) (null? (front-deque-ptr queue)))

(define (make-deque) (cons '() '()))

(define (front-deque queue)
  (if (empty-deque? queue)
      (error "FRONT called with an empty deque" queue)
      (triple-element (front-deque-ptr queue))))

(define (rear-deque queue)
  (if (empty-deque? queue)
      (error "REAR called with an empty deque" queue)
      (triple-element (rear-deque-ptr queue))))

(define (rear-insert-deque! queue item)
  (let ((new-triple (make-triple item '() (rear-deque-ptr queue))))
    (cond ((empty-deque? queue)
           (set-front-deque-ptr! queue new-triple)
           (set-rear-deque-ptr! queue new-triple)
           queue)
          (else
           (set-next! (rear-deque-ptr queue) new-triple)
           (set-rear-deque-ptr! queue new-triple)
           queue))))

(define (front-insert-deque! queue item)
  (let ((new-triple (make-triple item (front-deque-ptr queue) '())))
    (cond ((empty-deque? queue)
           (set-front-deque-ptr! queue new-triple)
           (set-rear-deque-ptr! queue new-triple)
           queue)
          (else
           (set-prev! (front-deque-ptr queue) new-triple)
           (set-front-deque-ptr! queue new-triple)
           queue))))

(define (front-delete-deque! queue)
  (cond ((empty-deque? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (begin
           (set-front-deque-ptr! queue (triple-next (front-deque-ptr queue)))
           (if (null? (front-deque-ptr queue))
               (set-rear-deque-ptr! queue '())
               (set-prev! (front-deque-ptr queue) '()))
           queue))))

(define (rear-delete-deque! queue)
  (cond ((empty-deque? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (begin
           (set-rear-deque-ptr! queue (triple-prev (rear-deque-ptr queue)))
           (if (null? (rear-deque-ptr queue))
               (set-front-deque-ptr! queue '())
               (set-next! (rear-deque-ptr queue) '()))
           queue))))

(define (print-deque queue)
  (define (p-dq current-node)
    (if (null? current-node)
        '()
        (cons (triple-element current-node)
              (p-dq (triple-next current-node)))))
  (p-dq (front-deque-ptr queue)))

; 3.3.3 tables

(define (lookup key table)
  (let ((record (assoc-s key (cdr table))))
    (if record
        (cdr record)
        #f)))
(define (assoc-s key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc-s key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc-s key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (assoc-s same-key? key records)
  (cond ((null? records) #f)
        ((same-key? key (caar records)) (car records))
        (else (assoc-s same-key? key (cdr records)))))

(define (make-table-old same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc-s same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc-s same-key? key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc-s same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc-s same-key? key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (inspect)
      local-table)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'inspect) inspect)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (lookup key-1 key-2 table)
  ((table 'lookup-proc) key-1 key-2))
(define (insert! key-1 key-2 value table)
  ((table 'insert-proc!) key-1 key-2 value))
(define (inspect table)
  ((table 'inspect)))

(define (debug-call f)
  (lambda args
    (begin 
      (map
       (lambda (elem) 
         (begin
           (display elem)
           (display "\n")
           '())) args)
      (apply f args))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup . keys)
      (define (local-lookup table key . more-keys)
        (let ((found-value (assoc-s same-key? key (cdr table))))
          (cond
            ((and (null? more-keys) (not found-value)) #f)
            ((null? more-keys) (cdr found-value))
            (else (apply local-lookup 
                         (cons found-value more-keys))))))
      (apply local-lookup (cons local-table keys)))
    
    (define (insert! value . keys)
      (define (local-insert! table value key . more-keys)
        (let ((found-value (assoc-s same-key? key (cdr table))))
          (if (null? more-keys)
              (if found-value
                  (set-cdr! found-value value)
                  (set-cdr! table
                            (cons (cons key value) (cdr table))))
              (let ((next-table
                     ; Make the sub-table if it doesn't exist
                     (if (not found-value)
                         (begin
                           (local-insert! table '() key)
                           (assoc-s equal? key (cdr table)))
                         found-value)))
                (apply local-insert! (append (list next-table value) more-keys))))))
      (apply local-insert!
             (append (list local-table value) keys))
      'ok)
    (define (inspect)
      local-table)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'inspect) inspect)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (lookup table . keys)
  (apply (table 'lookup) keys))
(define (insert! table value . keys)
  (apply (table 'insert!) (append (list value) keys)))
(define (inspect table)
  ((table 'inspect)))

(define operation-table (make-table equal?))
(define get 
  (lambda keys 
    (apply lookup (cons operation-table keys))))
(define put 
  (lambda (value . keys)
    (apply insert! (cons operation-table (cons value keys)))))

; 3.27

(define (memoize same-key? f)
  (let ((table (make-table same-key?)))
    (lambda (x)
      (let ((previously-computed-result (lookup table x)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! table result x)
              result))))))

(define (fib-cont cont)
  (lambda (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (cont (- n 1))
                   (cont (- n 2)))))))

(define (memoize-continuation-fn f same-key?)
  (define memo-internal
    (memoize same-key?
             (lambda (x) ((f memo-internal) x))))
  memo-internal)

(define curried-add
  (lambda (x) x))

(define (lazy-call f)
  (lambda args
    (cond 
      ((null? args) f)
      ((procedure? f) (apply (lazy-call (f (car args))) (cdr args)))
      (else f))))

(define y-com
  (lambda (f)
    (lambda (x) 
       (f 
        (lambda (recurse)
          ((x x) recurse)))
     (lambda (x)
       (f 
        (lambda (recurse)
          ((x x) recurse)))))))

(define fact 
  (y-com 
   (lambda (f) 
     (lambda (n) 
       (if (= 0 n) 
           1 
           (* n (f (- n 1))))))))

;(f (lambda (r) ((lambda (x) (f (lambda (r) (x x)))) (lambda (x) (f (lambda (r) (x x)))))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memo-fib
  (memoize =
           (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

