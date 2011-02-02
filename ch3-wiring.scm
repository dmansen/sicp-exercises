(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s t)
  (if (and (= s 1) (= t 1))
      1
      0))

(define (logical-or s t)
  (if (or (= s 1) (= t 1))
      1
      0))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure))

(define (alt-or-gate s t a)
  (let ((not-s (make-wire))
        (not-t (make-wire)))
    (inverter s not-s)
    (inverter t not-t)
    (and-gate not-s not-t a)
    'ok))

(define (ripple-carry-adder a-wires b-wires res-wires carry-wire)
  (cond
    ((null? a-wires) 'ok)
    (else (let ((new-carry (make-wire)))
            (full-adder (car a-wires) (car b-wires) carry-wire (car res-wires) new-carry)
            (ripple-carry-adder (cdr a-wire) (cdr b-wires) (cdr res-wires) new-carry)))))
      
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ (current-time the-agenda) delay)
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

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

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda)
  (cons 0 (make-queue)))

(define (empty-agenda? a)
  (empty-queue? (cdr a)))

(define (first-agenda-item a)
  (cdr (front-queue (cdr a))))

(define (remove-first-agenda-item! a)
  (delete-queue! (cdr a)))

(define (add-to-agenda! time action agenda)
  (define (add-time)
    (if (< (current-time agenda) time)
        (set-car! agenda time))
    (action))
  (insert-queue! (cdr agenda) (cons time add-time)))

(define (current-time agenda)
  (car agenda))

(define (display-agenda a)
  (((cdr a) 'print-queue)))

(define the-agenda (make-agenda))

(define or-gate-delay 2)
(define and-gate-delay 3)
(define inverter-delay 5)

(define (probe name wire)
  (add-action! wire
               (lambda ()        
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
(probe 'carry carry)
(half-adder input-1 input-2 sum carry)