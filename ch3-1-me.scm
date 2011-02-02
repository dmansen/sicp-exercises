; 3.1

(define (make-accumulator initial)
  (let ((current initial))
    (lambda (x)
      (set! current (+ current x))
      current)))

; 3.2

(define (make-monitored f)
  (let ((call-count 0))
    (lambda args
      (cond 
        ((and (= 1 (length args))
              (eq? (car args) 'how-many-calls?))
         call-count)
        ((and (= 1 (length args))
              (eq? (car args) 'reset-count))
         (begin (set! call-count 0) 'reset))
        (else (let ((res (apply f args)))
                (set! call-count (+ 1 call-count))
                res))))))

; 3.3

(define (call-the-cops)
  "Cops called")

(define (make-account balance password)
  (let ((bad-pw-count 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pass m)
      (if (not (eq? pass password))
          (if (> bad-pw-count 7)
              (call-the-cops)
              (begin
                (set! bad-pw-count (+ 1 bad-pw-count))
                (lambda (x) "Incorrect password")))
          (begin
            (set! bad-pw-count 0)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request -- MAKE-ACCOUNT"
                               m))))))
    dispatch))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define rand
  (let ((x (random-in-range 0 1000)))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; 3.5

(define (estimate-integral p x1 x2 y1 y2 num-trials)
  (define (experiment)
    (p (random-in-range x1 x2) (random-in-range y1 y2)))
  (monte-carlo num-trials experiment))

(define (make-in-circle radius x-center y-center)
  (lambda (x y)
    (let ((x-term (- x x-center))
          (y-term (- y y-center)))
      (<= (+ (* x-term x-term) (* y-term y-term)) (* radius radius)))))

; 3.7


(define (call-the-cops)
  "Cops called")

(define (make-account balance pw)
  (let ((bad-pw-count 0)
        (password pw))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (check-balance)
      balance)
    (define (check-password pass)
      (eq? pass password))
    (define (dispatch pass m)
      (if (not (eq? pass password))
          (if (> bad-pw-count 7)
              (call-the-cops)
              (begin
                (set! bad-pw-count (+ 1 bad-pw-count))
                (lambda (x) "Incorrect password")))
          (begin
            (set! bad-pw-count 0)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  ((eq? m 'balance) check-balance)
                  ((eq? m 'check-password) check-password)
                  (else (error "Unknown request -- MAKE-ACCOUNT"
                               m))))))
    dispatch))

(define (make-joint account old-pw new-pw)
  (define (new-dispatch pass m)
    (if (eq? pass new-pw)
        (account old-pw m)
        (lambda (x) "Incorrect password")))
  (if ((account old-pw 'check-password) old-pw)
      new-dispatch
      "Incorrect password, cannot create joint account"))

; 3.8

(define f
  (let ((first-call? #t))
    (lambda (x)
      (if first-call?
          (begin
            (set! first-call? #f)
            x)
          0))))

(define (get-keymap key map)
  (cond
    ((= 0 (length map)) key)
    ((eq? (caar map) key) (cdar map))
    (else (get-keymap key (cdr map)))))

(define (replace-symbols mapping body)
  (define (inner-replace prev rest)
    (cond
      ((null? rest) (reverse prev))
      ((symbol? (car rest)) (inner-replace (cons (get-keymap (car rest) mapping) prev) (cdr rest)))
      ((list? (car rest)) (inner-replace (cons (replace-symbols mapping (car rest)) prev) (cdr rest)))
      (else (inner-replace (cons (car rest) prev) (cdr rest)))))
  (inner-replace '() body))

(define (procedures-equal? p-arglist q-arglist p-body q-body)
  (if (not (= (length p-arglist) (length q-arglist)))
      #f
      (let ((arg-mapping (map cons p-arglist q-arglist)))
        (equal? q-body (replace-symbols p-body arg-mapping)))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

