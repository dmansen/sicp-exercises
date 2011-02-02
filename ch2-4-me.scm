(define (error msg what)
  (display "Error: ")
  (display msg)
  (display "\n")
  (display what)
  (display "\n"))

(define dispatch-table '())
(define (put op type fn)
  (set! dispatch-table (cons (list op type fn) dispatch-table)))
(define (get op type)
  (define (find table)
    (cond
      ((null? table) (error "couldn't find dispatch function" (list op type)))
      ((and 
        (eq? (caar table) op)
        (eq? (cadr (car table)) type))
       (list-ref (car table) 2))
      (else (find (cdr table)))))
  (find dispatch-table))

(define (make-sum x y)
  (cond
    ((and (number? x) (= x 0)) y)
    ((and (number? y) (= y 0)) x)
    ((and (number? x) (number? y)) (+ x y))
    (else (list '+ x y))))

(define (make-product x y)
  (cond
    ((and (number? x) (= x 1)) y)
    ((and (number? y) (= y 1)) x)
    ((or (and (number? x) (= x 0))
         (and (number? y) (= y 0)))
     0)
    (else (list '* x y))))

(define (addend exp) (car exp))
(define (augend exp) (cadr exp))
(define (multiplier exp) (car exp))
(define (multiplicand exp) (cadr exp))

(define (variable? x)
  (symbol? x))

(define (same-variable? x y)
  (eq? x y))

(define (install-deriv-package)
  (define (sum-der exp var)
    (make-sum 
     (deriv (addend exp) var)
     (deriv (augend exp) var)))
  (define (prod-der exp var)
    (make-sum
      (make-product
       (multiplier exp)
       (deriv (multiplicand exp) var))
      (make-product
       (deriv (multiplier exp) var)
       (multiplicand exp))))
  (put 'deriv '+ sum-der)
  (put 'deriv '* prod-der)
  'done)

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; 2.74

(define (element-keyset? key set)
  (cond 
    ((null? set) #f)
    ((eq? key (caar set)) #t)
    (else (element-keyset? key (cdr set)))))

(define (get-keyset key set)
  (cond
    ((null? set) '())
    ((eq? (caar set) key) (cadar set))
    (else (get-keyset key (cdr set)))))

(define (remove-keyset key set)
  (cond
    ((null? set) '())
    ((eq? (caar set) key) (cdr set))
    (else (cons (car set) (remove-keyset key (cdr set))))))

(define (adjoin-keyset key val set)
  (cons (list key val) (remove-keyset key set)))

(define empty-keyset '())

(define (type x)
  (car x))
(define (contents x) (cdr x))

(define (install-division1-structure)
  (define (tag val) (cons 'd1 val))
  (define (record file name)
    (if (null? (get-keyset name file))
        '()
        (tag (get-keyset name file))))
  (define (record->new address salary)
    (list address salary))
  (define (record->address rec)
    (car rec))
  (define (record->salary rec)
    (cadr rec))
  (put 'd1 'record record)
  (put 'd1 'record->address record->address)
  (put 'd1 'record->salary record->salary)
  (put 'd1 'record->new record->new))

(install-division1-structure)

(define sample-d1-file
  (cons 'd1 (adjoin-keyset 'mike ((get 'd1 'record->new) 'philly 36000) empty-keyset)))

(define (get-record name file)
  ((get (type file) 'record) (contents file) name))

(define (get-salary record)
  ((get (type record) 'record->salary) (contents record)))

(define (get-address record)
  ((get (type record) 'record->address) (contents record)))

(define (find-employee-record name files)
  (if (null? files)
      '()
      (let ((rec (get-record name (car files))))
        (if (null? rec)
            (find-employee-record name (cdr files))
            rec))))

; 2.75

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

