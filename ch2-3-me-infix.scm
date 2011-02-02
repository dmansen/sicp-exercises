(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum . a)
  (cond ((= (length a) 1) a)
        ((=number? (car a) 0) (apply make-sum (cdr a)))
        ((and (not (product? (cadr a))) (number? (car a)) (number? (cadr a)))
         (apply make-sum (cons (+ (car a) (cadr a)) (cddr a))))
        (else (cons (car a) (cons '+ (apply make-sum (cdr a)))))))

(define (make-product . m)
  (cond ((= (length m) 1) m)
        ((and (number? (cadr m)) (not (number? (car m))))
         (apply make-product (cons (cadr m) (cons (car m) (cddr m)))))
        ((=number? (car m) 0) 0)
        ((=number? (car m) 1) (apply make-product (cdr m)))
        ((=number? (cadr m) 0) 0)
        ((=number? (cadr m) 1) (apply make-product (cons (car m) (cddr m))))
        ((and (number? (car m)) (number? (cadr m)))
         (apply make-product (cons (* (car m) (cadr m)) (cddr m))))
        
        (else (cons (car m) (cons '+ (apply make-sum (cdr m)))))))

(define (sum? x)
  (and (pair? x) (not (null? (cdr x))) (eq? (cadr x) '+)))

(define (addend s) 
  (car s))

(define (augend s) 
  (if (= 1 (length (cddr s)))
      (caddr s)
      (cddr s)))
      

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) 
  (car p))

(define (multiplicand p)
  (if (= 1 (length (cddr p)))
      (caddr p)
      (cddr p)))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (make-exponentiation base exp)
  (cond ((=number? base 0) 0)
        ((=number? base 1) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp)) (expt base exp))
        (else (list base '** exp))))

(define (base expt)
  (car expt))

(define (exponent expt)
  (caddr expt))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))