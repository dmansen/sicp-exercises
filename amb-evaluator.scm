(define true #t)
(define false #f)

(define (error s val)
  (newline)
  (display "Error: ")
  (display s)
  (newline)
  (display "Value: ")
  (display val))

(define apply-in-underlying-scheme apply)

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((amb? exp) (analyze-amb exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->lambda exp)))
        ((or? exp) (analyze (or->if exp)))
        ((and? exp) (analyze (and->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))
(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env                        
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; success continuation for this aproc
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;; success continuation for recursive
                                ;; call to get-args
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (make-application procedure params)
  (cons procedure params))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (list-ref exp 1))
(define (let-body exp) (cddr exp))
(define (let->lambda exp)
  (make-application 
   (make-lambda (map car (let-bindings exp)) (let-body exp))
   (map cadr (let-bindings exp))))

(define (and? exp)
  (tagged-list? exp 'and))
(define (no-and-predicates? preds) (null? preds))
(define (and-predicates exp) (cdr exp))
(define (last-and-predicate? preds) (and (not (null? preds)) (= 1 (length preds))))
(define (first-and-predicate preds)
  (car preds))
(define (rest-and-predicates preds)
  (cdr preds))
(define (and->if exp)
  (if (no-and-predicates? exp)
      'true
      (if (last-and-predicate? (and-predicates exp))
          (make-if (first-and-predicate (and-predicates exp)) (first-and-predicate (and-predicates exp)) 'false)
          (make-if (first-and-predicate (and-predicates exp)) 
                   (and->if (make-and (rest-and-predicates (and-predicates exp)))) 'false))))
(define (make-and preds)
  (cons 'and preds))

(define (or? exp)
  (tagged-list? exp 'or))
(define (or-predicates exp) (cdr exp))
(define (no-or-predicates? exp) (= 1 (length exp)))
(define (last-or-predicate? preds) (and (not (null? preds)) (= 1 (length preds))))
(define (first-or-predicate preds)
  (car preds))
(define (rest-or-predicates preds)
  (cdr preds))
(define (make-or preds)
  (cons 'or preds))
(define (or->if exp)
  (if (no-or-predicates? exp)
      'false
      (make-if (first-or-predicate  (or-predicates exp)) 
               (first-or-predicate (or-predicates exp))
               (or->if (make-or (rest-or-predicates (or-predicates exp)))))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (list-ref exp 1))
(define (let-body exp) (cddr exp))
(define (let->lambda exp)
  (make-application 
   (make-lambda (map car (let-bindings exp)) (let-body exp))
   (map cadr (let-bindings exp))))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list '< <)
        (list '>= >=)
        (list '<= <=)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'memq memq)
        (list 'list list)
        (list 'apply (lambda (proc params) (apply proc params)))
        ))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")
(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define (eval-in-environment)
  (ambeval
   '(begin
      (define (not p)
        (if p false true))
      (define (member x lst)
        (if (null? lst)
            false
            (if (equal? (car lst) x) true (member x (cdr lst)))))
      (define (distinct? items)
        (cond ((null? items) true)
              ((null? (cdr items)) true)
              ((member (car items) (cdr items)) false)
              (else (distinct? (cdr items)))))
      
      (define (require p)
        (if (not p) (amb)))
      
      (define (an-element-of items)
        (require (not (null? items)))
        (amb (car items) (an-element-of (cdr items))))
      
      (define (an-integer-between a b)
        (if (> a b)
            (amb)
            (amb a (an-integer-between (+ 1 a) b))))
      
      (define (an-integer-starting-from n)
        (amb n (an-integer-starting-from (+ n 1))))
      
      (define (a-pythagorean-triple-between low high)
        (let ((i (an-integer-between low high)))
          (let ((j (an-integer-between i high)))
            (let ((k (an-integer-between j high)))
              (require (= (+ (* i i) (* j j)) (* k k)))
              (list i j k)))))
      
      (define (all-pythagorean-triples)
        (let ((i (an-integer-starting-from 1)))
          (let ((j (an-integer-between i (* i i))))
            (let ((k (an-integer-between j (+ (* i i) (* j j)))))
              (require (= (+ (* i i) (* j j)) (* k k)))
              (list i j k)))))
      
      (define (multiple-dwelling)
        (let ((baker (amb 1 2 3 4 5))
              (cooper (amb 1 2 3 4 5))
              (fletcher (amb 1 2 3 4 5))
              (miller (amb 1 2 3 4 5))
              (smith (amb 1 2 3 4 5)))
          (require
           (distinct? (list baker cooper fletcher miller smith)))
          (require (not (= baker 5)))
          (require (not (= cooper 1)))
          (require (not (= fletcher 5)))
          (require (not (= fletcher 1)))
          (require (> miller cooper))
          (list (list (quote baker) baker)
                (list (quote cooper) cooper)
                (list (quote fletcher) fletcher)
                (list (quote miller) miller)
                (list (quote smith) smith))))
      
      (define (liars)
        (let ((betty (amb 1 2 3 4 5))
              (ethel (amb 1 2 3 4 5))
              (joan (amb 1 2 3 4 5))
              (kitty (amb 1 2 3 4 5))
              (mary (amb 1 2 3 4 5)))
          (require (distinct? (list betty ethel joan kitty mary)))
          (require (not (and (= kitty 2) (= betty 3))))
          (require (or (= kitty 2) (= betty 3)))
          (require (not (and (= ethel 1) (= joan 2))))
          (require (or (= ethel 1) (= joan 2)))
          (require (not (and (= joan 3) (= ethel 5))))
          (require (or (= joan 3) (= ethel 5)))
          (require (not (and (= kitty 2) (= mary 4))))
          (require (or (= kitty 2) (= mary 4)))
          (require (not (and (= mary 4) (= betty 1))))
          (require (or (= mary 4) (= betty 1)))
          (list (list (quote betty) betty)
                (list (quote ethel) ethel)
                (list (quote joan) joan)
                (list (quote kitty) kitty)
                (list (quote mary) mary))))
      
      (define nouns (quote (noun student professor cat class)))
      (define verbs (quote (verb studies lectures eats sleeps)))
      (define articles (quote (article the a)))
      
      (define (parse-sentence)
        (list (quote sentence)
              (parse-noun-phrase)
              (parse-word verbs)))
      
      (define (parse-noun-phrase)
        (list 'noun-phrase
              (parse-word articles)
              (parse-word nouns)))
      
      (define (parse-word word-list)
        (require (not (null? *unparsed*)))
        (require (memq (car *unparsed*) (cdr word-list)))
        (let ((found-word (car *unparsed*)))
          (set! *unparsed* (cdr *unparsed*))
          (list (car word-list) found-word)))
      
      (define *unparsed* (quote ()))
      (define (parse input)
        (set! *unparsed* input)
        (let ((sent (parse-sentence)))
          (require (null? *unparsed*))
          sent))
      
      (define prepositions (quote (prep for to in by with)))
      
      (define (parse-prepositional-phrase)
        (list (quote prep-phrase)
              (parse-word prepositions)
              (parse-noun-phrase)))
      
      (define (parse-sentence)
        (list 'sentence
              (parse-noun-phrase)
              (parse-verb-phrase)))
      (define (parse-verb-phrase)
        (define (maybe-extend verb-phrase)
          (amb verb-phrase
               (maybe-extend (list 'verb-phrase
                                   verb-phrase
                                   (parse-prepositional-phrase)))))
        (maybe-extend (parse-word verbs)))
      
      (define (parse-simple-noun-phrase)
        (list 'simple-noun-phrase
              (parse-word articles)
              (parse-word nouns)))
      (define (parse-noun-phrase)
        (define (maybe-extend noun-phrase)
          (amb noun-phrase
               (maybe-extend (list 'noun-phrase
                                   noun-phrase
                                   (parse-prepositional-phrase)))))
        (maybe-extend (parse-simple-noun-phrase)))
      
      (define (prime-sum-pair list1 list2)
        (let ((a (an-element-of list1))
              (b (an-element-of list2)))
          (require (prime? (+ a b)))
          (list a b)))
      (define (map f list)
        (if (null? list)
            (quote ())
            (cons (f (car list)) (map f (cdr list)))))
      (define (factorial n)
        (define (fact-iter n t)
          (if (= n 0)
              t
              (fact-iter (- n 1) (* n t))))
        (fact-iter n 1))
      
      (define (or-test)
        (let ((x (amb 1 2 3 4 5))
              (y (amb 1 2 3 4 5)))
          (require (or (= x 2) (= y 4)))
          (require (or (= x 3) (= y 5)))
          (list x y)))
      )
   the-global-environment (lambda (val fail) 'ok) (lambda () 'failed))
  (driver-loop))