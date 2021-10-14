;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Parser) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;***** Environment Stuff *****
(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? varName (caar env)) (cadar env))
      (else (resolve varName (cdr env))))))

(define extend-env
  (lambda (lo-vars lo-vals env)
    (cond
      ((null? lo-vars) env)
      (else (extend-env (cdr lo-vars)
                        (cdr lo-vals)
                        (cons (list (car lo-vars) (car lo-vals)) env))))))

(define env-let-get-var-names
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (caar lst) (env-let-get-var-names (cdr lst))))))

(define env-let-get-var-values
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cadar lst) (env-let-get-var-values (cdr lst))))))

(define env-let-mapper
  (lambda (lst)
    (list (env-let-get-var-names lst)
          (env-let-get-var-values lst))))
                              


(define do-mathy-stuff-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '+) (+ num1 num2))
      ((eq? op '-) (- num1 num2))
      ((eq? op '/) (/ num1 num2))
      ((eq? op '//) (quotient num1 num2))
      ((eq? op '%) (modulo num1 num2))
      ((eq? op '*) (* num1 num2))
      (else #f))))

; ***** PARSERS *****
(define boolean-expression-parser
  (lambda (boolean-expression)
    (cond
      ((eq? (car boolean-expression) '<)
       (list 'less-then
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '<=)
       (list 'less-then-or-equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '>)
       (list 'greater-then
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '>=)
       (list 'greater-then-or-equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '==)
       (list 'equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '!=)
       (list 'not-equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      (else "Not a valid boolean expression"))))

(define no-code-function-parser
  (lambda (no-code-function)
    (list 'func-exp
             (append (list 'params) (cadr no-code-function))
             (list 'body
                   (no-parser (caddr no-code-function))))))
(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'local-vars)
       (list 'let-exp
             (cadr no-code)
             (no-parser (caddr no-code))))
      ((eq? (car no-code) 'ask)
       (list 'ask-exp
             (boolean-expression-parser (cadr no-code))
             (no-parser (caddr no-code))
             (no-parser (car (reverse no-code)))))
      (else (list 'call-exp
                  (no-code-function-parser (cadr no-code))
                  (map no-parser (cddr no-code)))))))

; ***** Interpreters *****
(define run-parsed-boolean-code
  (lambda (parsed-boolean-code env)
    (cond
      ((eq? (car parsed-boolean-code) 'less-then)
       (<
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'less-then-or-equal)
       (<=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'greater-then)
       (>
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'greater-then-or-equal)
       (>=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'equal)
       (=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'not-equal)
       (not (=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env))))
      (else "Not a legal boolean expression!"))))
       
        
(define run-parsed-function-code
  (lambda (parsed-no-code-function env)
    (run-parsed-code (cadr (caddr parsed-no-code-function)) (list (car env)))))

           
(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) (list (car env))))
      ((eq? (car parsed-no-code) 'let-exp)
       (let* ((var-2-lists (env-let-mapper (cadr parsed-no-code)))
              (new-env (extend-env (car var-2-lists) (cadr var-2-lists) (list (car env))))
              (body (caddr parsed-no-code)))))
         (run-parsed-code body new-env))) 
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) (list (car env)))
        (run-parsed-code (cadddr parsed-no-code) (list (car env)))))
      ((eq? (car parsed-no-code) 'ask-exp)
       (if (run-parsed-boolean-code (cadr parsed-no-code) (list (car env)))
           (run-parsed-code (caddr parsed-no-code) (list (car env)))
           (run-parsed-code (cadddr parsed-no-code) (list (car env)))))
      (else
       (cond
         ((> (length env) 1)
          (run-parsed-function-code
           (cadr parsed-no-code)
           (cdr env)))
         (else (run-parsed-function-code
        (cadr parsed-no-code)
        (list (extend-env
         (cdr (cadr (cadr parsed-no-code)))
         (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x (car env))) (caddr parsed-no-code)))
         (car env))))))))

(define env '((age 21) (a 7) (b 5) (c 23)))
(eq? (length (list (extend-env '(a) '(5) env) env)) 2)
(define sample-no-code '(call (function (a) (call (function (r) a) 10)) 5))
(define parsed-no-code (no-parser sample-no-code))
(run-parsed-code parsed-no-code (list env))