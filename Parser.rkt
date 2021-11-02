;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Parser) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; ~~~~~ HELPER FUNCTIONS ~~~~~
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? (caar env) 'global) (resolve varName (list (cdar env))))
      ((eq? (caar env) 'local) (resolve varName (cons (cdar env) (cdr env))))
      ((eq? varName (caaar env)) (car (cdaar env)))
      (else (resolve varName (list (cdar env)))))))

(define build-scope
  (lambda (lo-vars lo-vals)
    (cond
      ((null? lo-vals) '())
      (else (cons (list (car lo-vars) (car lo-vals)) (build-scope (cdr lo-vars) (cdr lo-vals)))))))

(define extend-env
  (lambda (lo-vars lo-vals env)
    (cons (cons 'local (build-scope lo-vars lo-vals)) env)))

(define get-global-env
  (lambda (env)
    (cond
      ((null? env) '())
      ((eq? (caar env) 'global) env)
      (else (get-global-env (cdr env))))))

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


; ~~~~~~~~~~~~~~~~~~~~
; ~~~~~ TOASTERS ~~~~~
; ~~~~~~~~~~~~~~~~~~~~

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

; ~~~~~~~~~~~~~~~~~~~~
; ~~~~~ PARSERS ~~~~~~
; ~~~~~~~~~~~~~~~~~~~~

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

; ~~~~~~~~~~~~~~~~~~~~~~~~
; ~~~~~ INTERPRETERS ~~~~~
; ~~~~~~~~~~~~~~~~~~~~~~~~

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
    (run-parsed-code (cadr (caddr parsed-no-code-function)) env)))

           
(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) env ))
      ((eq? (car parsed-no-code) 'let-exp)
       (let* ((var-2-lists (env-let-mapper (cadr parsed-no-code)))
              (new-env (extend-env (car var-2-lists) (cadr var-2-lists) env))
              (body (caddr parsed-no-code)))
         (run-parsed-code body new-env))) 
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'ask-exp)
       (if (run-parsed-boolean-code (cadr parsed-no-code) env)  
           (run-parsed-code (caddr parsed-no-code) env)
           (run-parsed-code (cadddr parsed-no-code) env)))
      (else
         (run-parsed-function-code
        (cadr parsed-no-code)
        (extend-env
         (cdr (cadr (cadr parsed-no-code)))
         (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x env)) (caddr parsed-no-code)))
         (get-global-env (list env))))))))   
 ; this is where you burn off until you get to the most local scope


; ~~~~~~~~~~~~~~~~~
; ~~~~~ MAIN ~~~~~~
; ~~~~~~~~~~~~~~~~~

(define env '(global (age 21) (a 7) (b 5) (c 23)))
(define extended-env (extend-env '(a v) '(5 12) (get-global-env (list env))))
extended-env
;(resolve 'a (list env))
(define sample-no-code '(call (function (a) (call (function (r) a) a)) 5))
;sample-no-code
(define parsed-no-code (no-parser sample-no-code))
parsed-no-code
(run-parsed-code parsed-no-code env)