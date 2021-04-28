#lang eopl

;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env))) 

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym)))))))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))



(define lexic
 '((space  (whitespace) skip)
  (comment ("//" (arbno (not #\newline)) ) skip)
  (id ("@" letter (arbno (or letter digit "?"))) symbol)
  (text ("$" letter (arbno (not #\$)) "$") string)
  (numericVal (digit (arbno digit)) number)
  (numericVal (digit (arbno digit) "." digit (arbno digit)) number)
  (numericVal ("-" digit (arbno digit)) number)
  (numericVal ("-" digit (arbno digit) "." digit (arbno digit) ) number))
  )

(define just-scan
  (sllgen:make-string-scanner lexic'()))


(define gramatic
  '(
    (program (expression) a-program)
    (expression (numericVal) num)
    (expression (id) var)
    (expression (text) string-exp)
    (expression (primitive "(" (separated-list expression ",") ")") computation)
    (primitive ("+") sumPrim)
    (primitive ("-") subPrim)
    (primitive ("*") prodPrim)
    (primitive ("/") divPrim)
    (primitive ("concat") concatPrim)
    (primitive ("length") lengthPrim)



    )
 )

(sllgen:make-define-datatypes lexic gramatic)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexic gramatic)))

(define scan&parse
  (sllgen:make-string-parser lexic gramatic))


(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      lexic
      gramatic)))

(define MEMORY
  (extend-env '(@x @y @z) '(5 6 7) (empty-env)))

(define eval-program
  (lambda (p)
    (cases program p
      (a-program (e) (eval-expression e MEMORY))
      )))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (num (n) n)
      (string-exp (t) t)
      (var (v) (apply-env env v))
      (computation (prim lexp) 
          (let
              (
               (evaluatedL (map (lambda (e) (eval-expression e env)) lexp))
               )
            (eval-prim prim evaluatedL)
           ))

        )
    )
  )


(define eval-prim
  (lambda (prim lValues)
    (cases primitive prim
    (sumPrim () (add lValues))
    (concatPrim () (concatenate lValues))
    (else #f)

    ))
  )

(define add
  (lambda (l)
    (if (null? l) 0
        (+ (car l) (add (cdr l))))))

(define concatenate
  (lambda (cl)
    (if (null? cl) ""
        (string-append (car cl) (concatenate (cdr cl)))))
 )



