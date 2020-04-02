#lang racket

(provide (all-defined-out))

(define (lambda? x)

  (if (or (equal? x 'lambda) (equal? x 'λ)) #t #f))
  

(define (create-binding x y)
	(string->symbol (string-append (symbol->string x) "!" (symbol->string y)))
)
 
(define (expr-compare x y)
  (cond [(equal? x y) x]
        [(and (boolean? x) (boolean? y)) 
         (if x '% '(not %))]
        ; if one of them is not list - which means that not function
        [(or (not (list? x)) 
             (not (list? y)))
         (list 'if '% x y)]
        ;from now on both are lists and are different

        ;quotes
        [(and (equal? (car x) 'quote) (equal? (car y) 'quote))
         (list 'if '% x y)]

        ;lambda function (not lambda function on something)
        [(and (lambda? (car x)) (lambda? (car y)) ) (lhelper x y)]
        ;if statement
        [(and (equal? (car x) 'if) (equal? (car y) 'if)) (ihelper x y)]
        ;totally different types
        [ (or (equal? (car x) 'quote) (equal? (car y) 'quote) (lambda? (car x)) (lambda? (car y)) (equal? (car x) 'if) (equal? (car y) 'if))
          (list 'if '% x y)]
        ;both procedure
        [else (phelper x y)]

   )
)


(define (naive-expr-compare x y)
  (cond [(equal? x y) x]
        [(and (boolean? x) (boolean? y)) 
         (if x '% '(not %))]
        ; if one of them is not list - which means that not function
        [(or (not (list? x)) 
             (not (list? y)))
         (list 'if '% x y)]
        ;from now on both are lists and are different

        ;quotes
        [(and (equal? (car x) 'quote) (equal? (car y) 'quote))
         (list 'if '% x y)]

        ;lambda function (not lambda function on something)
        [(and (lambda? (car x)) (lambda? (car y)) ) (lhelper x y)]
        ;if statement
        [(and (equal? (car x) 'if) (equal? (car y) 'if)) (ihelper x y)]

        ;both procedure
        [else (naive-phelper x y)]

   )
)

(define (naive-phelper x y)
        
  (cond [(not(equal? (length x) (length y)))
         (list 'if '% x y)
         ]
        ;parameter number is same
        [else (cons (naive-expr-compare (car x) (car y)) (naive-expr-compare (cdr x) (cdr y)))]

  )

)

;both are procedure 
(define (phelper x y)
        
  (cond [(not(equal? (length x) (length y)))
         (list 'if '% x y)
         ]
        ;parameter number is same
        [else (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]

  )

)
;idead: lambda functions are things fall into procedures calls first
;and the car comes to the lambda helper
;both are lambda function
(define (lhelper x y)
  ;deal with if lambda arguments number are different
  (cond
    [(or (equal? (cdr x) '()) (equal? (cdr y) '())) (list(list 'if '% (car x) (car y)))]
    [(equal? (length (cadr x)) (length (cadr y)))
      (cons (lsymbol (car x) (car y)) (lhelper2 (cdr x)(cdr y)(cadr x) (cadr y) '() (hash) (hash)))]  ; (hash) or '(hash)
        
      [else (list 'if '% x y)]
   )
      
)
;deals with the list of things after lambda in the function
;this function keeps all the renaming in the hashmap
;z w is the reduced version of the first part of x and y
;g  will keep upgrading to its new version (of first part of x and y)
(define (lhelper2 x y z w g  hmapx hmapy)
        ;list are both empty
  (cond [(and (equal? z '() ) (equal?  w'()))
         (list g (naive-expr-compare (lhelper3 (cadr x) hmapx)(lhelper3 (cadr y) hmapy))  ) ] ;change this part
        
        ;;;taking care of deep rambda 

          ;first in the list is equal
        [(equal? (car z) (car w))
         (lhelper2 x y (cdr z) (cdr w) (append g (list (car z))) hmapx hmapy)]
        ;first in the list not equal
        [else (lhelper2 x y (cdr z) (cdr w)
         (append g (list (create-binding (car z) (car w))) )
         (udmap1 hmapx (car z) (car w)) (udmap2 hmapy (car z) (car w)))]
                                                 
))


;this two functions returns a new map
(define (udmap1 hmap x y)
  (define hmap1 (hash-set hmap x (create-binding x y)))
  hmap1
 )
(define (udmap2 hmap x y)
  (define hmap1 (hash-set hmap y (create-binding x y)))
  hmap1
 )
  
;this function takes cares of changing the name afterwards
;x is the second part 
(define (lhelper3 x hmap )
  (cond [(not (list? x))
          (let ([temp (hash-ref hmap x "Not found")])
           (if (equal? "Not found" temp) x temp))]
        [(equal? x '()) '()]
        [(list? (car x))
         (if (lambda? (caar x))
             (cons (car x) (lhelper3 (cdr x) hmap))
             (cons (lhelper3 (car x) hmap) (lhelper3 (cdr x) hmap)))]
        [else (let ([temp (hash-ref hmap (car x) "Not found")])
          (if (equal? "Not found" temp) (cons (car x) (lhelper3 (cdr x) hmap)) (cons temp (lhelper3 (cdr x) hmap))))]

))


;get the right lambda symbol
(define (lsymbol x y)
  (if (equal? x y) x 'λ
      ))

;both are if
(define (ihelper x y) (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y))))

;test
(define (test-expr-compare x y) 
  (and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y))))
       (equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y))))))

(define test-expr-x
  ''(
    (+ 1 2)
    (quote (1 2))
    (lambda (x) (- x x) 10)
    (if #t 1 2)
  )
)

#| test-expr-y |#
(define test-expr-y
  ''((* 1 2)
    (quote (* 1 2))
    (lambda (x y) (+ x y) 10)
    (if #f 1 2)
    ''(1 2)
  )
)
