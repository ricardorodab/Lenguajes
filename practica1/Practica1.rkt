#lang plai

; Ejercicio pow.
; Función que nos da la potencia de un número x a la y. Multiplicando x hasta que y sea cero.

(define (pow x y)
  (cond [(zero? y) 1]
        [else (* x (pow x (- y 1)))]))

; Ejercicio average.
; Función que me da el promedio de la lista dividiendo la suma de sus elementos entre su longitud.

(define (average lst)
  (/ (suma-lista lst) (my-length lst)))

; Ejercicio primes.
; Función que nos da los n primeros números primos.

(define (primes n)
  (cond [(zero? n) empty]
        [(= 1 n) empty]
        [(divisor n (- n 1)) (mconcat (primes (- n 1)) (cons n '()))]
        [else (primes (- n 1))]))

; Ejercicio reduce
; Función que aplica una función binaria a n listas.

(define (reduce f l)
  (cond
    [(empty? (cddr l)) (f (car l) (cadr l))]
    [else  (f (car l) (reduce f (cdr l)))]))

; Ejercicio zip
; Función que dada dos listas, regresa una lista con listas de tamaño dos.

(define zip
  (lambda (l1 l2 )
    (cond
      [(empty? l1) '()]
      [(empty? l2) '()]
      [else(cons (list (car l1) (car l2)) (zip (cdr l1) (cdr l2)) )])))

; Ejercicio mconcat
; Función que nos genera una sola lista dada dos listas (concat).

(define (mconcat l1 l2)
  (cond
    [(eq? l1 empty) l2]
    [(eq? l2 empty) l1]
    [else  (cons (car l1) (mconcat (cdr l1) l2))]))

; Ejercicio mmap

(define (mmap f l)
  (cond
    ((eq? empty l) empty)
    (else  (append (list (f (first l))) (mmap f (rest l))))))

; Ejercicio filter

(define (mfilter f lst)
  (cond
    [(empty? lst) empty]
    [(f (car lst)) (mconcat (cons (car lst) '()) (mfilter f (cdr lst)))]
    [else (mfilter f (cdr lst))]))

;Ejercicio any?

(define (any? f element)
  (cond [(empty? element) #f]
        [(f (car element)) #t]
        [else (any? f (cdr element))]))

;Ejercicio every?

(define (every? f lst)
  (cond [(empty? lst) #t]
        [(f (car lst)) (every? f (cdr lst))]
        [else #f]))

;Ejercicio mpowerset

(define (mpowerset lst)
  (if (empty? lst) (list empty)
      (let ([aux (mpowerset (cdr lst))])
        (append (mmap (lambda(l) 
                       (cons (car lst) l)) 
                     aux) aux))))


;FUNCIONES AUXILIARES: 
;Función auxiliar para generar la cantidad de elementos que tiene una lista sumando uno por uno sus elementos.

(define (suma-lista lst)
  (cond [(empty? lst) 0]
        [else (+ (car lst) (suma-lista (cdr lst)))]))

;Función auxiliar para generar la longitud de una lista sumando 1 por cada elemento que vea la lista.

(define (my-length lst)
  (cond [(empty? lst) 0]
        [else (+ 1 (my-length (cdr lst)))]))

;Función auxiliar que nos dice si un número n es un primo.

(define (divisor a b)
     (cond [(= 1 b) #t]
        [(integer? (/ a b)) #f]
        [else (divisor a (- b 1))]))



; Pruebas

; Función pow

(test (pow 130 3)  2197000)
(test (pow 23 2)  529)
(test (pow 15 3)  3375)
(test (pow 2 5)  32)
(test (pow 200 0)  1)


; Función average

(test (average '(5))  5)
(test (average '(1 2 3 4 5))  3)
(test (average '(5 9 1)) 5 )
(test (average '( 0 0 0))  0)
(test (average '(5 5 5 5 5))  5)

; Función primes

(test (primes 30)  '(2 3 5 7 11 13 17 19 23 29))
(test (primes 2)  '(2))
(test (primes 1)  '())
(test (primes 100)  '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))
(test (primes 24)  '(2 3 5 7 11 13 17 19 23))

; Función zip

(test (zip '() '())  '() )
(test (zip '(1 ) '())  '() )
(test (zip '() '(1))  '() )
(test (zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6)))
(test (zip '(1 3) '(2 4)) '((1 2) (3 4)) )


; Función reduce

(test (reduce + '(1 2 3 4 5 6 7 8 9 10)) 55)
(test (reduce pow '(2 2 2)) 16) 
(test (reduce zip '((1 2 3) (4 5 6) (7 8 9)) ) '((1 (4 7)) (2 (5 8)) (3 (6 9))) )
(test (reduce * '(2 2)) 4)
(test (reduce mconcat '((1 2 3) (4 5 6)) ) '(1 2 3 4 5 6) )



; Función mconcat

(test (mconcat '() '()) '() )
(test (mconcat '() '(1)) '(1) )
(test (mconcat '(1) '()) '(1) )
(test (mconcat '(1 3 5) '(2 4 6)) '(1 3 5 2 4 6) )
(test (mconcat '(1 ) '(1)) '(1 1) )


; Función mmap

(test (mmap  primes '(1 2 3 4 5) ) '(() (2) (2 3) (2 3) (2 3 5)))
(test (mmap  average '((1) ( 1 2 3 4 5)) ) '(1 3))
(test (mmap  mpowerset '( () (1 2)) ) '((()) ((1 2) (1) (2) ())) )
(test (mmap  first '((1 2 3) (2 5 8)) ) '(1 2))
(test (mmap  add1 '(1 2 3 4 5) ) '(2 3 4 5 6) )


; Función mfilter

(test (mfilter (lambda (x) (not (zero? x))) '(2 0 1 4 0)) '(2 1 4))
(test (mfilter (lambda (l) (not (empty? l))) '((1 4 2) () (2 4) ())) '((1 4 2) (2 4)))
(test (mfilter (lambda (n) (= (modulo n 2) 0)) '(1 2 3 4 5 6)) '(2 4 6))
(test (mfilter (lambda (n) (not (= (modulo n 2) 0))) '(1 2 3 4 5 6)) '(1 3 5))
(test (mfilter (lambda (n) (list? n)) '(1 '() (2 3) 4 5 6)) '('() (2 3)))

; Función any?
(test (any? number? '()) #f)
(test (any? number? '(a b c d 1)) #t)
(test (any? symbol? '(1 2 3 4)) #f)
(test (any? (lambda (n) (list? n))'(1 '() (2 3) 4 5 6)) #t)
(test (any? (lambda (x) (not (zero? x))) '(0 0 0 0)) #f)

; Función every?
(test (every? number? '()) #t)
(test (every? number? '(1 2 3 4)) #t)
(test (every? symbol? '(1 2 d 3 4)) #f)
(test (every? (lambda (n) (list? n))'(1 '() (2 3) 4 5 6)) #f)
(test (every? (lambda (x) (not (zero? x))) '(0 0 0 0)) #f)

; Función mpowerset

(test (mpowerset '()) '(()))
(test (mpowerset '(1)) '((1) ()))
(test (mpowerset '(1 2 )) '((1 2) (1) (2) ()))
(test (mpowerset '(1 2 3 4)) '((1 2 3 4) (1 2 3) (1 2 4) (1 2) (1 3 4) (1 3) (1 4) (1) (2 3 4) (2 3) (2 4) (2) (3 4) (3) (4) ()))
(test (mpowerset '(1 2 3 5)) '((1 2 3 5) (1 2 3) (1 2 5) (1 2) (1 3 5) (1 3) (1 5) (1) (2 3 5) (2 3) (2 5) (2) (3 5) (3) (5) ()))