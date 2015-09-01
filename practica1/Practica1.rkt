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
