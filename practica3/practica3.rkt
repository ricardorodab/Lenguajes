#lang plai

(require "practica3-base.rkt")

(define (zones rest max )
  (local
    ([define range (- max rest)])
    (list
     (resting rest (+ rest(- (* range 0.5) 1)))
     (warm-up (+ rest (* range (+ 0.5 (* 0.1 0)))) (+ rest (- (* range (+ 0.5 (* 0.1 1))) 1)))
     (fat-burning (+ rest (* range (+ 0.5 (* 0.1 1)))) (+ rest (- (* range (+ 0.5 (* 0.1 2))) 1)))
     (aerobic (+ rest (* range (+ 0.5 (* 0.1 2)))) (+ rest (- (* range (+ 0.5 (* 0.1 3))) 1)))
     (anaerobic (+ rest (* range (+ 0.5 (* 0.1 3)))) (+ rest (- (* range (+ 0.5 (* 0.1 4))) 1)))
     (maximum (+ rest (* range (+ 0.5 (* 0.1 4)))) (+ rest  (* range (+ 0.5 (* 0.1 5)))))
    )))

(define my-zones (zones 50 180))


(define (get-zone symbol zone)
  (cond
   [(empty? zone ) empty]
   [(and (resting? (car zone)) (equal? symbol 'resting)) (car zone) ]
   [(and (warm-up? (car zone)) (equal? symbol 'warm-up)) (car zone) ]
   [(and (fat-burning? (car zone)) (equal? symbol 'fat-burning)) (car zone) ]
   [(and (aerobic? (car zone)) (equal? symbol 'aerobic)) (car zone) ]
   [(and (anaerobic? (car zone)) (equal? symbol 'anaerobic)) (car zone) ]
   [(and (maximum? (car zone)) (equal? symbol 'maximum)) (car zone) ]
   [else (get-zone symbol (cdr zone))]))



(define (bpm->zone fc z)
  (if (empty? fc)
      empty
      (letrec ([busca (lambda (n zone)
                        (if (empty? zone)
                            empty
                            (type-case HRZ (car zone) 
                              [resting (low high) (if (and (<= low n) (<= n high)) (car zone) (busca n (cdr zone)))]
                              [warm-up (low high) (if (and (<= low n) (<= n high)) (car zone) (busca n (cdr zone)))]
                              [fat-burning (low high) (if (and (<= low n) (<= n high)) (car zone) (busca n (cdr zone)))]
                              [aerobic (low high) (if (and (<= low n) (<= n high)) (car zone) (busca n (cdr zone)))]
                              [anaerobic (low high) (if (and (<= low n) (<= n high)) (car zone) (busca n (cdr zone)))]
                              [maximum (low high) (if (and (<= low n) (<= n high)) (car zone) (busca n (cdr zone)))]
                              )))])
        (cons (busca (car fc) z) (bpm->zone (cdr fc) z)))))

(define (create-trackpoints l zones)
  (if (empty? l)
      empty
       (cons
        (trackpoint (GPS (first (second (car l))) (second (second (car l)))) (third (car l)) (first (bpm->zone (list (third (car l))) zones)) (first (car l)))
        (create-trackpoints (cdr l) zones) )
      
      ))

(create-trackpoints (take raw-data 4) my-zones)


(define (ninBT arbol)
  (type-case BTree  arbol
    [EmptyBT () 0]
    [BNode  (c l e r) 
           (if(and (EmptyBT? l) (EmptyBT? r))
              0
              (+ (+ 1 (ninBT l))(ninBT r)))]))

(define (nlBT arbol)
  (type-case BTree arbol
    [EmptyBT () 0]
    [BNode (c l e r)
           (if(or (EmptyBT? l)(EmptyBT? r))
              1
              (+(nlBT l) (nlBT r)))]))


(define (nnBT arbol)
  (type-case BTree arbol
    [EmptyBT () 0]
    [BNode (c l e r)
           (if(and (EmptyBT? l)(EmptyBT? r))
              1
              (+ (nnBT l) (+ 1 (nnBT r))))]))

(define (mapBT f arb)
  (type-case BTree arb
    [EmptyBT () ebt]
    [BNode (c l e r)
           (if (EmptyBT? l)
               (bnn ebt (f e) (mapBT f r))
               (if(EmptyBT? r)
                  (bnn (mapBT f l) (f e) ebt)
                  (bnn (mapBT f l) (f e) (mapBT f r))))]))





(define (preorderBT arbol )
  (type-case BTree arbol
    [EmptyBT () empty]
    [BNode (c l e r)  
           (flatten (list e (preorderBT l) (preorderBT r)))]) )


(define (inorderBT arbol )
  (type-case BTree arbol
    [EmptyBT () empty]
    [BNode (c l e r)  
           (flatten (list  (inorderBT l)  e (inorderBT r)))]) )


(define (posorderBT arbol )
  (type-case BTree arbol
    [EmptyBT () empty]
    [BNode (c l e r)  
           (flatten (list  (posorderBT l) (posorderBT r) e))]) )
