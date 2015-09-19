#lang plai

(require "practica3-base.rkt")

; Sección I Heart Rate Zones 
;
; Función zones
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


; Función get-zone 
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


; Función bpm->zone 
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

; Función create-trackpoints 
(define (create-trackpoints l zones)
  (if (empty? l)
      empty
       (cons
        (trackpoint (GPS (first (second (car l))) (second (second (car l)))) (third (car l)) (first (bpm->zone (list (third (car l))) zones)) (first (car l)))
        (create-trackpoints (cdr l) zones) )))


; Función  total-distance
(define (total-distance  lt )
  (if (empty? (cdr lt))
      0
      (+ (haversine (dame-gps (first lt)) (dame-gps (second lt))) (total-distance (cdr lt)) )  ))

; Función average-hr  
(define (average-hr lt)
  (letrec ([average-aux (lambda (l suma long)
                          (if (empty? l)
                              (round (/ suma long))
                              (average-aux (cdr l) (+ (dame-hr (car l)) suma) (add1 long ))))])
    (average-aux lt 0 0)))

; Función max-hr 
(define (max-hr lt )
  (letrec ([max-hr-aux (lambda (l current)
                        (if (empty? l)
                              current
                              (max-hr-aux (cdr l) (if (> (dame-hr (car l)) current) (dame-hr (car l)) current) )))])
  (max-hr-aux lt 0 )))

; Sección II  Árboles Binarios
;
; Funcion ninBT 
(define (ninBT arbol)
  (type-case BTree  arbol
    [EmptyBT () 0]
    [BNode  (c l e r) 
           (if(and (EmptyBT? l) (EmptyBT? r))
              0
              (+ (+ 1 (ninBT l))(ninBT r)))]))

; Funcion nlBT 
(define (nlBT arbol)
  (type-case BTree arbol
    [EmptyBT () 0]
    [BNode (c l e r)
           (if(or (EmptyBT? l)(EmptyBT? r))
              1
              (+(nlBT l) (nlBT r)))]))


; Funcion nnBT
(define (nnBT arbol)
  (type-case BTree arbol
    [EmptyBT () 0]
    [BNode (c l e r)
           (if(and (EmptyBT? l)(EmptyBT? r))
              1
              (+ (nnBT l) (+ 1 (nnBT r))))]))

; Funcion mapBT
(define (mapBT f arb)
  (type-case BTree arb
    [EmptyBT () ebt]
    [BNode (c l e r)
           (if (EmptyBT? l)
               (bnn ebt (f e) (mapBT f r))
               (if(EmptyBT? r)
                  (bnn (mapBT f l) (f e) ebt)
                  (bnn (mapBT f l) (f e) (mapBT f r))))]))



; Funcion preorderBT
(define (preorderBT arbol )
  (type-case BTree arbol
    [EmptyBT () empty]
    [BNode (c l e r)  
           (flatten (list e (preorderBT l) (preorderBT r)))]) )

; Funcion inorderBT
(define (inorderBT arbol )
  (type-case BTree arbol
    [EmptyBT () empty]
    [BNode (c l e r)  
           (flatten (list  (inorderBT l)  e (inorderBT r)))]) )

; Funcion posorderBT
(define (posorderBT arbol )
  (type-case BTree arbol
    [EmptyBT () empty]
    [BNode (c l e r)  
           (flatten (list  (posorderBT l) (posorderBT r) e))]) )

; Funcion haversine para obtener la distancia entre dos coordenadas
(define (haversine gps1 gps2)
  (let*  [(radio-tierra 6371)
          (lat (degrees->radians (- (dameLatitud gps2 ) (dameLatitud gps1))))
          (long (degrees->radians (- (dameLongitud gps2) (dameLongitud gps1 ))))
          (a  (+ (sqr(sin(/ lat 2))) (* (cos (degrees->radians (dameLatitud gps1))) (* (cos (degrees->radians (dameLatitud gps2))) (sqr (sin (/ long 2)))))))
          (c (* 2 (asin (sqrt a))))
          (d (* radio-tierra c))]
    d))

; Función que nos da la latitud de una coordenada
(define (dameLatitud gps)
  (type-case Coordinate gps
    [GPS (lat long) lat]))

; Función que nos da la longitud de una coordenada
(define (dameLongitud gps)
  (type-case Coordinate gps
    [GPS (lat long) long]))

; Función que nos da el GPS de un trackpoint
(define (dame-gps f)
  (type-case Frame f
    [trackpoint (loc hr zone unix-time) loc]))

; Función que nos da el ritmo cardiaco de un trackpoint
(define (dame-hr f)
  (type-case Frame f
    [trackpoint (loc hr zone unix-time) hr]))


; Variables para pruebas
(define my-zones (zones 50 180))
(define sample (create-trackpoints (take raw-data 100) my-zones))
(define trackpoints (create-trackpoints raw-data my-zones))
(define sample-four (create-trackpoints (take raw-data 4) my-zones))