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


; Función collapse-trackpoints
(define (collapse-trackpoints lst e)
  (define (collapse pas lst trackp e)
    (cond
      [(empty? lst) (append (list trackp) pas)]
      [(if (and (< (haversine (trackpoint-loc (car lst)) (trackpoint-loc trackp)) e) (eq? (trackpoint-hr (car lst)) (trackpoint-hr trackp)))
            (append pas lst)
            (collapse (append pas (list (car lst))) (cdr lst) trackp e))]))
  (cond
    [(empty? lst) empty]
    [else (collapse '() (collapse-trackpoints (cdr lst) e) (car lst) e)]))

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

; PRUEBAS


; Test zones

(test (zones 50 180) (list (resting 50 114.0) (warm-up 115.0 127.0) (fat-burning 128.0 140.0) (aerobic 141.0 153.0) (anaerobic 154.0 166.0) (maximum 167.0 180.0)))
(test (zones 60 160) (list (resting 60 109.0) (warm-up 110.0 119.0) (fat-burning 120.0 129.0) (aerobic 130.0 139.0) (anaerobic 140.0 149.0) (maximum 150.0 160.0)))
(test (zones 50 170) (list (resting 50 109.0) (warm-up 110.0 121.0) (fat-burning 122.0 133.0) (aerobic 134.0 145.0) (anaerobic 146.0 157.0) (maximum 158.0 170.0)))
(test (zones 40 200) (list (resting 40 119.0) (warm-up 120.0 135.0) (fat-burning 136.0 151.0) (aerobic 152.0 167.0) (anaerobic 168.0 183.0) (maximum 184.0 200.0)))
(test (zones 60 130) (list (resting 60 94.0) (warm-up 95.0 101.0) (fat-burning 102.0 108.0) (aerobic 109.0 115.0) (anaerobic 116.0 122.0) (maximum 123.0 130.0)))

; Test get-zone

(test (get-zone 'resting my-zones) (resting 50 114.0))
(test (get-zone 'warm-up my-zones) (warm-up 115.0 127.0))
(test (get-zone 'fat-burning my-zones) (fat-burning 128.0 140.0))
(test (get-zone 'maximum my-zones) (maximum 167.0 180.0))
(test (get-zone 'anaerobic my-zones) (anaerobic 154.0 166.0))

; Test bpm -> zone

(test (bpm->zone empty my-zones) '())
(test (bpm->zone '(180) my-zones) (list (maximum 167.0 180.0)))
(test (bpm->zone '(50 60) my-zones) (list (resting 50 114.0) (resting 50 114.0)))
(test (bpm->zone '(140 141) my-zones) (list (fat-burning 128.0 140.0) (aerobic 141.0 153.0)))
(test (bpm->zone '(50 60 70 80) my-zones) (list (resting 50 114.0) (resting 50 114.0) (resting 50 114.0) (resting 50 114.0)))

; Test create-trackpoints

(test (create-trackpoints (take raw-data 0) my-zones) '())
(test (create-trackpoints (take raw-data 1) my-zones)
      (list (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)))
(test (create-trackpoints (take raw-data 2) my-zones)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)))
(test (create-trackpoints (take raw-data 4) my-zones) (list
                                                       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)
                                                       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
                                                       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
                                                       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)))
(test (create-trackpoints (take raw-data 5) my-zones)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)
       (trackpoint (GPS 19.4907086 -99.2411981) 111 (resting 50 114.0) 1425619671)))

; Test total-distance

(test (total-distance trackpoints) 5.0551083734469096)
(test (total-distance (create-trackpoints (take raw-data 100) my-zones)) 0.9509291243812747)
(test (total-distance (create-trackpoints raw-data my-zones)) 5.051934549322941)
(test (total-distance (create-trackpoints (take raw-data 10) my-zones)) 0.057104023456293194)
(test (total-distance (create-trackpoints (take raw-data 1) my-zones)) 0)

; Test average-hr

(test (average-hr sample) 134)
(test (average-hr trackpoints) 150)
(test (average-hr (create-trackpoints (take raw-data 10) my-zones)) 111)
(test (average-hr (create-trackpoints (take raw-data 50) my-zones)) 128)
(test (average-hr (create-trackpoints (take raw-data 70) my-zones)) 131)

; Test max-hr

(test (max-hr sample) 147)
(test (max-hr trackpoints) 165)
(test (max-hr (create-trackpoints (take raw-data 10) my-zones)) 120)
(test (max-hr (create-trackpoints (take raw-data 50) my-zones)) 136)
(test (max-hr (create-trackpoints (take raw-data 70) my-zones)) 142)

; Test collapse-trackpoints

(test (collapse-trackpoints sample-four 0.01)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)))
(test (collapse-trackpoints (create-trackpoints (take raw-data 2) my-zones) 0.7)
      (list (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)))
(test (collapse-trackpoints '() 0) '())
(test (collapse-trackpoints (create-trackpoints (take raw-data 3) my-zones) 0.21)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)))
(test (collapse-trackpoints (create-trackpoints (take raw-data 5) my-zones) 1.1)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)
       (trackpoint (GPS 19.4907086 -99.2411981) 111 (resting 50 114.0) 1425619671)))

; Test ninBT

(test (ninBT (EmptyBT)) 0)
(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)
(test (ninBT arb2) 1)
(test (ninBT arb3) 3)
(test (ninBT arb4) 7)

; Test  nlBT

(test (nlBT (EmptyBT)) 0)
(test (nlBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 2)
(test (nlBT arb1) 1)
(test (nlBT arb3) 4)
(test (nlBT arb4) 8)

; Test nnBT

(test (nnBT (EmptyBT)) 0)
(test (nnBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 3)
(test (nnBT arb1) 1)
(test (nnBT arb4) 15)
(test (nnBT arb3) 7)

; Test mapBT

(test (mapBT add1 (EmptyBT)) (EmptyBT))
(test (mapBT add1 (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 3 (EmptyBT))))
(test (mapBT (lambda (x) (* x x)) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 9 (BNode < (EmptyBT) 4 (EmptyBT))))
(test (mapBT (lambda (x) (* x 2)) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 6 (BNode < (EmptyBT) 4 (EmptyBT))))
(test (mapBT sqrt (BNode < (BNode < (EmptyBT) 4 (EmptyBT)) 9 (BNode < (EmptyBT) 16 (EmptyBT)))) (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 3 (BNode < (EmptyBT) 4 (EmptyBT))))

; Test preorderBT

(test (preorderBT arbol-base) '("F" "B" "A" "D" "C" "E" "G" "I" "H"))
(test (preorderBT arb1) '(1))
(test (preorderBT arb2) '(2 1 1))
(test (preorderBT arb3) '(3 2 1 1 2 1 1))
(test (preorderBT arb4) '(4 3 2 1 1 2 1 1 3 2 1 1 2 1 1))

; Test inorderBT

(test (inorderBT arbol-base) '("A" "B" "C" "D" "E" "F" "G" "H" "I"))
(test (inorderBT arb1) '(1))
(test (inorderBT arb2) '(1 2 1))
(test (inorderBT arb3) '(1 2 1 3 1 2 1))
(test (inorderBT arb4) '(1 2 1 3 1 2 1 4 1 2 1 3 1 2 1))

; Test postorderBT

(test (posorderBT arbol-base) '("A" "C" "E" "D" "B" "H" "I" "G" "F"))
(test (posorderBT arb1) '(1))
(test (posorderBT arb2) '(1 1 2))
(test (posorderBT arb3) '(1 1 2 1 1 2 3))
(test (posorderBT arb4) '(1 1 2 1 1 2 3 1 1 2 1 1 2 3 4))
