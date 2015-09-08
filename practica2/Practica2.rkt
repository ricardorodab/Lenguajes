#lang plai

;Ejercicio Array.

(define-type Array
  [MArray (n number?)
          (lst list?)])

; Ejercicio MList

(define (any? x) #t)

(define-type MList
  [MEmpty]
  [MCons (n any?)
         (lst MList?)])


; Ejercicio NTree
(define-type NTree
  [TLEmpty]
  [NodeN (n number?)
         (lst list?)])
   
; Ejercicio Position
(define-type Position
  [2D-Point (n number?)
            (m number?)])

; Ejercicio Figure
(define-type Figure
  [Circle (n Position?)
          (m number?)]
  [Square (n Position?)
          (m number?)]
  [Rectangle (n Position?)
             (m number?)
             (o number?)])

  
; PARTE 2

; Ejercicio setValueA

(define (setvalueA ar pos val)
  (type-case Array ar
    [MArray (n lst)
            (if (>= pos n)
                (error "Out of bounds")
                (MArray n (cambia lst '() pos val)))]))

; Ejercicio MArray2MList.

(define (MArray2MList ar)
  (type-case Array ar
    [MArray (n lst) (cast2MList n lst)]))

; Ejercicio printML

;(define (printML lst)
(define (printML l )
  (letrec ([printMLAux (lambda (lm acc)
                         (type-case MList lm
                           [MEmpty ()  (substring acc 1) ]
                           [MCons (n lis)   (if (MList? n) (string-append (printML n ) "," (printMLAux lis "") ) (printMLAux lis (string-append  acc  "," (~a n) )) )  ]))])
    (type-case MList l
      [MEmpty () "[]" ]
      [MCons (n lis)  (format "[~a]" (printMLAux l "") ) ])))


; Ejercicio concatML
(define (concatML lst1 lst2)
  (type-case MList lst1
    [MEmpty () (continuaLista2 lst2)]
    [MCons (n lst) (MCons n (concatML lst lst2))]))

; Ejercicio lengthML.
(define (lengthML lst)
  (type-case MList lst
    [MEmpty () 0]
    (MCons (n lst2) (+ 1 (lengthML lst2)))))

; Ejercicio mapML
(define (mapML f lst)
  (type-case MList lst
    [MEmpty () (MEmpty)]
    [MCons (n lst2) (MCons (f n) (mapML f lst2))]))

; Ejercicio filterML
(define (filterML f lst)
  (type-case MList lst
    [MEmpty () (MEmpty)]
    [MCons (n lst2) (if (f n)
                        (MCons n (filterML f lst2))
                        (filterML f lst2))]))

(define-type Coordinates
  [GPS (lat number?)
       (long number?)])
(define-type Location
  [building (name string?)
            (loc GPS?)])
;; Coordenadas GPS
(define gps-satelite (GPS 19.510482 -99.23411900000002))
(define gps-ciencias (GPS 19.3239411016 -99.179806709))
(define gps-zocalo (GPS 19.432721893261117 -99.13332939147949))
(define gps-perisur (GPS 19.304135 -99.19001000000003))

(define plaza-satelite (building "Plaza Satelite" gps-satelite))
(define ciencias (building "Facultad de Ciencias" gps-ciencias))
(define zocalo (building "Zocalo" gps-zocalo))
(define plaza-perisur (building "Plaza Perisur" gps-perisur))

(define plazas (MCons plaza-satelite (MCons plaza-perisur (MEmpty))))



;Ejercicio haversine

(define (haversine gps1 gps2)
  (let*  [(radio-tierra 6371)
          (lat (degrees->radians (- (dameLatitud gps2 ) (dameLatitud gps1))))
          (long (degrees->radians (- (dameLongitud gps2) (dameLongitud gps1 ))))
          (a  (+ (sqr(sin(/ lat 2))) (* (cos (degrees->radians (dameLatitud gps1))) (* (cos (degrees->radians (dameLatitud gps2))) (sqr (sin (/ long 2)))))))
          (c (* 2 (asin (sqrt a))))
          (d (* radio-tierra c))]

    d))


; Ejercicio gps-coordinates

(define (gps-coordinates lst)
  (type-case MList lst
    [MEmpty () (MEmpty)]
    [MCons (n lst2) (MCons (if (MList? n)
                               (gps-coordinates n)
                               (dameCoordenadas n)) (gps-coordinates lst2))]))

; Ejercicio closest-building

(define (closest-building coord lst2)
  (type-case Location coord
    [building (name loc)
              (type-case Coordinates loc
              [GPS (lat long)
                   (diferencias lat long lst2 -1 (GPS lat long))])]))

; Ejcercicio buildings-at-distance

(define (buildings-at-distance edif lst pto)
  (type-case Location edif
    [building (name loc)
              (type-case Coordinates loc
                [GPS (lat long)
                     (alrededor lat long lst (/ pto 100) (MEmpty))])]))


; Ejercicio area
(define (area figura)
  (type-case Figure figura
    [Circle (pos num) (* 3.1415926535 (* num num))]
    [Square (pos num) (* num num)]
    [Rectangle (pos x y) (* x y)]))


; Ejercicio in-figure?

;(define (in-figure? figura punto)
;  (type-case Figure figura
;    [Circle (pos num) (estaPunto 1 punto pos num num)]
;    [Square (pos num) (estaPunto 2 punto pos num num)]
;    [Rectangle (pos x y) (estaPunto 3 punto pos x y)]))



; FUNCIONES AUXILIARES

; Función auxiliar para encontrar la distancia menor entre dos puntos del GPS.

(define (diferencias lat long lst n gps)
  (type-case MList lst
    [MEmpty () gps]
    [MCons (n1 lst2) (if
                      (or (<= (sqrt (+ (sqr (- lat (lat-coordinates n1))) (sqr (- long (long-coordinates n1))))) n) (= n -1))
                      (diferencias lat long lst2 (sqrt (+ (sqr (- lat (lat-coordinates n1))) (sqr (- long (long-coordinates n1))))) n1)
                      (diferencias lat long lst2 n gps))]))

; Función auxiliar que nos da la latitud de unas coordenadas GPS.

(define (lat-coordinates gps)
  (type-case Location gps
    [building (name loc)
              (type-case Coordinates loc
                [GPS (lat long) lat])]))

; Función auxiliar que nos da la longitud de unas coordenadas GPS.

(define (long-coordinates gps)
  (type-case Location gps
    [building (name loc)
              (type-case Coordinates loc
                [GPS (lat long) long])]))

; Función que nos da la latitud de una coordenada
(define (dameLatitud gps)
  (type-case Coordinates gps
    [GPS (lat long) lat]))

; Función que nos da la longitud de una coordenada
(define (dameLongitud gps)
  (type-case Coordinates gps
    [GPS (lat long) long]))

; Función que nos da las coordenadas de un punto en el mapa.

(define (dameCoordenadas gps)
  (type-case Location gps
    [building (name loc)
              (type-case Coordinates loc
                [GPS (lat long) (GPS lat long)])]))

; Función auxiliar para poder concatenar otra lista a la primera.

(define (continuaLista2 lst2)
  (type-case MList lst2
    [MEmpty () (MEmpty)]
    [MCons (n lst) (MCons n (continuaLista2 lst))]))

; Función auxiliar que nos da dado un punto, edificios que estén a cierta distancia.

(define (alrededor lat long lst pto Mlist)
  (type-case MList lst
    [MEmpty () Mlist]
    [MCons (n1 lst2) (if
                      (<= (+ (sqrt (+ (sqr (- lat (lat-coordinates n1))) (sqr (- long (long-coordinates n1))))) .006) pto)
                      (MCons n1 (alrededor lat long lst2 pto Mlist))
                      (alrededor lat long lst2 pto Mlist))]))

; Función auxiliar para cambiar un arreglo a lista propias.

(define (cast2MList n lst)
  (cond
    [(zero? n) (MEmpty)]
    [(empty? lst) (MEmpty)]
    [else (MCons (car lst) (cast2MList (- n 1) (cdr lst)))]))

; Función auxiliar para aceptar cadenas y numberos.
(define (numOCadena? n)
  (cond
    [(number? n) #t]
    [(string? n) #t]
    [else #t]))

; Función auxiliar para cambiar un elemento de posición en la lista.

(define (cambia lst lstFin pos val)
  (cond
    [(zero? pos) (mconcat lstFin (cons val (cdr lst)))]
    [else (cambia (cdr lst) (cons (car lst) lstFin) (- pos 1) val)]))



; Ejercicio mconcat
; Función que nos genera una sola lista dada dos listas (concat).

(define (mconcat l1 l2)
  (cond
    [(eq? l1 empty) l2]
    [(eq? l2 empty) l1]
    [else  (cons (car l1) (mconcat (cdr l1) l2))]))
