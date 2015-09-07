#lang plai

;Ejercicio Array.

(define-type Array
  [MArray (n number?)
          (lst list?)])

; Ejercicio MList

;(define-type MList
;  [MEmpty]
;  [MCons
;   (n numOCadena?)
;   (lst MList?)])

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
;(define plazas (MCons plaza-satelite (MCons plaza-perisur (MEmpty))))

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


; Función auxiliar para poder concatenar otra lista a la primera.

(define (continuaLista2 lst2)
  (type-case MList lst2
    [MEmpty () (MEmpty)]
    [MCons (n lst) (MCons n (continuaLista2 lst))]))

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
