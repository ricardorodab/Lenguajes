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



(define (bpm->zone fc zone)
  (if (empty? fc)
      empty
      (cons (busca (car fc) zone) (bpm->zone (cdr fc) zone))))

(define (busca n zone)
  (if (empty? zone)
      empty
      (type-case HRZ (car zone) 
        [resting (low high) (if (and (<= low n) (<= n high)) (car zone) (busca n (cdr zone)))]
        [warm-up (low high) (if (and (<= low n) (<= n high)) (car zone) (busca n (cdr zone)))]
        [fat-burning (low high) (if (and (<= low n) (<= n high)) (car zone) (busca n (cdr zone)))]
        [aerobic (low high) (if (and (<= low n) (<= n high)) (car zone) (busca n (cdr zone)))]
        [anaerobic (low high) (if (and (<= low n) (<= n high)) (car zone) (busca n (cdr zone)))]
        [maximum (low high) (if (and (<= low n) (<= n high)) (car zone) (busca n (cdr zone)))]
    )))


(bpm->zone empty my-zones)
(bpm->zone '(50 60) my-zones)
(bpm->zone '(140 141) my-zones)

