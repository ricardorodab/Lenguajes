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



(zones 50 180)


