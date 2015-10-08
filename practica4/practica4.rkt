#lang plai

(require "practica4-base.rkt")

(print-only-errors true)

; Ejercicio desugar

(define (desugar expr)
  (type-case FAES expr
    [numS (n) (num n)]
    [idS (name) (id name)]
    [funS (params body) (fun params (desugar body))]
    [appS (fuS ars) (app (desugar fuS) (dameFAE ars))]
    [binopS (f l r) (binop f (desugar l) (desugar r))]
    [withS (bindings body) (app (fun (sacaName bindings) (desugar body)) (sacaVal bindings))]
    [with*S (bindings body) (app (fun (sacaName bindings) (desugar body)) (sacaVal bindings))]))

(define (sacaVal lst)
  (if (empty? lst)
      '()
      (type-case Binding (car lst)
        [bind (name val) (cons (desugar val) (sacaVal (cdr lst)))])))

; Me da el nombre de las funciones.

(define (sacaName lst)
  (if (empty? lst)
      '()
      (type-case Binding (car lst)
        [bind (name val) (cons name (sacaName (cdr lst)))])))

; Función auxiliar para manejar listas de FAEs.

(define (dameFAE lst)
  (if (empty? lst)
      '()
      (cons (desugar (car lst)) (dameFAE (cdr lst)))))


(define (cparse sexp)
  (desugar (parse sexp)))

; Función interp.

(define (interp expr env)
 (type-case FAE expr
   [num (n) (numV n)]
   [binop (f l r) (opV f (interp l env) (interp r env))]
   [id (name) (lookup name env)]
   [fun (params body) (closureV params body env)]
   [app (fu args) (local ([define fulist (interp fu env)])
                         (appArgs fu (closureV-param fulist) args env))]))

; Función auxiliar para la interpretación de aplicación de funciones.

(define (appArgs fu fulst args env)
  (if (empty? (cdr fulst))
      (local  ([define fun-val (interp fu env)])
        (interp (closureV-body fun-val)
                (aSub (car fulst)
                      (interp (car args) env) 
                      (closureV-env fun-val))))
      (appArgs fu (cdr fulst) (cdr args) (aSub (car fulst)
                                               (interp (car args) env)
                                               env))))

; Función para interpretar las funciones binarias.

(define (opV proc num1 num2)
  (type-case FAE-Value num1
    [numV (n1)
          (type-case FAE-Value num2
            [numV (n2) (numV (proc n1 n2))]
            [closureV (param body env) (error 'opV "No numV")])]
  [closureV (param body env) (error 'opV "No numV")])) 

(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "No existe el valor "(symbol->string name))]
     [aSub (name2 value env2)
           (if (symbol=? name2 name)
               value
               (lookup name env2))]))
  
(define (rinterp expr)
  (interp expr (mtSub)))

(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))
(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {+ x x}})) (numV 20))
(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z})) (numV 8))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {x 10} {z {+ x y}}} z})) (numV 15))
(test/exn (rinterp (cparse '{with {{x 10} {x 20}} x})) "El id x está repetido")
(test (rinterp (cparse '{with* {{x 10} {x 20}} x})) (numV 20))

; PRUEBAS

(test (desugar (parse '1)) (num 1))
(test (desugar (parse 'x)) (id 'x))
(test (desugar (parse '{fun {x} {x}})) (fun '(x) (app (id 'x) '())))
(test (desugar (parse '{app {fun {x} {x}} 1})) (app (id 'app) (list (fun '(x) (app (id 'x) '())) (num 1))))
(test (desugar (parse '{- 2 2})) (binop - (num 2) (num 2)))


(interp (desugar (parse '{+ 3 4})) '() )
(interp (desugar (parse 'x)) (aSub 'x (numV 2) (mtSub)))
(interp (desugar (parse '{fun {x} {x}})) (mtSub) )
(interp (desugar (parse '{fun {x} {+ x y}})) (aSub 'y (numV 2) (mtSub)) )
(interp (desugar (parse '{- y y})) (aSub 'y (numV 2) (mtSub)) ) 



