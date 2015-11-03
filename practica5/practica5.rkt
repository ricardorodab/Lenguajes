#lang plai

(require "practica5-base.rkt")

(print-only-errors true)

; Ejercicio desugar

(define (desugar expr)
  (type-case RCFAELS expr
    [numS (n) (num n)]
    [idS (name) (id name)]
    [boolS (b) (bool b)]
    [equalS? (cond1 cond2) (isequal? (desugar cond1) (desugar cond2))]
    [ifS (cond case1 case2) (ifC (desugar cond) (desugar case1) (desugar case2))]
    [funS (params body) (fun params (desugar body))]
    [appS (fuS ars) (app (desugar fuS) (dameFCFAEL ars))]
    [binopS (f l r) (binop f (desugar l) (desugar r))]
    [listS (lst) (listC (map desugar lst))]
    [withS (bindings body) (app (fun (sacaName bindings) (desugar body)) (sacaVal bindings))]
    [with*S (bindings body) ;Nueva implementación de with*S
            (if (empty? bindings)
                (desugar body)
                (desugar (withS (list (car bindings)) (with*S (cdr bindings) body))))]))
    ;    [with*S (bindings body) (app (fun (sacaName bindings) (desugar body)) (sacaVal bindings))]))

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

; Función auxiliar para manejar listas de RCFAELS.

(define (dameFCFAEL lst)
  (if (empty? lst)
      '()
      (cons (desugar (car lst)) (dameFCFAEL (cdr lst)))))


(define (cparse sexp)
  (desugar (parse sexp)))

; Función interp.

(define (interp expr env)
 (type-case FCFAEL expr
   [num (n) (numV n)]
   [bool (b) (boolV b)] ;AQUI
   [listC (lst) (forAllInterp lst env)]
   [ifC (cond case1 case2) (if
                            (valorBool (interp cond env))
                            (interp case1 env)
                            (interp case2 env))]
   [isequal? (cond1 cond2) (valorEqual (interp cond1 env) (interp cond2 env))]
   [binop (f l r) (opV f (interp l env) (interp r env))]
   [id (name) (lookup name env)]
   [fun (params body) (closureV params body env)]
   [app (fu args) (if (revisaArgs args env)
                      (local ([define fulist (interp fu env)])
                         (appArgs fu (closureV-param fulist) args env))
                      (error 'interp "Existe un símbolo que no está en el ambiente"))]))

(define (forAllInterp lst env)
  (cond
    [(empty? lst) empty]
    [else (cons (interp (car lst) env) (forAllInterp (cdr lst) env))]))

(define (valorEqual cond1 cond2)
  (cond
    [(list? cond1)
     (if (not (list? cond2))
         (boolV #f)
         (if (= (length cond1) (length cond2))
             (boolV #t) ;REVISAR 1 X 1
             (boolV #f)))]
     [(list? cond2) (boolV #f)]
     [else (type-case FCFAEL-Value cond1
             [numV (n)
                   (type-case FCFAEL-Value cond2
                     [numV (m) (boolV (= n m))]
                     [else (boolV #f)])]
             [boolV (b)
                    (type-case FCFAEL-Value cond2                     
                      [boolV (a) (boolV (equal? a b))]
                      [else (boolV #f)])]
             [else (boolV #f)])]))

(define (valorBool cond)
  (type-case FCFAEL-Value cond
    [boolV (b) b]
    [else (error 'valorBool "La condición no es de tipo bool #b")]))

; Revisa que los argumentos estén en el env.

(define (revisaArgs args env)
  (if (empty? args)
      #t
      (and (lookupExiste (car args) env) (revisaArgs (cdr args) env))))

; Revisa que un sólo argumento se encuentre en el env.

(define (lookupExiste args env)
  (type-case FCFAEL args
    [num (n) #t]
    [bool (b) #t]
    [ifC (cond case1 case2) #t]
    [isequal? (cond1 cond2) #t]
    [listC (lst) #t]
    [binop (f l r) (and (lookupExiste l env) (lookupExiste r env))]
    [id (name) 
        (type-case Env env
          [mtSub () #f]
          [aSub (name2 value env2)
                (if (symbol=? name2 name)
                    #t
                    (lookupExiste args env2))])]
    [fun (params body) #t]
    [app (fu args) #t]))

; Función auxiliar para la interpretación de aplicación de funciones.

(define (appArgs fu fulst args env)
  (if (empty? (cdr fulst))
      (local ([define fun-val (interp fu env)])
        (interp (closureV-body fun-val)
                (aSub (car fulst)
                      (interp (car args) env)
                      (closureV-env fun-val))))
      (appArgs fu (cdr fulst) (cdr args) (aSub (car fulst)
                                               (interp (car args) env)
                                               env))))


; Función para interpretar las funciones binarias.

#|(define (opV proc num1 num2)
  (type-case FCFAEL-Value num1
    [numV (n1)
          (type-case FCFAEL-Value num2
            [numV (n2) (numV (proc n1 n2))]
            [else (error 'opV "No numV")])]   
    [else (error 'opV "No numV")]))


(case proc
                    [(+) (numV (proc arg1 arg2))]
                    [(-) (numV (proc arg1 arg2))]
                    [(*) (numV (proc arg1 arg2))]
                    [(/) (numV (proc arg1 arg2))]
    [(<) (boolV (< arg1 arg2))]
    [(>) (boolV (> arg1 arg2))]
    [(<=) (boolV (<= arg1 arg2))]
    [(>=) (boolV (>= arg1 arg2))]
    [(and) (boolV (and arg1 arg2))]
    [(or) (boolV (or arg1 arg2))]))
|#

(define (opV proc arg1 arg2)
  (type-case FCFAEL-Value arg1
    [numV (n1)
          (type-case FCFAEL-Value arg2
            [numV (n2)
                  (if (number? (proc n1 n2))
                    (numV (proc n1 n2))
                    (boolV (proc n1 n2)))]
            [else  (error 'opV "No numV")])]
    [boolV (b1)
           (type-case FCFAEL-Value arg2
             [boolV (b2) (boolV (proc b1 b2))]
             [else (error 'opV "No boolV")])]
    [else (error 'opV "No numV")]))

                  

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

; Prueba del with*

(test/exn (rinterp (cparse '{{fun {x y} y} 3 {+ 2 x}})) "Existe un símbolo que no está en el ambiente")

(test (interp (desugar (parse '{+ 3 4})) '() ) (numV 7))
(test (interp (desugar (parse 'x)) (aSub 'x (numV 2) (mtSub))) (numV 2))
(test (interp (desugar (parse '{fun {x} {x}})) (mtSub) ) (closureV '(x) (app (id 'x) '()) (mtSub)))
(test (interp (desugar (parse '{fun {x} {+ x y}})) (aSub 'y (numV 2) (mtSub)) ) (closureV '(x) (binop + (id 'x) (id 'y)) (aSub 'y (numV 2) (mtSub))))
(test (interp (desugar (parse '{- y y})) (aSub 'y (numV 2) (mtSub)) ) (numV 0))


; PRUEBAS PRÁCTICA 5


(test (rinterp (cparse true)) (boolV #t))

(test (interp (desugar (parse '{lista 22 (false) (+ 2 3) (/ 27 9) (true)})) (mtSub)) (list (numV 22) (boolV #f) (numV 5) (numV 3) (boolV #t)))

(test (rinterp (cparse {if (< {+ 2 2} {* 2 2}) 1024 2048})) (numV 2048))
(test (rinterp (cparse {if (<= {+ 2 2} {* 2 2}) 1024 2048})) (numV 1024))
(test (rinterp (cparse {or true false})) (boolV #t))
(test (rinterp (cparse {equal? 4 5})) (boolV #f))
(test (rinterp (cparse {equal? 5 5})) (boolV #t))



