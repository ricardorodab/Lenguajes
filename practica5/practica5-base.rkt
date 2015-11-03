#lang plai

(define-type Binding
  [bind (name symbol?) (val RCFAELS?)])
;(RCFAELS? (listS (numS 3) (listS (boolS #t) (numS 4))))

(define-type RCFAELS
  [numS (n number?)]
  [withS (bindings (listof bind?))
         (body RCFAELS?)]
  [with*S (bindings (listof bind?))
          (body RCFAELS?)]
  [idS (name symbol?)]
  [funS (params (listof symbol?))
        (body RCFAELS?)]
  [appS (fun RCFAELS?)
        (args (listof RCFAELS?))]
  [binopS (f procedure?)
         (l RCFAELS?)
         (r RCFAELS?)]
  [boolS (b boolean?)]
  [ifS (cond RCFAELS?)
       (case1 RCFAELS?)
       (case2 RCFAELS?)]
  [equalS? (comp1 RCFAELS?)
           (comp2 RCFAELS?)]
  [listS (elems (listof RCFAELS?))])

(define-type FCFAEL
  [num (n number?)]
  [id (name symbol?)]
  [fun (params (listof symbol?))
       (body FCFAEL?)]
  [app (fun FCFAEL?)
       (args (listof FCFAEL?))]
  [binop (f procedure?)
         (l FCFAEL?)
         (r FCFAEL?)]
  [bool (b boolean?)]
  [isequal? (cond1 FCFAEL?)
            (cond2 FCFAEL?)]
  [ifC (cond FCFAEL?)
      (case1 FCFAEL?)
      (case2 FCFAEL?)]
  [listC (restlst (listof FCFAEL?))])
  

(define-type FCFAEL-Value
  [numV (n number?)]
  [boolV (b boolean?)]
  [listV (elements (listof FCFAEL-Value?))]
  [closureV (param (listof symbol?))
            (body FCFAEL?)
            (env Env?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) 
        (value FCFAEL-Value?) 
        (env Env?)])

; FUNCIONES AUXILIARES

;; A::= <number>|<symbol>|listof(<A>)
;; B::= (list <symbol> <A>)
;; parse-bindings: listof(B) -> listof(bind?)
;; "Parsea" la lista de bindings lst en sintaxis concreta
;; mientras revisa la lista de id's en busca de repetidos.
;; (define (parse-bindings lst) 
(define (parse-bindings lst allow)
  (let ([bindRep (buscaRepetido lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))])
    (if (or (boolean? bindRep) allow)
        (map (lambda (b) (bind (car b) (parse (cadr b)))) lst)
        (error 'parse-bindings (string-append "El id " (symbol->string (car bindRep)) " está repetido")))))

(define (elige s)
  (case s
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(<) <]
    [(>) >]
    [(<=) <=]
    [(>=) >=]
    [(and) (and)]
    [(or) (or)]))

    
;; buscaRepetido: listof(X) (X X -> boolean) -> X
;; Dada una lista, busca repeticiones dentro de la misma
;; usando el criterio comp. Regresa el primer elemento repetido
;; o falso eoc.
;; (define (buscaRepetido l comp) 
(define (buscaRepetido l comp) 
  (cond
    [(empty? l) #f]
    [(member? (car l) (cdr l) comp) (car l)]
    [else (buscaRepetido (cdr l) comp)]))

;; member?: X listof(Y) (X Y -> boolean) -> boolean
;; Determina si x está en l usando "comparador" para
;; comparar x con cada elemento de la lista.
;; (define (member? x l comparador)
(define (member? x l comparador)
  (cond
    [(empty? l) #f]
    [(comparador (car l) x) #t]
    [else (member? x (cdr l) comparador)]))

(define (forAllParse lst)
  (cond
    [(empty? lst) empty]
    [else (cons (parse (car lst)) (forAllParse (cdr lst)))]))

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> RCFAELS
(define (parse sexp)
  (cond
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(list? sexp)
     (case (car sexp)
       [(true) (boolS #t)]
       [(false) (boolS #f)]
       [(equal?) (equalS? (parse (cadr sexp)) (parse (caddr sexp)))] 
       [(if) (ifS (parse (cadr sexp)) (parse (caddr sexp)) (parse (cadddr sexp)))]
       [(with) (withS (parse-bindings (cadr sexp) #f) (parse (caddr sexp)))]
       [(with*) (with*S (parse-bindings (cadr sexp) #t) (parse (caddr sexp)))]
       [(fun) (funS (cadr sexp) (parse (caddr sexp)))]
       [(lista) (listS (map parse (cdr sexp)))]
       [(+ - / * < > <= >= and or) (binopS (elige (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
       [else (appS (parse (car sexp)) (map parse (cdr sexp)))])]))
