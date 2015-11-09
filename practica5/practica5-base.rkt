#lang plai

(define-type Binding
  [bind (name symbol?) (val RCFAELS?)])



(define-type RCFAELS
  [MEmptyS]
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
  [boolOpBinS (f symbol?)
             (l RCFAELS?)
             (r RCFAELS?)]
  [opS (f symbol?)
       (rst RCFAELS?)]
  [boolS (b boolean?)]
  [ifS (cond RCFAELS?)
       (case1 RCFAELS?)
       (case2 RCFAELS?)]
  [equalS? (comp1 RCFAELS?)
           (comp2 RCFAELS?)]
  [listS (n RCFAELS?)
         (rst RCFAELS?)]
  [recS (name symbol?)
        (name-exp RCFAELS?)
        (body RCFAELS?)])

(define-type FCFAEL
  [MEmpty]
  [num (n number?)]
  [id (name symbol?)]
  [fun (params (listof symbol?))
       (body FCFAEL?)]
  [app (fun FCFAEL?)
       (args (listof FCFAEL?))]
  [binop (f procedure?)
         (l FCFAEL?)
         (r FCFAEL?)]
  [boolOpBin (f symbol?)
             (l FCFAEL?)
             (r FCFAEL?)]
  [op (f symbol?)
      (rst FCFAEL?)]
  [bool (b boolean?)]
  [isequal? (cond1 FCFAEL?)
            (cond2 FCFAEL?)]
  [ifC (cond FCFAEL?)
      (case1 FCFAEL?)
      (case2 FCFAEL?)]
  [listC (n FCFAEL?)
         (rst FCFAEL?)]
   [rec (name symbol?)
        (name-exp FCFAEL?)
        (body FCFAEL?)])
  

(define-type FCFAEL-Value
  [MEmptyV]
  [numV (n number?)]
  [boolV (b boolean?)]
  [listV (n FCFAEL-Value?)
         (rst FCFAEL-Value?)]
  [closureV (param (listof symbol?))
            (body FCFAEL?)
            (env Env?)])

(define (enCaja? var)
  (and (box? var)
       (FCFAEL-Value? (unbox var))))

(define-type Env
  [mtSub]
  [aSub (name symbol?) 
        (value FCFAEL-Value?) 
        (env Env?)]
  [recSub (name symbol?)
           (value enCaja?)
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
    [(and) 'and]
    [(or) 'or]
    ['and 'and]
    ['or 'or]
    [(inc) 'inc]
    [(dec) 'dec]
    [(zero?) 'zero?]
    [(num?) 'num?]
    [(neg) 'neg]
    [(bool?) 'bool?]
    [(first) 'first]
    [(rest) 'rest]
    [(empty?) 'empty?]
    [(list?) 'list?]))

    
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

(define (parseList exp)
  (if (empty? (cdr exp))
      (listS (parse (car exp)) (MEmptyS))
      (listS (parse (car exp)) (parseList (cdr exp)))))

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> RCFAELS
(define (parse sexp)
  (cond
    [(equal? sexp 'true) (boolS #t)]
    [(equal? sexp 'false) (boolS #f)]
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)]

    [(list? sexp)
     (case (car sexp)
       [(true) (boolS #t)]
       [(false) (boolS #f)]
       [('true) (boolS #t)]
       [('false) (boolS #f)]
       ['quote (parse (cdr sexp))]
       [(equal?) (equalS? (parse (cadr sexp)) (parse (caddr sexp)))] 
       [(if) (ifS (parse (cadr sexp)) (parse (caddr sexp)) (parse (cadddr sexp)))]
       [(with) (withS (parse-bindings (cadr sexp) #f) (parse (caddr sexp)))]
       [(with*) (with*S (parse-bindings (cadr sexp) #t) (parse (caddr sexp)))]
       [(fun) (funS (cadr sexp) (parse (caddr sexp)))]
       [(lista) (if (empty? (cdr sexp))
                    (listS (MEmptyS) (MEmptyS))
                    (parseList (cdr sexp)))]
       [(+ - / * < > <= >=) (binopS (elige (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
       [(and or 'and 'or (or) (and)) (boolOpBinS (elige (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
       [(inc dec zero? num? neg bool? first rest empty? list?) (opS (elige (car sexp)) (parse (cadr sexp)))]
       [(rec) (recS (caadr sexp) (parse (cadadr sexp)) (parse (caddr sexp)))]
       [else (appS (parse (car sexp)) (map parse (cdr sexp)))])]))
