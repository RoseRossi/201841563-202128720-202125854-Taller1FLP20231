#lang eopl

; Taller 1 LP

;Stefhania Noguera Romero - 202125854
;Daniel Andrés Mora Muñoz - 201841563
;Isabela Rosero Obando - 202128720

;Punto 1

;Se define un dato multiuplo5? para que funcione como en la prueba requerida.
(define multiplo5? 'multiplo5?)

;se define una función predicado para que actue según el predicado que se le de.
(define predicado
  (lambda (tupla P)
    (cond
      [(equal? P even?)(and (even? (car tupla)) (even? (car (cdr tupla))))]
      [(equal? P 'multiplo5?)(and (equal? (remainder (car tupla) 5) 0)(equal? (remainder (car (cdr tupla)) 5) 0))]
      [(equal? P odd?)(and (odd? (car tupla)) (odd? (car (cdr tupla))))]
      [else #f]
      )))
;invert: S x L -> L' : procedimiento que invierte el orden de las parejas de una lista de tuplas dependiendo del predicado
;uso:(invert l) = invierte el orden de las parejas dentro de la lista l
;<lista> := ()
;          := (<tuplas> <lista>)
;<tuplas> := (<char> <char>)
(define invert
  (lambda (list P)
    (cond
      [(null? list)
       empty]
      [else (if (predicado (car list) P)
                (cons (cons (car (cdr (car list))) (cons (car (car list)) empty))(invert (cdr list) P))
                (invert (cdr list) P))
            ]
      )))

;Pruebas
;(invert '((3 2) (4 2) (1 5) (2 8)) even?)
;(invert '((5 9) (10 90) (82 7)) multiplo5?)
;(invert '((6 9) (10 90) (82 7)) odd?)

;Punto 7
;funcion-append:
;Proposito:Procedimiento que concatena 2 listas.
;L x L -> L’:
;<lista> := ()
;:=<Valor><lista>
;<Valor> := string|int
(define append
  (lambda (l1 l2)
    (cond
      [ (null? l1) l2]
      [else (  cons (car l1) (append (cdr l1) l2) )])))

;cartesian-product:
;Proposito:Procedimiento que concatena el primer elemento de la lista llamado x con cada uno de los elementos de lista
;X x L -> L’ :
; <lista> := ()
; :=<Valor><lista>
; <Valor> := string|int
(define cartesian-product-aux
  (lambda (x L)
    (cond
      [(null? L) L  ]
      [else ( cons (list x (car L)) (cartesian-product-aux x (cdr L)))])))

;cartesian-product :
;Proposito:Procedimiento que invoca a la función auxiliar,teniendo como argumentos el primero de la l1 y la lista l2, donde hace el mismo proceso para los demas elementos de l1.
;L x L -> L’:
;<lista> := ()
; :=<Valor><lista>
; <Valor> := string|int
 (define cartesian-product
   (lambda (l1 l2)
     (cond
       [(or ( null? l1) ( null? l2)) empty]
       [else ( append (cartesian-product-aux (car l1)l2) (cartesian-product (cdr l1)l2) ) ])))

;Pruebas
;(cartesian-product '(a b c) ’(x y))
;(cartesian-product '(p q r) '(5 6 7))
;(cartesian-product '(ba) '(tir lada lazo ño nco rco))

;Punto 12

;filter-acum: a x b x F x acum x filter -> <Int>
;propósito: retorna un acumulador como el resultado de aplicar una operación binaria entre números de un intervalo dado si cumplen un predicado
;<a> :: = <Int>
;<b> :: = <Int>
;<F> :: = +| - | *| /
;<acum> :: = <Int>
;<filter> :: = <datatype?>

(define filter-acum
  (lambda(a b F acum filter)
    (if (or(< a b)(eqv? a b))
        (if(filter a)
	  (F a(filter-acum (+ a 1) b F (+ acum a) filter))
          (filter-acum (+ a 1) b F acum filter)) 0)))
;Pruebas
;(filter-acum 1 10 + 0 even?)
;(filter-acum 1 10 + 0 odd?)
