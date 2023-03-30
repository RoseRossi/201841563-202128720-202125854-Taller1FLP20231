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

;(invert '((3 2) (4 2) (1 5) (2 8)) even?)
;(invert '((5 9) (10 90) (82 7)) multiplo5?)
;(invert '((6 9) (10 90) (82 7)) odd?)
