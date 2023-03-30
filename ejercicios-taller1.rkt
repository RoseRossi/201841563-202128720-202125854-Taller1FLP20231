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



; Punto 2
; down :
; Proposito:
; L -> L’ : Es un procedimiento que le añade un parentesis mas a cada elemento de una lista L
;
;<lista> :=()
; := <Val><list>

(define down
  (lambda (L);define la funcion down con un parametro de entrada llamado L 
    (cond
      [(null? L) L];Si la lista esta vacia entonces la funcion devuelve una lista vacia
      [else (cons (list (car L);devuelve el primer elemento de L creando una lista
                        ) (down(cdr L)))])));Llama a la funcion drown y llama al resto de la lista 

;Pruebas
;(down ’(1 2 3))
;(down ’((una) (buena) (idea)))
;(down ’(un (objeto (mas)) complicado))
;(down '(090201 101102 290803 030102))
;(down '(Habia (una (vez)) alguien (( que )se mato)))



; Punto 4
; filter-in:
; Proposito:
; P x L -> 'L: Es un procedimiento el cual retorna los elementos que pertenecen a L en una lista con la condicion que cumpla con el predicado P
; <P> ::= datatype?
; <L> :== '()

(define filter-in
  (lambda (P L);dos parametros de entrada 
    (if (= (length L) 0);verifica si la lista es vacia o no 
        '();si es verdadero devuelve una lista vacia 
        (if (P (list-ref L 0));mira si el primer elemento de la lista es verdadero para P
            (cons (car L) (filter-in P (cdr L))); devuelve una lista con el primer elemento y el resto que sean verdadero para P 
            (filter-in P (cdr L));llama a la lista sin contar el primer elemento 
            )
        )
    )
  )
;Pruebas
;(filter-in number? '(e 3 (5 2) w 1 ))
;(filter-in symbol? '(x (z y) 123 owo))
;(filter-in string? '(a b c "lenguajes" "de" "programacion" 100 123 (5 6 7)))
;(filter-in number? '(a 2 (1 3) b 7))
;(filter-in symbol? '(a (b c) 17 foo))
;(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))


; Punto 6
; swapper:
; Proposito: 
; E1 x E2 x L -> 'L: Es un procedimiento que devuelve los elementos pertenecientes a L, intercambiando E1 y E2 en una lista
; <E1> ::= <int> | <string>
; <E2> ::= <int> | <string>
; <L> :== '()
;     :== <int> <L> | <string> <L>

(define swapper
  (lambda (E1 E2 L);define la funcion con los parametros de entrada 
  (cond [(null? L) empty];verifica si la lista esta vacia, para devolver vacio si es true 
        [(equal? (car L) E1) (cons E2 (swapper E1 E2 (cdr L)))]; mira si el primer elemnto de la lista es igual a E1 si es true manda una lista donde E2 esta de primero
        [(equal? (car L) E2) (cons E1 (swapper E1 E2 (cdr L)))]
        [else (cons (car L) (swapper E1 E2 (cdr L)))];si el primer elemento no es igual ni a E1 ni a E2 
        )
    )
  )
;Pruebas
;(swapper 'a 's '(d f g h))
;(swapper 'q 'w '(e r () t y))
;(swapper 'z 'x '(c v b n m z x c v))
;(swapper 'a 'd '(a b c d))
;(swapper 'a 'd '(a d () c d))
;(swapper 'x 'y '(y y x y x y x x y))



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


; Punto 8
; mapping:
; Proposito:
; F x L1 x L2: Es un procedimiento que a cada elemento de L1 le aplica la funcion F y si el resultado es igual a L2 lo retorna
; <F>::= <lambda exp>
; <lambda exp>::= <identifier>
;             ::= (lambda (<identifier>) <Function>)
; <Function>::= <operator> <identifier> <int>
; <operator>::= + | - | * | /
; <L1>::= '()
;     ::= <int> <L1> | <string> <L1>
; <L2>::= '()
;     ::= <int> <L2> | <string> <L2>


(define mapping
  (lambda (F L1 L2); define los parametros de un argumento y dos listas 
    (if (or (null? L1)  (null? L2));revisa si alguna de las listas esta vacia
        '(); si se cumple devuelve una lista vacia 
        (if(equal? (F (car L1)) (car L2)); compara si el primer elemento aplicandole el F es igual
            (cons (list (car L1) (car L2)) (mapping F (cdr L1) (cdr L2)))
            (mapping F (cdr L1) (cdr L2));llama a la misma funcion sin el primer elemento de esta
            )
        )
    )
  )

;Pruebas
;(mapping (lambda (d) (* d 3)) (list 1 2 3) (list 3 9 12))
;(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
;(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
;(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))




; Punto 11
; zip:
; Proposito:
; F x L1 x L2 -> 'L: Es un procedimiento que aplica la funcion F sobre la posicion nesima de L1 y L2 y lo retorna en una lista
; <F>::= <Binary function>
; <L1>::= '()
;     ::= <Int> <L>
; <L2>::= '()
;     ::= <Int> <L>
(define zip
  (lambda (F L1 L2); define dos listas y un argumento 
    (cond; se utiliza como un if ya que este puede analizar varias condiciones 
      [(eqv? F *)(map * L1 L2)];en estos se hace el analisis de quesi f es suma pues que se sumen y asi 
      [(eqv? F -)(map - L1 L2)]
      [(eqv? F +)(map + L1 L2)]
      [else (map / L1 L2)]; esto es que si no es ninguno de los otros pues se divide xd 
      )
    )
  )
;Pruebas
;(zip - '(123) '(100))
;(zip + '(123 321) '(12 21))
;(zip * '(43 2 1) '(23 5 32))
;(zip + '(1 4) '(6 2))
;(zip * '(11 5 6) '(10 9 8))



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

;Punto 14

;path
;Proposito:  
;n x BST -> <Lista>: Procedimiento que devuelve el camino que se debe recorrer en un arbol binario de busqueda para llegar a un numero(n)
;<n> ::= Int
;<BST> ::= <arbol-binario>
;<arbol-binario> ::= (arbol-vacío) empty
;                ::= (nodo) numero <árbol-binario> <árbol-binario>
(define path
  (lambda (n BST)
    (cond
      [(eqv? (car BST) n) '()]
      [(< (car BST) n)(cons 'right (path n (caddr BST)))]
      [(> (car BST) n)(cons 'left (path n (cadr BST)))]
      )
    )
  )
;Pruebas
;(path 17 '(14 (7 () (12 () ()))(26 (20 (17 () ())())(31 () ()))))
;(path 10 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))

;Punto 15

;count-ood (aux count-odd-and-even): L -> Int
;Proposito: cuenta la cantidad de números impares en una lista l.
;<L> :== '()
;    :== <int> <L>
(define count-odd
  (lambda (lista)
    (cond [(null? lista) 0]
          [(and (number? (car lista)) (odd? (car lista))) (+ 1 (count-odd (cdr lista)))]
          [(or (list? (car lista)) (pair? (car lista))) (+ (count-odd (car lista)) (count-odd (cdr lista)))]
          [else (count-odd (cdr lista))]
          )))

;count-even (aux count-odd-and-even): L -> Int
;Proposito: Procedimiento que cuenta la cantidad de números pares en una lista L.
;<L> :== '()
;    :== <int> <L>
(define count-even
  (lambda (lista)
    (cond [(null? lista) 0]
          [(and (number? (car lista)) (even? (car lista))) (+ 1 (count-even (cdr lista)))]
          [(or (list? (car lista)) (pair? (car lista))) (+ (count-even (car lista)) (count-even (cdr lista)))]
          [else (count-even (cdr lista))]
          )))

;count-odd-and-even : lista -> 'lista
;Proposito: Procedimiento que cuenta la cantidad de números pares e impares de una lista L, y pone sus contadores en una lista.
;<L> :== '()
;    :== <int> <L>
(define count-odd-and-even
  (lambda (lista)
    (cons (count-even lista) (cons (count-odd lista) empty))
    ))
;Pruebas
;(count-odd-and-even '(14 (7 () (12 () ()))(26 (20 (17 () ())())(31 () ()))))
;(count-odd-and-even '(6 (12 40 50) 13)

;punto 17

;prod-scalar-matriz 
;Mat x Vec-> Int:realiza la multiplicacion de matriz por vector y retorna el resultado de dicha operación
;<Mat>::= '()
;     ::= ((<int> <int>) <Mat>)
;<Vec>::= ((<int> <int>))

(define prod-scalar-matriz
  (lambda (Mat Vec)
    (if (null? Mat)
        empty
        (cons (cons (*(car Mat) (car Vec))  (cons (*(cdr Mat) (cadr Vec))  empty)) (prod-scalar-matriz (cdr Mat) Vec))
        )))
;Pruebas
;(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))
;(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
;(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(3 2))
