#lang eopl

; TALLER-1 LP 2023-1

;Stefhania Noguera Romero - 202125854
;Daniel Andrés Mora Muñoz - 201841563
;Isabela Rosero Obando - 202128720


;PUNTO 1 --------------------------------------------------

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


; PUNTO 2 --------------------------------------------------
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


; PUNTO 4 --------------------------------------------------
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


;PUNTO 5 --------------------------------------------------

;list-index
;Proposito: Funcion que retorna el primer elemento de una lista L
;            que cumple con la condición de un predicado P y
;            si no cumple con la condición retorna #f
;P x L -> L
;<P>::= Datatype?
;<L>::= ()
;   ::= <int> <L> | <string> <L>

(define list-index
  (lambda (P L)
    (letrec 
        ((list-aux (lambda ((i 0) (list L));Funcion auxiliar que toma un indice i que se inicia en 0 y una lista L de la funcion list-index
           (cond
             [(null? list) #f]; si la lista es nula se devuelve #f para indicar que el elemento no se encontró en la lista
             [(P (car list)) i]; si la lista no es nula, se comprueba si el predicado P es verdadero para el primer elemento de la lista
                               ; en caso de ser verdadero, se devuelve el indice actual i
             [else (list-aux (+ i 1) (cdr list))]; si no se cumple con lo anterior  se llama list-aux recursivamente
                                                 ; con un i+1 y la cola de la lista
             ))))
      (list-aux)))); se llama la funcion auxiliar

;Pruebas punto 5
(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))


; PUNTO 6 --------------------------------------------------
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


; PUNTO 7 --------------------------------------------------
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


; PUNTO 8 --------------------------------------------------
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


;PUNTO 9 --------------------------------------------------

;inversions
;Proposito: Funcion que que recibe como entrada una lista L,
;          y determina el numero de inversiones de la lista L.
;L -> int
;<L>::= ()
;   ::= <int> <L> 

(define inversions
  (lambda (L)
    
  (define counter ; funcion auxiliar que cuenta las inversiones en L
    (lambda (L n)
    (cond
      ((null? L) n)
      (else
        (counter (cdr L)
                 (pairs (car L) L n)))))) ;se llama a la función pairs para contar el número de pares en L
    
  (define pairs ;funcion auxiliar que cuenta parejas que hay en L
    (lambda (x L n)
    (cond
      [(null? L) n]
      [(> x (car L))
       (pairs x (cdr L) (+ n 1))];si x es mayor al primer elemento de L se llama a pairs con el resto de la fila y con n+1
      [else
       (pairs x (cdr L) n)])));si se han recorrido todos los elementos de la lista, counter devuelve n

  (counter L 0)));contador n que se inicializa en 0

;Pruebas punto 9
(inversions '(2 3 8 6 1))
(inversions '(1 2 3 4))
(inversions '(3 2 1))


;PUNTO 10 --------------------------------------------------
; Proposito:Funcion que recibe una lista L y retorna la lista
;         sin el par de parentesis de cada elemento del nivel mas alto, la lista incluye los elemetnos que no tienen parentesis
; L -> L
; <L>::= '()
;   ::= <Int> <L> | <string> <L>
(define up
  (lambda (L) ;funciónque recibe como parametro una lista L
    (cond
      [(null? L) '()] ;si la lista está vacía, devuelve una lista vacía
      [(and (list? (car L)) (null? (car L))) (up (cdr L))] ;si el primer elemento de la lista es una sub-lista vacía,
                                                           ;elimina esa sub-lista y llama a la función recursivamente con el resto de la lista
      [(list? (car L)) (cons (caar L) (up (cons (cdar L) (cdr L))))];si no está vacía, promueve su primer elemento a nivel superior y llama a 'up' con una nueva lista
                                                                     ;que contiene el resto de los elementos de la sub-lista original seguido por el resto de la lista original
      [else (cons (car L) (up(cdr L)))];por el contrario, se agrega ese elemento a la lista resultada y llama 'up' con el resto de la lista
      )
    )
  )

;Pruebas punto 10
(up '((1 2) (3 4)))
(up '((x (y)) z))


; PUNTO 11 --------------------------------------------------
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



;PUNTO 12 --------------------------------------------------

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


;PUNTO 13 --------------------------------------------------

; operate
; Proposito: funcion que recibe una lista de funciones binarias de tama˜no n (Irators)
;            y una lista de numeros de tama˜no n+1 (lrands).
;            Y retorna el resultado de aplicar sucesivamente las operaciones en lrators a los valores en lrands.
;Irators x Irands -> <int>
;<Irators> ::= <lista>
;<Irands> ::= <lista>

(define operate 
  (lambda (lrators lrands);toma dos listas como parametros
    (cond
      [(null? lrators) (car lrands)];si la lista de Irators esta vacia, devuelve el primer elemento de Irands
      [else (operate (cdr lrators)(cons ((car lrators) (car lrands) (cadr lrands)) (cddr lrands)))]
      )));en caso contrario,se llama a sí misma pasando como argumentos la cola de lrators (cdr),
         ;y una nueva lista que se construye concatenando el resultado de aplicar el primer operador en lrators
         ;a los primeros dos elementos de lrands

;Pruebas Punto 13
(operate (list + * + - *) '(1 2 8 4 11 6))
(operate (list *) '(4 5))


;PUNTO 14 --------------------------------------------------

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


;PUNTO 15 --------------------------------------------------

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


;PUNTO 16 --------------------------------------------------

; simpson-rule
; Proposito: función que calcula la integral de una funcion f
;             entre los valores a y b mediante la regla de Simpson
;f x a x b x n -> <int>
; <f> ::= <procedure>
; <a> ::= <Int>
; <b> ::= <Int>
; <n> ::= <<Int>

(define h
   (lambda (a b n) ;funcion para hacer el calculo de la h, h=(b-a)/n
     (/ (- b a) n)))

(define funcion ;funcion para calcular la suma en la regla de simpson
 (lambda (f a b n k);f es la funcion, a y b son los límites de integracion, n el num de intervalo y k es el contador
   (cond
     [(eqv? n k) (f (+ a (* k (h a b n))))]
     [(eqv? k 0) (+ (f (+ a (* k (h a b n)))) (funcion f a b n (+ k 1)))]
     [(even? k) (+ (* 2 (f (+ a (* k (h a b n))))) (funcion f a b n (+ k 1)))]
     [else (+ (* 4 (f (+ a (* k (h a b n))))) (funcion f a b n (+ k 1)))]
     )))

(define simpson-rule ;aplicacion de la regla de Simpson
  (lambda (f a b n)      
    (cond
      [(even? n) (* (/ (h a b n) 3) (funcion f a b n 0))];verifica que el numero de subintervalos n sea par
      [else #f]
      )))     

;Pruebas punto 16
(simpson-rule (lambda (x) (* x (* x x))) 1 5 8)
(simpson-rule (lambda (x) x) 1 5 12)

;PUNTO 17 --------------------------------------------------

; prod-scalar-matriz
; Proposito: recibe una matriz mat como una lista de listas y un vector vec
;            como una lista y retorna el resultado de realizar la multiplicacion matriz por vector
; mat x vec-> Int
; <mat>::= '()
;      ::= ((<int> <int>) <mat>)
; <vec>::= ((<int> <int>))

(define prod-scalar-matriz
  (lambda (mat vec)
    (if (null? mat); si la matriz es vacia devuelve una matriz vacia
        empty
        (cons (cons (*(caar mat) (car vec)) (cons (*(cadar mat) (cadr vec))  empty)) ;multiplica la primera fila de la matriz por el vector y la agrega a la matriz resultante
              (prod-scalar-matriz (cdr mat) vec)) ;llamada a la funcion para multiplicar el resto de las filas de la matriz por el vector y agregarlas a la matriz resultante
        )))
;Pruebas punto 17
(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))
(prod-scalar-matriz '((1 1) (2 2) (3 3) (4 4)) '(3 4))

;Punto 18

;aux
;Proposito: Calcula la posicion (X,Y) del triangulo de pascal
;X x y -> int
;<x> ::= <int>
;<y> ::= <int>

(define aux_1
  (lambda (x y) 
  (if (or (= y 0) (= x y))1(+ (aux_1 (- x 1) y) (aux_1 (- x 1) (- y 1))))))

;pascal_1
;Proposito: Realiza un llamado recursivo a la funcion aux para calcular toda una fila N del triangulo
;n x y -> <lista>
;<n> ::= <int>
;<y> ::= <int>

(define pascal_1
  (lambda (n y)
    (if(= n y) (list(aux_1 n y)) (append (list (aux_1 n y)) (pascal_1 n (+ y 1))))
      ))

;pascal
;Proposito: Llama a la función pascal1 pero usando unicamente un argumento
;n -> <lista>
;<n> ::= <int>
(define pascal
  (lambda (N)
    (pascal_1 (- N 1) 0)
    ))

;pruebas
;(pascal 5)
;(pascal 1)
