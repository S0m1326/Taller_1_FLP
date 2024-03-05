#lang eopl
;Sebastián Orrego Marín - 1941144
;Franklin Aguirre ...


;Función invert
;proposito:
;List -> List: Procedimiento que invierte la posición de pares ordenados de una lista.
;usage: (invert List) = List

(define invert
  (lambda (l)
    (if (null? l)
        empty
        (
         cons (list (cdar l)(caar l)) (invert (cdr l))
              )
        )
    )
  )

;Pruebas:


;Función down
;proposito:
;List -> List: Procedimiento que añade un nivel mas de paréntesis comparado con su estado
;original
;usage: (down List) = List

(define down
  (lambda (l)
    (if (null? l)
        empty
        (
         cons (list (car l)) (down (cdr l))
              )
        )
    )
  )

;Pruebas:


;Función list-set
;proposito:
;List, n, x -> List: Procedimiento que sustituye en la posición n de una lista por el
;elemento x
;usage: (list-set List n x) = List

(define list-set
  (lambda (l n x)
    (letrec
        (
         (sume
          (lambda (l1 c)
            (if (null? l1)
                empty
                (if (not (= c n))
                    (cons (car l1) (sume (cdr l1) (+ c 1)))
                    (cons x (sume (cdr l1) (+ c 1)))
                    )
                )
            )
          )
         )
      (sume l 0)
      )
    )
  )

;Pruebas:


;Función filter-in
;proposito:
;P, List -> List: Procedimiento que retorna una lista que
;contiene los elementos que pertenecen a L y que satisfacen el predicado P.
;usage: (filter-in List P) = List

(define filter-in
  (lambda (P l)
    (if (null? l)
        empty
        (if (P (car l))
            (cons (car l) (filter-in P (cdr l)))
            (filter-in P (cdr l))
            )
        )
    )
  )

;Pruebas:


;Función mix
;proposito:
;List1, List2 -> List: Procedimiento que retorna una sola lista con los
;elementos cruzados entre ambas listas
;usage: (mix List List2) = List

(define mix
  (lambda (l1 l2)
    (if (or (null? l1) (null? l2))
        empty
        (
         cons (car l1) (cons (car l2) (mix (cdr l1) (cdr l2)))
         )
        )
    )
  )

;Pruebas:


;Función swapper
;proposito:
;E1, E2, List -> List: Procedimiento que  retorna una lista similar a L,
;sólo que cada ocurrencia anterior de E1 será reemplazada por E2
;y cada ocurrencia anterior de E2 será reemplazada por E1.
;usage: (swapper E1 E2 List) = List

(define swapper
  (lambda (E1 E2 l)
    (if (null? l)
        empty
        (if (eqv? E1 (car l))
            (cons E2 (swapper E1 E2 (cdr l)))
            (
             if (eqv? E2 (car l))
                (cons E1 (swapper E1 E2 (cdr l)))
                (
                 cons (car l) (swapper E1 E2 (cdr l))
                      )
               )
            )
        )
    )
  )

;Pruebas:


;Función cartesian-product
;proposito:
;List1, List2 -> List: Procedimiento que retorna una lista de tuplas que representa
;el producto cartesiano entre L1 y L2
;usage: (cartesian-product List1 List2) = List

(define cartesian-product
  (lambda (l1 l2)
    (letrec
        (
         (sume
          (lambda (l3 l4)
            (if (or (null? l1) (null? l2))
                empty
                (
                 if (null? l4)
                    (sume (cdr l3) l2)
                    (
                     if (null? l3)
                        empty
                        (
                         cons (list (car l3) (car l4)) (sume l3 (cdr l4))
                         )
                        )
                    )
                )
            )
          )
         )
      (sume l1 l2)
      )
    )
  )

;Pruebas:


;Función mapping
;proposito:
;F, List1, List2 -> List: Procedimiento que La función debe retornar una lista de
;pares (a,b) siendo a elemento de L1 y b elemento de L2, cumpliéndose la
;propiedad que al aplicar la función unaria F con el argumento a, debe arrojar
;el número b.
;usage: (mapping F List1 List2) = List

(define mapping
  (lambda (F l1 l2)
    (letrec
        (
         (sume
          (lambda (l3 l4)
            (if (or (null? l1) (null? l2))
                empty
                (
                 if (null? l3)
                    empty
                    (
                     if (null? l4)
                        (sume (cdr l3) l2)
                        (
                         if (= (F (car l3)) (car l4))
                            (cons (list (car l3) (car l4)) (sume (cdr l3) (cdr l4)))
                            (sume l3 (cdr l4))
                         )
                        )
                    )
                )
            )
          )
         )
      (sume l1 l2)
      )
    )
  )

;Pruebas:


;Función reverse
;proposito:
;List -> List: Procedimiento que retorna la misma lista con los elementos
;en posiciones invertidas.
;usage: (reverse List) = List

(define reverse
  (lambda (l)
    (letrec
        (
         (rec
             (lambda (l1 l2)
               (if (null? l1)
               l2
               (
                rec (cdr l1) (cons (car l1) l2)
                 )
               )
               )
           )
         )
      (rec l '())
      )
    )
  )

;Pruebas:


;Función flatten
;proposito:
;List -> List: Procedimiento que retorna la lista ”aplanada”, es decir,
;sin ningún tipo de lista anidada interna conservando todos los elementos
;usage: (flatten List) = List

(define flatten
  (lambda (l)
    (letrec
        (
         (rec
             (lambda (l l1)
               (if (null? l1)
                   (if (null? (cdr l))
                       empty
                       (rec (cdr l) (cdr l))
                       )
                   (if (list? (car l1))
                       (rec l (car l1))
                       (cons (car l1) (rec l (cdr l1)))
                       )
                   )
               )
           )
          
         )
      (rec l l)
      )
    )
  )

;Pruebas:


;Función unzip
;proposito:
;List -> List, List: Procedimiento que retorna dos listas resultantes de
;descomponer la lista original en dos, la primera con el primer elemento de
;las tuplas y la segunda con el segundo elemento de las tuplas.
;usage: (unzip List) = List, List

(define unzip
  (lambda (l)
    (letrec
        (
         (rec1
          (lambda (l1)
            (if (null? l1)
                empty
                (cons (cdar l1) (rec1 (cdr l1)))
                )
            )
          )
         (rec2
          (lambda (l2)
            (if (null? l2)
                empty
                (cons (caar l2) (rec2 (cdr l2)))
                )
            )
          )
         )
      (cons (rec2 l) (list (flatten (rec1 l))))
      )
    )
  )

;Pruebas:


;Función scan
;proposito:
;List, n, F -> List, List: Procedimiento que retorna una lista con cada
;resultado parcial de aplicara la función binaria F con cada elemento
;de la lista de forma acumulativa empezando con el elemento n.
;usage: (scan List n F) = List

(define scan
  (lambda (l n F)
    (if (null? l)
        (cons n '())
        (cons n (scan (cdr l) (F n (car l)) F))
        )
    )
  )

;Pruebas:


;Función operate
;proposito:
;lrators, lrands -> Int: Procedimiento que retorna el resultado de
;aplicar sucesivamente las operaciones en lrators a los valores en lrands.
;usage: (operate lrators lrands) = Int

(define operate
  (lambda (lrators lrands)
    (letrec
        (
         (rec
             (lambda (l1 l2 cont)
               (if (null? l1)
                   cont
                   (rec (cdr l1) (cdr l2) ((car l1) cont (car l2)))
                   )
               )
           )
         )
      (rec (cdr lrators) (cddr lrands) ((car lrators) (car lrands) (cadr lrands)))
      )
    )
  )

;Pruebas:


;Función path
;proposito:
;Number, BST -> List: Procedimiento que retorna una lista con la ruta a tomar
;desde el nodo raíz del árbol binario de búsqueda BST hasta el número n.
;usage: (path n BST) = List

(define path
  (lambda (n BST)
    (if (null? BST)
        empty
        (if (= n (car BST))
            empty
            (if (< n (car BST))
                (cons 'left (path n (cadr BST)))
                (cons 'right (path n (caddr BST)))
                )
            )
        )
    )
  )

;Pruebas:


;Función in-order
;proposito:
;BST -> List: Procedimiento que retorna una lista con los elementos del árbol binario
;correspondientes a recorrerlo inorder. En el recorrido inorder los nodos se visitan
;de la forma (izquierda, raíz, derecha).
;usage: (in-order BST) = List

(define inorder
  (lambda (BST)
    (if (null? BST)
        empty
        (append (inorder (cadr BST))
                (cons (car BST) (inorder (caddr BST))))
        )
    )
  )

;Pruebas:


;Función Operar-binarias
;proposito:
;operacionB -> Int: Procedimiento que retorna el resultado de hacer
;las operaciones suma, resta y multiplicación correspondientes.
;usage: (Operar-binarias operacionB) = Int

(define Operar-binarias
  (lambda (operacionB)
    (if (number? operacionB)
        operacionB
        (if (list? operacionB)
            (cond
              [(eq? (cadr operacionB) 'suma) (+ (Operar-binarias (car operacionB)) (Operar-binarias (car (cddr operacionB))))]
              [(eq? (cadr operacionB) 'resta) (- (Operar-binarias (car operacionB)) (Operar-binarias (car (cddr operacionB))))]
              [(eq? (cadr operacionB) 'multiplica) (* (Operar-binarias (car operacionB)) (Operar-binarias (car (cddr operacionB))))]
              )
            ("Error")
          )
        )
    )
  )

;Pruebas:


;Función prod-scalar-matriz
;proposito:
;mat, vec -> List: Procedimiento que retorna el resultado de realizar la
;multiplicación matriz por vector.
;usage: (prod-scalar-matriz mat vec) = List

(define prod-scalar-matriz
  (lambda (mat vec)
    (if (null? mat)
        empty
        (letrec
            (
             (suma
              (lambda (l1 l2)
                (if (null? l1)
                    empty
                    (cons (* (car l1) (car l2)) (suma (cdr l1) (cdr l2)))
                    )
                )
              )
             )
          (cons (suma (car mat) vec) (prod-scalar-matriz (cdr mat) vec))
          )
        )
    )
  )

;Pruebas:


;Función pascal
;proposito:
;N -> List: Procedimiento que retorna la fila N del triangulo de Pascal.
;usage: (pascal N) = List

(define pascal
  (lambda (N)
    (letrec
        (
         (nuevaFila
          (lambda (cont filaAnt)
            (if (= cont N)
                filaAnt
                (nuevaFila (+ cont 1) (suma (flatten (list (list 0) filaAnt)) (flatten (list filaAnt (list 0)))))
                )
            )
          )
         (suma
          (lambda (l1 l2)
            (if (null? l1)
                empty
                (cons (+ (car l1) (car l2)) (suma (cdr l1) (cdr l2)))
                )
            )
          )
         )
      (nuevaFila 1 '(1))
      )
    )
  )

;Pruebas: