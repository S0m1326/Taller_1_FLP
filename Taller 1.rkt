#lang eopl

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

;Función list-set
;proposito:
;List, n, x -> List: Procedimiento que sustituye en la posición n de una lista por el
;elemento x
;usage: (list-set List, n, x) = List

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

;Función filter-in
;proposito:
;P, List -> List: Procedimiento que retorna una lista que
;contiene los elementos que pertenecen a L y que satisfacen el predicado P.
;usage: (filter-in List, P) = List

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

;Función mix
;proposito:
;List1, List2 -> List: Procedimiento que retorna una sola lista con los
;elementos cruzados entre ambas listas
;usage: (mix List, List2) = List

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

;Función swapper
;proposito:
;E1, E2, List -> List: Procedimiento que  retorna una lista similar a L,
;sólo que cada ocurrencia anterior de E1 será reemplazada por E2
;y cada ocurrencia anterior de E2 será reemplazada por E1.
;usage: (swapper E1, E2, List) = List

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

;Función cartesian-product
;proposito:
;List1, List2 -> List: Procedimiento que retorna una lista de tuplas que representa
;el producto cartesiano entre L1 y L2
;usage: (cartesian-product List1, List2) = List

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

;Función mapping
;proposito:
;F, List1, List2 -> List: Procedimiento que La función debe retornar una lista de
;pares (a,b) siendo a elemento de L1 y b elemento de L2, cumpliéndose la
;propiedad que al aplicar la función unaria F con el argumento a, debe arrojar
;el número b.
;usage: (mapping F, List1, List2) = List

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

;Función flatten
;proposito:
;List -> List: Procedimiento que retorna la lista ”aplanada”, es decir,
;sin ningún tipo de lista anidada interna conservando todos los elementos
;usage: (flatten List) = List

(define flatten
  (lambda (l)
    (
     
     )
    )
  )