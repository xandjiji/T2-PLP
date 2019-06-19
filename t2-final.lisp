;; funcao desparentiza (usada nos dois exercicios)

(defun desparentiza(L)
       (cond    ((null L) ())
                ((atom (car L)) (append (list (car L)) (desparentiza (cdr L))))
                (t (append (desparentiza(car L)) (desparentiza(cdr L))))
       )
)



;; funcoes do primeiro exercicio

(defun contaElemento(E L)
    (cond   ((null L) 0)
            ((equal E (car L)) (+ 1 (contaElemento E (cdr L))))
            (t (contaElemento E (cdr L)))
    )
)

(defun elimina(E L)
    (cond   ((null L) ())
            ((equal E (car L)) (elimina E (cdr L)))
            (t (append (list (car L)) (elimina E (cdr L))))
    )
)

(defun montaPares(L)
    (cond   ((null L) ())
            (t (append (list (append (list (car L)) (list (contaElemento (car L) L)))) (montaPares (elimina (car L) L))))
    )
)



;; funcoes do segundo exercicio

(defun contaConsec(E L)
    (cond   ((null L) 0)
            ((equal E (car(cdr L))) (+ 1 (contaConsec E (cdr L))))
            ((not(equal E (car(cdr L)))) 1)
    )
)

(defun removeConsec(L)
    (cond   ((null L) ())
            ((equal (car L) (car(cdr L))) (removeConsec (cdr L)))
            (t (cdr L))
    )
)

(defun montaParesConsec(L)
    (cond   ((null L) ())
            (t (append (list (append (list (contaConsec (car L) L)) (list (car L)))) (montaParesConsec (removeConsec L))))
    )
)



;; chamadas para executar o programa

(write-line "Exercicio 1 utilizando a lista L = (a b z x 4.6 (a x) () (5 z x) ()):")
(print
    (montaPares (desparentiza '(a b z x 4.6 (a x) () (5 z x) ())))
)

(write-line "")
(write-line "")
(write-line "")
(write-line "")

(write-line "Exercicio 2 utilizando a lista L = (a a a a b c c a a d e e e e):")
(print
    (montaParesConsec (desparentiza '(a a a a b c c a a d e e e e)))
)
