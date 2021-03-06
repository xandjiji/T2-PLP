(write-line "TRABALHO T2 DE LISP")
(write-line "Alunos: Thiago Bretas de Souza(551899) e Alexandre Regali Seleghim(551473)")


(defun desparentiza(L)
       (cond ((null L) ())
       ((atom (car L)) (append (list (car L)) (desparentiza (cdr L))))
       (t(append (desparentiza(car L)) (desparentiza(cdr L))))))
       

(defun contaElemento(a L)
       (cond ((null L) 0)
             ((= a (car L)) (+ 1 (contaElemento a (cdr L))))
             (t(contaElemento a (cdr L)))))
             
(defun pertence(a L)
       (cond ((null L) nil)
             ((= a (car L)) t)
             (t(pertence a (cdr L)))))
             


(defun intersec (L1 L2)
       (cond ((null L1) nil)
             ((null L2) nil)
             ((pertence (car L1) L2) (cons (car L1) (intersec (cdr L1) L2)))
             (t(intersec(cdr L1) L2))))
             


(defun elimina(e L)
       (cond ((null L) nil)
             ((= e (car L)) (elimina e (cdr L)))
             (t(cons (car L) (elimina e (cdr L))))))


             
(defun monta(L)
       (cond ((null L) nil)
             (t(cons (list (car L) (contaElemento (car L) L)) (monta (elimina (car L) (cdr L)))))))

(defun listaFinal (L1 L2)
       (setq L (intersec L1 L2))
       (setq LAux(intersec L2 L1))
       (setq LF(append L Laux))
       (write(monta LF)))

(setq x (list 1 2 2 4 (list 6 6 2)))
(setq y (list 3 2 4 5 6))
(write-line "EXEMPLO 1")
(write-line "\n")
(write-line "Lista 1: ")
(write x)
(write-line "\n")
(write-line "Lista 1 desparentizada: ")
(write(desparentiza x))
(write-line "\n")
(write-line "Lista 2: ")
(write y)
(write-line "\n")
(write-line "Lista Final: ")
(listaFinal (desparentiza x) y)
(write-line "\n")
(setq x (list 2 3 5 2 (list 3 3 5)))
(setq y (list 1 3 5 2 4 3 6))
(write-line "EXEMPLO 2")
(write-line "\n")
(write-line "Lista 1: ")
(write x)
(write-line "\n")
(write-line "Lista 1 desparentizada: ")
(write(desparentiza x))
(write-line "\n")
(write-line "Lista 2: ")
(write y)
(write-line "\n")
(write-line "Lista Final: ")
(listaFinal (desparentiza x) y)
