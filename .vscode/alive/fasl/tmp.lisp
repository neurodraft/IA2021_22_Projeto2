(let 
    (
        (melhor-jogada nil)
    )
    (defun alfabeta (no f-sucessores f-utilidade profundidade &optional (alfa -999999999999) (beta 999999999999) (jogador-max t) (inicio t))
    (labels ((valor-alfa (sucessores alfa &optional (valor -999999999999) )
                (cond
                ((null sucessores) valor)
                (t
                    (let* ((novo-valor (max valor (alfabeta (car sucessores) f-sucessores f-utilidade (1- profundidade) alfa beta nil nil)))
                        (novo-alfa (max alfa novo-valor)))
                    (if (and inicio (> novo-valor valor)) (setf melhor-jogada (car sucessores)))
                    (cond
                    ((>= novo-valor beta) novo-valor)
                    (t (valor-alfa (cdr sucessores) novo-alfa novo-valor)))))))
            (valor-beta (sucessores beta &optional (valor 999999999999))
                (cond
                ((null sucessores) valor)
                (t
                    (let* ((novo-valor (min valor (alfabeta (car sucessores) f-sucessores f-utilidade (1- profundidade) alfa beta t nil)))
                        (novo-beta (min beta novo-valor)))
                    (cond
                    ((<= novo-valor alfa) novo-valor)
                    (t (valor-beta (cdr sucessores) novo-beta novo-valor))))))))
        (cond
        ((zerop profundidade) (funcall f-utilidade no))
        (t (let ((sucessores (funcall f-sucessores no jogador-max)))
            (cond
            ((null sucessores) (funcall f-utilidade no))
            (jogador-max (valor-alfa sucessores alfa))
            (t (valor-beta sucessores beta))
            ))))))
    (defun obter-melhor-jogada ()
        melhor-jogada
    )
    (defun limpar-melhor-jogada ()
        (setf melhor-jogada nil)
    )
)