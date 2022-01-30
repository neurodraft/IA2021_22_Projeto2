(let 
    (
        (melhor-jogada nil)
        (melhor-valor nil)
        (nos-analisados 0)
        (cortes-alfa 0)
        (cortes-beta 0)
        (limite-tempo nil)
        (real-time-inicio nil)
        (limite-tempo-alcancado nil)
    )
    (defun alfabeta (no f-sucessores f-utilidade profundidade &optional (alfa -999999999999) (beta 999999999999) (jogador-max t) (inicio t))
    (labels ((valor-alfa (sucessores alfa &optional (valor -999999999999) )
                (cond
                ((null sucessores) valor)
                (t
                    (let* ((novo-valor (max valor (alfabeta (car sucessores) f-sucessores f-utilidade (1- profundidade) alfa beta nil nil)))
                        (novo-alfa (max alfa novo-valor)))
                    (if (and inicio (> novo-valor valor)) (setf melhor-jogada (car sucessores)) (setf melhor-valor novo-valor))
                    (cond
                    ((>= novo-valor beta) (setf cortes-alfa (1+ cortes-alfa)) novo-valor )
                    (t (valor-alfa (cdr sucessores) novo-alfa novo-valor)))))))
            (valor-beta (sucessores beta &optional (valor 999999999999))
                (cond
                ((null sucessores) valor)
                (t
                    (let* ((novo-valor (min valor (alfabeta (car sucessores) f-sucessores f-utilidade (1- profundidade) alfa beta t nil)))
                        (novo-beta (min beta novo-valor)))
                    (cond
                    ((<= novo-valor alfa) (setf cortes-beta (1+ cortes-beta)) novo-valor )
                    (t (valor-beta (cdr sucessores) novo-beta novo-valor))))))))
        (progn
        (setf nos-analisados (1+ nos-analisados))
        (cond
        ((and (not (null limite-tempo)) (null real-time-inicio)) (setf real-time-inicio (get-internal-real-time)) (alfabeta no f-sucessores f-utilidade profundidade))
        ((and (not (null limite-tempo)) (not (null real-time-inicio)) (> (- (get-internal-real-time) real-time-inicio) (- limite-tempo 5))) (setf limite-tempo-alcancado t) melhor-valor)
        ((and inicio (zerop profundidade)) nil)
        ((zerop profundidade) (funcall f-utilidade no))
        (t (let ((sucessores (funcall f-sucessores no jogador-max)))
            (cond
            ((and inicio (null sucessores)) nil)
            ((null sucessores) (funcall f-utilidade no) )
            (jogador-max (valor-alfa sucessores alfa))
            (t (valor-beta sucessores beta))
            )))))))
    (defun obter-melhor-jogada ()
        melhor-jogada
    )
    (defun obter-melhor-valor ()
        melhor-valor
    )
    (defun obter-nos-analisados ()
        nos-analisados
    )
    (defun obter-cortes-alfa ()
        cortes-alfa
    )
    (defun obter-cortes-beta ()
        cortes-beta
    )
    (defun obter-limite-tempo-alcancado ()
        limite-tempo-alcancado
    )
    (defun reiniciar-valores ()
        (progn 
            (setf melhor-jogada nil)
            (setf melhor-valor nil)
            (setf nos-analisados 0)
            (setf cortes-alfa 0)
            (setf cortes-beta 0)
            (setf limite-tempo nil)
            (setf real-time-inicio nil)
            (setf limite-tempo-alcancado nil)
        )
    )
    (defun definir-limite-tempo (milisegundos)
        (setf limite-tempo milisegundos)
    )
)