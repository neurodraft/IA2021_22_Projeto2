(defun alfabeta (no f-sucessores f-utilidade profundidade alfa beta jogador-max)
  (labels ((valor-alfa (sucessores alfa &optional (valor -999999999999))
              (cond
               ((null sucessores) valor)
               (t
                (let* ((novo-valor (max valor (alfabeta (car sucessores) f-sucessores f-utilidade (1- profundidade) alfa beta nil)))
                       (novo-alfa (max alfa novo-valor)))
                  (cond
                   ((>= novo-valor beta) novo-valor)
                   (t (valor-alfa (cdr sucessores) novo-alfa novo-valor)))))))
           (valor-beta (sucessores beta &optional (valor 999999999999))
              (cond
               ((null sucessores) valor)
               (t
                (let* ((novo-valor (min valor (alfabeta (car sucessores) f-sucessores f-utilidade (1- profundidade) alfa beta t)))
                       (novo-beta (min beta novo-valor)))
                  (cond
                   ((<= novo-valor alfa) novo-valor)
                   (t (valor-beta (cdr sucessores) novo-beta novo-valor))))))))
    (cond
     ((zerop profundidade) (funcall f-utilidade no))
     (t (let ((sucessores (funcall f-sucessores no)))
          (cond
           ((null sucessores) (funcall f-utilidade no))
           (jogador-max (valor-alfa sucessores alfa))
           (t (valor-beta sucessores beta))
          ))))))