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
        (memo (make-hash-table :test 'equal))
        (memoizacao-ativada 0)
        (jogador-proprio 1)
    )
    (defun alfabeta (no f-sucessores f-utilidade profundidade &optional (alfa -999999999999) (beta 999999999999) (jogador-max t) (inicio t))
    "Implementa��o recursiva do algoritmo Minimax com cortes alfabeta fail-soft e memoiza��o. Recebendo um n�,
    uma fun��o de gera��o de sucessores (que dever� receber um n� e se o jogador � maximizante
    e retornar a lista de todos os n�s sucessores), uma fun��o de avalia��o de utilidade
    (que dever� receber um n� e retornar o valor de utilidade relativo ao estado) e
    uma profundidade m�xima de an�lise, retorna o valor m�ximo correspondente � utilidade do
    melhor sucessor imediato encontrado. � tambem necess�rio reiniciar os valores da closure com a fun��o
    reiniciar-valores e definir o jogador atrav�s da fun��o definir-jogador-proprio (1 por defeito) antes de iniciar
    o algoritmo. Adicionalmente � poss�vel definir um limite de tempo utilizando a fun��o definir-limite-tempo.
    S�o tamb�m disponibilizadas as fun��es para obter a melhor jogada e dados estat�sticos."
    (labels ((valor-alfa (sucessores alfa &optional (valor -999999999999) )
                (cond
                ((null sucessores) valor)
                (t
                    (let* ((novo-valor (max valor (alfabeta (car sucessores) f-sucessores f-utilidade (1- profundidade) alfa beta nil nil)))
                        (novo-alfa (max alfa novo-valor)))
                    ; Se topo da arvore e se o valor m�ximo foi superado ent�o foi encontrada uma nova melhor jogada
                    (if (and inicio (> novo-valor valor)) (setf melhor-jogada (car sucessores)) (setf melhor-valor novo-valor))
                    (cond
                    ; Se o valor m�ximo � superior ou igual a beta ent�o � corte-alfa e retornado o valor sem continuar a an�lise dos sucessores (fail soft)
                    ((>= novo-valor beta) (setf cortes-alfa (1+ cortes-alfa)) novo-valor )
                    ; Continuar a an�lise dos sucessores com novo alfa e valor m�ximo
                    (t (valor-alfa (cdr sucessores) novo-alfa novo-valor)))))))
            (valor-beta (sucessores beta &optional (valor 999999999999))
                (cond
                ((null sucessores) valor)
                (t
                    (let* ((novo-valor (min valor (alfabeta (car sucessores) f-sucessores f-utilidade (1- profundidade) alfa beta t nil)))
                        (novo-beta (min beta novo-valor)))
                    (cond
                    ; Se o valor m�nimo � inferior ou igual a alfa ent�o � corte-beta e retornado o valor sem continuar a an�lise dos sucessores (fail soft)
                    ((<= novo-valor alfa) (setf cortes-beta (1+ cortes-beta)) novo-valor )
                    ; Continuar a an�lise dos sucessores com novo beta e valor minimo
                    (t (valor-beta (cdr sucessores) novo-beta novo-valor)))))))
            (get-stored ()
                (gethash (list (first no) profundidade jogador-proprio jogador-max) memo)
            )        
            (store-memo (value)
                (setf (gethash (list (first no) profundidade jogador-proprio jogador-max) memo) value)
            )
            )
        
        (progn
        (setf nos-analisados (1+ nos-analisados))
        (cond
            ; Se existe um limite de tempo mas ainda n�o foi registado o inicio da execu��o
            ((and (not (null limite-tempo)) (null real-time-inicio))
                ; Definir o tempo de inicio e voltar a chamar a fun��o com os mesmos valores
                (setf real-time-inicio (get-internal-real-time)) (alfabeta no f-sucessores f-utilidade profundidade))
            ; Se existe um limite de tempo, o tempo inicial est� definido e se o tempo restante � menor a 5 milisegundos
            ((and (not (null limite-tempo)) (not (null real-time-inicio)) (> (- (get-internal-real-time) real-time-inicio) (- limite-tempo 5)))
                ; Terminar e devolver o resultado
                (setf limite-tempo-alcancado t) melhor-valor)
            ; Se for o topo da �rvore e a profundidade for 0 devolver nil
            ((and inicio (zerop profundidade)) nil)
            (t (let ((stored (get-stored)))
                (cond 
                    ; Se existe um valor na tabela e n�o � o topo da �rvore devolver esse valor
                    ((and stored (not inicio)) (setf memoizacao-ativada (1+ memoizacao-ativada)) stored )
                    ; Se a profundidade for 0 devolver a avalia��o de utilidade do no
                    ((zerop profundidade) (funcall f-utilidade no))
                    (t (let ((sucessores (funcall f-sucessores no jogador-max)))
                        (cond
                            ; Se for o topo da �rvore e n�o existirem sucessores devolver nil
                            ((and inicio (null sucessores)) nil)
                            ; Se simplesmente n�o existirem sucessores devolver utilidade
                            ((null sucessores) (let ((utilidade (funcall f-utilidade no)))
                                ; Guardar valor na tabela de memoiza��o
                                (if (not stored) (store-memo utilidade))
                                utilidade))
                            ; Em �ltimo caso, percorrer recursivamente os sucessores para determinar o valor do n�
                            (t (let ((valor (if jogador-max (valor-alfa sucessores alfa) (valor-beta sucessores beta))))
                                ; Guardar valor na tabela de memoiza��o
                                (if (not stored) (store-memo valor))
                                valor)))))))))
        )
    ))
    (defun obter-melhor-jogada ()
        "Permite obter o melhor sucessor encontrado na �ltima execu��o do algoritmo."
        melhor-jogada
    )
    (defun obter-melhor-valor ()
        "Permite obter o melhor valor encontrado na �ltima execu��o do algoritmo."
        melhor-valor
    )
    (defun obter-nos-analisados ()
        "Permite obter o total de n�s analisados na �ltima execu��o do algoritmo."
        nos-analisados
    )
    (defun obter-cortes-alfa ()
        "Permite obter o total de cortes alfa efetuados na �ltima execu��o do algoritmo."
        cortes-alfa
    )
    (defun obter-cortes-beta ()
        "Permite obter o total de cortes beta efetuados na �ltima execu��o do algoritmo."
        cortes-beta
    )
    (defun obter-limite-tempo-alcancado ()
        "Permite determinar se a �ltima execu��o do algoritmo foi interrompida devido
        a limite de tempo."
        limite-tempo-alcancado
    )
    (defun obter-memoizacao-ativada ()
        "Permite obter o total de utiliza��es de valores presentes na tabela de memoiza��o
        durante a execu��o do algoritmo."
        memoizacao-ativada
    )
    (defun reiniciar-valores ()
        "Permite reiniciar os valores da closure associada ao algoritmo."
        (progn 
            (setf melhor-jogada nil)
            (setf melhor-valor nil)
            (setf nos-analisados 0)
            (setf cortes-alfa 0)
            (setf cortes-beta 0)
            (setf limite-tempo nil)
            (setf real-time-inicio nil)
            (setf limite-tempo-alcancado nil)
            (setf memoizacao-ativada 0)
        )
    )
    (defun definir-limite-tempo (milisegundos)
        "Permite definir o limite de tempo para a execu��o do algoritmo."
        (setf limite-tempo milisegundos)
    )

    (defun definir-jogador-proprio (jogador)
    "Definir qual o n�mero de jogador. Por defeito assume valor 1. "
        (setf jogador-proprio jogador)
    )

    (defun limpar-memoizacao ()
    "Permite limpar a tabela de memoiza��o."
        (setf memo (make-hash-table :test 'equal))
    )
)

;; IMPLEMENTA��O SEM MEMOIZA��O
;
; (let 
;     (
;         (melhor-jogada nil)
;         (melhor-valor nil)
;         (nos-analisados 0)
;         (cortes-alfa 0)
;         (cortes-beta 0)
;         (limite-tempo nil)
;         (real-time-inicio nil)
;         (limite-tempo-alcancado nil)
;     )
;     (defun alfabeta (no f-sucessores f-utilidade profundidade &optional (alfa -999999999999) (beta 999999999999) (jogador-max t) (inicio t))
;     (labels ((valor-alfa (sucessores alfa &optional (valor -999999999999) )
;                 (cond
;                 ((null sucessores) valor)
;                 (t
;                     (let* ((novo-valor (max valor (alfabeta (car sucessores) f-sucessores f-utilidade (1- profundidade) alfa beta nil nil)))
;                         (novo-alfa (max alfa novo-valor)))
;                     (if (and inicio (> novo-valor valor)) (setf melhor-jogada (car sucessores)) (setf melhor-valor novo-valor))
;                     (cond
;                     ((>= novo-valor beta) (setf cortes-alfa (1+ cortes-alfa)) novo-valor )
;                     (t (valor-alfa (cdr sucessores) novo-alfa novo-valor)))))))
;             (valor-beta (sucessores beta &optional (valor 999999999999))
;                 (cond
;                 ((null sucessores) valor)
;                 (t
;                     (let* ((novo-valor (min valor (alfabeta (car sucessores) f-sucessores f-utilidade (1- profundidade) alfa beta t nil)))
;                         (novo-beta (min beta novo-valor)))
;                     (cond
;                     ((<= novo-valor alfa) (setf cortes-beta (1+ cortes-beta)) novo-valor )
;                     (t (valor-beta (cdr sucessores) novo-beta novo-valor))))))))
;         (progn
;         (setf nos-analisados (1+ nos-analisados))
;         (cond
;         ((and (not (null limite-tempo)) (null real-time-inicio)) (setf real-time-inicio (get-internal-real-time)) (alfabeta no f-sucessores f-utilidade profundidade))
;         ((and (not (null limite-tempo)) (not (null real-time-inicio)) (> (- (get-internal-real-time) real-time-inicio) (- limite-tempo 5))) (setf limite-tempo-alcancado t) melhor-valor)
;         ((and inicio (zerop profundidade)) nil)
;         ((zerop profundidade) (funcall f-utilidade no))
;         (t (let ((sucessores (funcall f-sucessores no jogador-max)))
;             (cond
;             ((and inicio (null sucessores)) nil)
;             ((null sucessores) (funcall f-utilidade no) )
;             (jogador-max (valor-alfa sucessores alfa))
;             (t (valor-beta sucessores beta))
;             )))))))
;     (defun obter-melhor-jogada ()
;         melhor-jogada
;     )
;     (defun obter-melhor-valor ()
;         melhor-valor
;     )
;     (defun obter-nos-analisados ()
;         nos-analisados
;     )
;     (defun obter-cortes-alfa ()
;         cortes-alfa
;     )
;     (defun obter-cortes-beta ()
;         cortes-beta
;     )
;     (defun obter-limite-tempo-alcancado ()
;         limite-tempo-alcancado
;     )
;     (defun reiniciar-valores ()
;         (progn 
;             (setf melhor-jogada nil)
;             (setf melhor-valor nil)
;             (setf nos-analisados 0)
;             (setf cortes-alfa 0)
;             (setf cortes-beta 0)
;             (setf limite-tempo nil)
;             (setf real-time-inicio nil)
;             (setf limite-tempo-alcancado nil)
;         )
;     )
;     (defun definir-limite-tempo (milisegundos)
;         (setf limite-tempo milisegundos)
;     )
; )