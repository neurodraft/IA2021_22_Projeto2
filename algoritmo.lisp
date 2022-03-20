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
    "Implementação recursiva do algoritmo Minimax com cortes alfabeta fail-soft e memoização. Recebendo um nó,
    uma função de geração de sucessores (que deverá receber um nó e se o jogador é maximizante
    e retornar a lista de todos os nós sucessores), uma função de avaliação de utilidade
    (que deverá receber um nó e retornar o valor de utilidade relativo ao estado) e
    uma profundidade máxima de análise, retorna o valor máximo correspondente à utilidade do
    melhor sucessor imediato encontrado. É tambem necessário reiniciar os valores da closure com a função
    reiniciar-valores e definir o jogador através da função definir-jogador-proprio (1 por defeito) antes de iniciar
    o algoritmo. Adicionalmente é possível definir um limite de tempo utilizando a função definir-limite-tempo.
    São também disponibilizadas as funções para obter a melhor jogada e dados estatísticos."
    (labels ((valor-alfa (sucessores alfa &optional (valor -999999999999) )
                (cond
                ((null sucessores) valor)
                (t
                    (let* ((novo-valor (max valor (alfabeta (car sucessores) f-sucessores f-utilidade (1- profundidade) alfa beta nil nil)))
                        (novo-alfa (max alfa novo-valor)))
                    ; Se topo da arvore e se o valor máximo foi superado então foi encontrada uma nova melhor jogada
                    (if (and inicio (> novo-valor valor)) (setf melhor-jogada (car sucessores)) (setf melhor-valor novo-valor))
                    (cond
                    ; Se o valor máximo é superior ou igual a beta então é corte-alfa e retornado o valor sem continuar a análise dos sucessores (fail soft)
                    ((>= novo-valor beta) (setf cortes-alfa (1+ cortes-alfa)) novo-valor )
                    ; Continuar a análise dos sucessores com novo alfa e valor máximo
                    (t (valor-alfa (cdr sucessores) novo-alfa novo-valor)))))))
            (valor-beta (sucessores beta &optional (valor 999999999999))
                (cond
                ((null sucessores) valor)
                (t
                    (let* ((novo-valor (min valor (alfabeta (car sucessores) f-sucessores f-utilidade (1- profundidade) alfa beta t nil)))
                        (novo-beta (min beta novo-valor)))
                    (cond
                    ; Se o valor mínimo é inferior ou igual a alfa então é corte-beta e retornado o valor sem continuar a análise dos sucessores (fail soft)
                    ((<= novo-valor alfa) (setf cortes-beta (1+ cortes-beta)) novo-valor )
                    ; Continuar a análise dos sucessores com novo beta e valor minimo
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
            ; Se existe um limite de tempo mas ainda não foi registado o inicio da execução
            ((and (not (null limite-tempo)) (null real-time-inicio))
                ; Definir o tempo de inicio e voltar a chamar a função com os mesmos valores
                (setf real-time-inicio (get-internal-real-time)) (alfabeta no f-sucessores f-utilidade profundidade))
            ; Se existe um limite de tempo, o tempo inicial está definido e se o tempo restante é menor a 5 milisegundos
            ((and (not (null limite-tempo)) (not (null real-time-inicio)) (> (- (get-internal-real-time) real-time-inicio) (- limite-tempo 5)))
                ; Terminar e devolver o resultado
                (setf limite-tempo-alcancado t) melhor-valor)
            ; Se for o topo da árvore e a profundidade for 0 devolver nil
            ((and inicio (zerop profundidade)) nil)
            (t (let ((stored (get-stored)))
                (cond 
                    ; Se existe um valor na tabela e não é o topo da árvore devolver esse valor
                    ((and stored (not inicio)) (setf memoizacao-ativada (1+ memoizacao-ativada)) stored )
                    ; Se a profundidade for 0 devolver a avaliação de utilidade do no
                    ((zerop profundidade) (funcall f-utilidade no))
                    (t (let ((sucessores (funcall f-sucessores no jogador-max)))
                        (cond
                            ; Se for o topo da árvore e não existirem sucessores devolver nil
                            ((and inicio (null sucessores)) nil)
                            ; Se simplesmente não existirem sucessores devolver utilidade
                            ((null sucessores) (let ((utilidade (funcall f-utilidade no)))
                                ; Guardar valor na tabela de memoização
                                (if (not stored) (store-memo utilidade))
                                utilidade))
                            ; Em último caso, percorrer recursivamente os sucessores para determinar o valor do nó
                            (t (let ((valor (if jogador-max (valor-alfa sucessores alfa) (valor-beta sucessores beta))))
                                ; Guardar valor na tabela de memoização
                                (if (not stored) (store-memo valor))
                                valor)))))))))
        )
    ))
    (defun obter-melhor-jogada ()
        "Permite obter o melhor sucessor encontrado na última execução do algoritmo."
        melhor-jogada
    )
    (defun obter-melhor-valor ()
        "Permite obter o melhor valor encontrado na última execução do algoritmo."
        melhor-valor
    )
    (defun obter-nos-analisados ()
        "Permite obter o total de nós analisados na última execução do algoritmo."
        nos-analisados
    )
    (defun obter-cortes-alfa ()
        "Permite obter o total de cortes alfa efetuados na última execução do algoritmo."
        cortes-alfa
    )
    (defun obter-cortes-beta ()
        "Permite obter o total de cortes beta efetuados na última execução do algoritmo."
        cortes-beta
    )
    (defun obter-limite-tempo-alcancado ()
        "Permite determinar se a última execução do algoritmo foi interrompida devido
        a limite de tempo."
        limite-tempo-alcancado
    )
    (defun obter-memoizacao-ativada ()
        "Permite obter o total de utilizações de valores presentes na tabela de memoização
        durante a execução do algoritmo."
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
        "Permite definir o limite de tempo para a execução do algoritmo."
        (setf limite-tempo milisegundos)
    )

    (defun definir-jogador-proprio (jogador)
    "Definir qual o número de jogador. Por defeito assume valor 1. "
        (setf jogador-proprio jogador)
    )

    (defun limpar-memoizacao ()
    "Permite limpar a tabela de memoização."
        (setf memo (make-hash-table :test 'equal))
    )
)

;; IMPLEMENTAÇÂO SEM MEMOIZAÇÂO
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