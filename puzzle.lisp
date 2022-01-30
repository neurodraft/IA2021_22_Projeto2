(defun no-inicial ()
    (list (estado-inicial) nil)
)

(defun estado-inicial ()
    (list (tabuleiro-vazio) (pecas-iniciais) (pecas-iniciais))
)

(defun criar-estado (tabuleiro pecas-jogador-1 pecas-jogador-2)
    (list tabuleiro pecas-jogador-1 pecas-jogador-2)
)

(defun criar-no (estado ultima-jogada)
    (list estado ultima-jogada)
)

(defun no-estado (no)
    (first no)
)

(defun no-jogada (no)
    (second no)
)

(defun estado-tabuleiro (estado)
    (first estado)
)

(defun estado-pecas-jogador (estado jogador)
    (nth jogador estado)
)

(defun peca-offset-hotspot (peca)
    (third peca)
)

(defun tabuleiro-vazio () 
    '(
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0 0 0 0 0 0))
)

(defun pecas-iniciais ()
    '(10 10 15)
)

(defun peca-c-h ()
    "Peça C horizontal descrita como uma matriz 3x2 e uma lista de 4 deslocações
    cada deslocação é descrita com uma lista de direcões diagonais de contato e um offset em x e y
    da forma da peça relativamente a posicao no tabuleiro"
    '(
        (
            (0 1 1)
            (1 1 0)
        )
        (
            ((sup-esq) (-1 1))
            ((sup-dir inf-dir) (-2 1))
            ((inf-dir) (-1 0))
            ((inf-esq sup-esq) (0 0))
        )
        (0 -1)
    )
)

(defun peca-c-v ()
    "Peça C vertical descrita como uma matriz 2x3 e uma lista de 4 deslocações
    cada deslocação é descrita com uma lista de direcões diagonais de contato e um offset em x e y
    da forma da peça relativamente a posicao no tabuleiro"
    '(
        (
            (1 0)
            (1 1)
            (0 1)
        )
        (
            ((sup-dir) (-1 -1))
            ((inf-dir inf-esq) (-1 -2))
            ((inf-esq) (0 -1))
            ((sup-esq sup-dir) (0 0))
        )
        (0 0)
    )
)

(defun peca-a ()
    "Peça A descrita como uma matriz 1x1 e uma lista de 1 deslocações
    cada deslocação é descrita com uma lista de direcões diagonais de contato e um offset em x e y
    da forma da peça relativamente a posicao no tabuleiro"
    '(
        (
            (1)
        )
        (
            ((sup-dir inf-dir inf-esq sup-esq) (0 0))
        )
        (0 0)
    )
)

(defun peca-b ()
    "Peça B descrita como uma matriz 2x2 e uma lista de 4 deslocações
    cada deslocação é descrita com uma lista de direcões diagonais de contato e um offset em x e y
    da forma da peça relativamente a posicao no tabuleiro"
    '(
        (
            (1 1)
            (1 1)
        )
        (
            ((sup-dir) (-1 0))
            ((inf-dir) (-1 -1))
            ((inf-esq) (0 -1))
            ((sup-esq) (0 0))
        )
        (0 0)
    )
)

(defun operadores ()
    "Devolve os simbolos relativos às 4 peças concretas do jogo"
    (list 'peca-a  'peca-b 'peca-c-h 'peca-c-v)
)

;;; Métodos auxiliares

(defun celula (row col tabuleiro)
    "Retorna uma celula na linha e coluna do tabuleiro"
  (nth col (nth row tabuleiro)))

(defun substituir-posicao (idx line &optional (value 1))
    "Substitui a célula na posição idx da linha recebida pelo valor"
  (labels ((recursive (current)
              (cond ((null (nth current line)) nil)
                    ((= current idx) (cons value (recursive (1+ current))))
                    (t (cons (nth current line) (recursive (1+ current)))))))
    (recursive 0))
)

(defun substituir (row col tabuleiro &optional (value 1))
    "Substitui a célula na posição row col recebida pelo valor"
  (labels ((recursive (current)
              (cond ((null (nth current tabuleiro)) nil)
                    ((= current row) (cons (substituir-posicao col (nth current tabuleiro) value) (recursive (1+ current))))
                    (t (cons (nth current tabuleiro) (recursive (1+ current)))))))
    (recursive 0))
)

(defun list-0-to-n (n)
    "Devolve uma lista de 0 a n"
    (cond 
        ((< n 0) nil)
        (t (append (list-0-to-n (1- n)) (list n))) 
    )
)

(defun remove-from-list (l index &optional (i 0))
  "Remove da lista l o elemento de indice index, devolvendo uma lista de dimensão (1- (length l))"
    (cond 
        ((= i index) (cdr l))
        (t (cons (car l) (remove-from-list (cdr l) index (1+ i))))
    )
)

(defun shuffle-list (l &optional (shuffled-list nil) (indexes nil) (init nil))
    "Baralha a lista l aleatóriamente"
    (cond
        ((null init) (shuffle-list l shuffled-list (list-0-to-n (1- (length l))) t ))
        ((null indexes) shuffled-list)
        (t
            (let* ((random-n (random (length indexes)))
                    (random-index (nth random-n indexes)))

                (shuffle-list l (cons (nth random-index l) shuffled-list) (remove-from-list indexes random-n) init)        
            ) 
        )
    )
)

(defun eliminar-duplicados (L)
    "Elimina elementos duplicados numa lista L"
  (cond ((null L) L)
        ((member (car L) (cdr L) :test #'equal)
         (eliminar-duplicados (cdr L)))
        (t (cons (car L) (eliminar-duplicados (cdr L))))))



(defun obter-vizinhanca (tabuleiro x y)
    "Obtem uma matriz 3x3 que representa a vizinhança de uma célula no tabuleiro
    Representa espaços fora do tabuleiro com o valor -1"
    (labels (
        (recursive (tabuleiro i)
            (cond 
                ((null tabuleiro) nil)
                ((and (listp (car tabuleiro)) (> (abs (- i y)) 1)) (cons nil (recursive (cdr tabuleiro) (1+ i))))
                ((and (listp (car tabuleiro)) (<= (abs (- i y)) 1)) (cons  (recursive (car tabuleiro) 0) (recursive (cdr tabuleiro) (1+ i))))
                ((null (car tabuleiro)) nil)
                (t 
                    (cond
                        ((<= (abs (- i x)) 1) (cons (car tabuleiro) (recursive (cdr tabuleiro) (1+ i))))
                        (t (cons nil (recursive (cdr tabuleiro) (1+ i))))
                    )
                )
            )
        )
    ) 
    (let* 
        (
            (obtidos (mapcar (lambda (linha) (remove nil linha)) (remove nil (recursive tabuleiro 0) )))
            (obtidos-colunas-corrigidas
                (cond 
                    ((= (length (car obtidos)) 2)
                        (cond
                            ((= x 0) (mapcar (lambda (linha)
                                (cons -1 linha)
                            ) obtidos))
                            (t (mapcar (lambda (linha) 
                                (append linha '(-1))
                            ) obtidos))
                        )
                    )
                    (t obtidos)
                )
            )
            (obtidos-linhas-corrigidas
                (cond
                    ((= (length obtidos-colunas-corrigidas) 2) 
                        (cond
                            ((= y 0) (cons '(-1 -1 -1) obtidos-colunas-corrigidas))
                            (t (append obtidos-colunas-corrigidas '((-1 -1 -1))))
                        )
                    )
                    (t obtidos-colunas-corrigidas)
                )
            )
        )
        obtidos-linhas-corrigidas
    )
    ) 
)

(defun espacos-validos (tabuleiro jogador)
    "Procura espaços validos para jogar no tabuleiro
    Devolve lista de listas com par de coordenadas e lista de direções diagonais de contacto
    (sup-esq, sup-dir, inf-esq, inf-dir)"
    (labels
        (
            (recursive (_tabuleiro x y)
                (cond
                    ((null _tabuleiro) nil)
                    ((listp (car _tabuleiro)) (append  (recursive (car _tabuleiro) 0 y) (recursive (cdr _tabuleiro) 0 (1+ y))))
                    ((null (car _tabuleiro)) nil)
                    (t 
                        (cond 
                            ((/= (car _tabuleiro) 0) (append nil (recursive (cdr _tabuleiro) (1+ x) y)))
                            (t 
                                (let*
                                    (
                                        (vizinhanca (obter-vizinhanca tabuleiro x y))
                                        ;; Verificar se não existem peças colocadas nas laterais
                                        ;; e existe pelo menos uma peça nas diagonais
                                        (decisao (and 
                                            (not (or 
                                                (= (second (first vizinhanca)) jogador)
                                                (= (first (second vizinhanca)) jogador)
                                                (= (third (second vizinhanca)) jogador)
                                                (= (second (third vizinhanca)) jogador)
                                                ))
                                            (or
                                                (= (first (first vizinhanca)) jogador)
                                                (= (third (first vizinhanca)) jogador)
                                                (= (first (third vizinhanca)) jogador)
                                                (= (third (third vizinhanca)) jogador)
                                            )
                                            ))
                                        (direcoes-de-contato
                                            (remove nil (cons (if (= (first (first vizinhanca)) jogador) 'sup-esq nil) 
                                                (cons (if (= (third (first vizinhanca)) jogador) 'sup-dir nil) 
                                                    (cons (if (= (first (third vizinhanca)) jogador) 'inf-esq nil) 
                                                        (cons (if (= (third (third vizinhanca)) jogador) 'inf-dir nil) nil)))))
                                        )
                                    ) 
                                    (append (if decisao (list (list (list x y) direcoes-de-contato)) nil) (recursive (cdr _tabuleiro) (1+ x) y))
                                )
                            )
                        )
                    
                    )
                )
            )
        )
        (cond
            ((tabuleiro-vaziop tabuleiro jogador)
                (cond
                    ((= jogador 1) '(((0 0) (sup-esq))))
                    ((= jogador 2) '(((13 13) (inf-dir))))
                )
            )
            (t (recursive tabuleiro 0 0))
        )
        
    )
)

(defun tabuleiro-vaziop (tabuleiro &optional (jogador 1))
    "Função que avlia se um tabuleiro fornecido não tem peças colocadas pelo jogador"
    (cond
        ((null tabuleiro) t)
        ((listp (car tabuleiro)) (and (tabuleiro-vaziop (car tabuleiro) jogador) (tabuleiro-vaziop (cdr tabuleiro) jogador)))
        ((/= (car tabuleiro) jogador) (and t (tabuleiro-vaziop (cdr tabuleiro) jogador)))
        (t nil)
    )
)

(defun potenciais-colocacoes-com-peca ( posicoes peca)
    "Obtem uma lista de potenciais colocações da peça no tabuleiro
    que deve ainda ser testada na prática"
    (eliminar-duplicados (apply #'append (mapcar (lambda (posicao)
             (remove nil (potenciais-colocacoes  posicao (deslocacoes-peca peca)))
        )
     posicoes)))
)

(defun potenciais-colocacoes (posicao deslocacoes)
    "Devolve a resolução em colocações concretas da peça ao comparar a lista de deslocacoes da peça
    com os contatos da posicao recebida."
    (cond
        ((null deslocacoes) nil)
        ((lista-contem-todos (first (car deslocacoes)) (second posicao)) (let 
            (
                (x (+ (first (first posicao)) (first (second (car deslocacoes)))))
                (y (+ (second (first posicao)) (second (second (car deslocacoes)))))
            )
            (cond
                ((or (< x 0) (< y 0)) (cons nil (potenciais-colocacoes posicao (cdr deslocacoes))))
                (t (cons (list x y) (potenciais-colocacoes posicao (cdr deslocacoes))))
            )
        ))
        (t (cons nil (potenciais-colocacoes posicao (cdr deslocacoes))))
    )
)

(defun lista-contem-todos (lista elementos)
    "Verifica se a lista recebida contém todos os elementos"
    (cond
        ((null elementos) t)
        ((member (car elementos) lista) (and t (lista-contem-todos  lista (cdr elementos))))
        (t nil)
    )
)

(defun deslocacoes-peca (peca)
    "Obtem a lista de deslocações (offsets) da peça relativamente aos pontos de contacto"
    (car (cdr peca))
)

(defun potenciais-colocacoes-por-peca (estado operadores jogador)
    "Obtém todas as potenciais colocações por peça no tabuleiro considerando as peças restantes na mão.
    Todas as colocações devolvidas devem primeiro ser testadas em prática.
    Devolve lista de listas com operador e uma lista de potencias colocações em listas de coordenas x y."
    (let
        (
            (posicoes-validas (espacos-validos (first estado) jogador))
        )
        (mapcar (lambda (operador)
            (cond
                ((tem-peca operador (estado-pecas-jogador estado jogador)) (list operador (potenciais-colocacoes-com-peca posicoes-validas (funcall operador))))
                (t nil)            
            )
            
        ) operadores)
    )
)

(defun tem-peca (peca mao)
    "Valida se uma peça existe na mão do jogador"
    (cond
        ((equal peca 'peca-a) (> (first mao) 0))
        ((equal peca 'peca-b) (> (second mao) 0))
        ((or (equal peca 'peca-c-h) (equal peca 'peca-c-v)) (> (third mao) 0))
        (t nil)
    )
)

(defun peca-casas-ocupadas (x y peca)
    "Retorna uma lista de listas de coordenas que são as casa ocupadas concretamente de jogar
    a peça nas posições x y"
  (labels
    (
      (recursivo (matriz-peca i j offset)
        (cond
          ((null (car matriz-peca)) nil)
          ((listp (car matriz-peca)) (append (recursivo (car matriz-peca) 0 j offset) (recursivo (cdr matriz-peca) 0 (1+ j) offset)))
          (t 
            (cond
              ((= (car matriz-peca) 1) (cons (list (+ x i (first offset)) (+ y j (second offset))) (recursivo (cdr matriz-peca) (1+ i) j offset)) )
              (t (cons nil (recursivo (cdr matriz-peca) (1+ i) j offset)))
            )
          )
        )
      )
    )
    (remove nil (recursivo (first peca) 0 0 (peca-offset-hotspot peca)))
  )
)


(defun valida-casas (tabuleiro casas jogador &optional (primeira-jogada nil) (contato-diagonal nil))
    "Valida se é possível jogar no tabuleiro nas casas"
    (cond
        ((null casas) (or primeira-jogada contato-diagonal))
        ((or (> (first (car casas)) 13) (> (second (car casas)) 13)
            (< (first (car casas)) 0) (< (second (car casas)) 0)
        ) nil)
        (t 
            (let ((vizinhanca (obter-vizinhanca tabuleiro (first (car casas)) (second (car casas)))))
                (and
                    (and (= (second (second vizinhanca)) 0)
                        (not (or 
                            (= (second (first vizinhanca)) jogador)
                            (= (first (second vizinhanca)) jogador)
                            (= (third (second vizinhanca)) jogador)
                            (= (second (third vizinhanca)) jogador)
                        ))
                    )
                (valida-casas tabuleiro (cdr casas) jogador
                    (if (not primeira-jogada) (or (and (= jogador 1) (equal (car casas) '(0 0)))
                                                    (and (= jogador 2) (equal (car casas) '(13 13)))) t)
                    (if (not contato-diagonal) (or 
                            (= (first (first vizinhanca)) jogador)
                            (= (first (third vizinhanca)) jogador)
                            (= (third (first vizinhanca)) jogador)
                            (= (third (third vizinhanca)) jogador)
                        ) t))
                )
            )
        )
    )
)

(defun atualizar-mao (mao peca-jogada)
    "Devolve uma nova mão sem a peça jogada"
    (cond
        ((equal peca-jogada 'peca-a) (list (1- (first mao)) (second mao) (third mao)))
        ((equal peca-jogada 'peca-b) (list (first mao) (1- (second mao)) (third mao)))
        ((or (equal peca-jogada 'peca-c-h) (equal peca-jogada 'peca-c-v)) (list (first mao) (second mao) (1- (third mao))))
    )
)

(defun ocupar-casas (tabuleiro casas jogador)
    "Imprime a peça sobre o tabuleiro, devolvendo um novo tabuleiro"
    (cond
        ((null casas) tabuleiro)
        (t (ocupar-casas (substituir (second (car casas)) (first (car casas)) tabuleiro jogador) (cdr casas) jogador))
    )
)



(defun criar-f-sucessores (jogador-proprio jogador-adversario)
    (lambda (no jogador-max) 
        (sucessores no (if jogador-max jogador-proprio jogador-adversario) (operadores))
    )
)


(defun sucessores (no jogador operadores)
    "Com base no nó e nos operadores disponíveis, devolve uma lista de sucessores válidos"
    (shuffle-list (apply #'append (mapcar (lambda (peca-colocacoes)  
        (remove nil (mapcar (lambda (colocacao)
            (let ((casas-ocupadas (peca-casas-ocupadas (first colocacao) (second colocacao) (funcall (first peca-colocacoes)))))
                (cond 
                    ((valida-casas (first (no-estado no)) casas-ocupadas jogador)
                        (let
                            (
                                (novo-estado (criar-novo-estado (no-estado no) casas-ocupadas (first peca-colocacoes) jogador))
                            )
                            (criar-no novo-estado (list (first peca-colocacoes) colocacao))
                        )
                    )
                        
                    (t nil)
                )
            )
        ) (second peca-colocacoes)))
    ) (potenciais-colocacoes-por-peca (no-estado no) operadores jogador))))
)

(defun criar-novo-estado (estado casas-a-ocupar peca jogador)
    (if (= jogador 1)
        (criar-estado (ocupar-casas (estado-tabuleiro estado) casas-a-ocupar jogador) (atualizar-mao (estado-pecas-jogador estado 1) peca) (estado-pecas-jogador estado 2))
        (criar-estado (ocupar-casas (estado-tabuleiro estado) casas-a-ocupar jogador) (estado-pecas-jogador estado 1) (atualizar-mao (estado-pecas-jogador estado 2) peca))
    )
)

(defun criar-f-utilidade (jogador-proprio jogador-adversario)
    (lambda (no)
        (-  (pontuacao (no-estado no) jogador-adversario) (pontuacao (no-estado no) jogador-proprio))
    )
)

(defun pontuacao (estado jogador)
    (let (
        (pecas (estado-pecas-jogador estado jogador))
        )
        (+ (first pecas) (* (second pecas) 4) (* (third pecas) 4))
    )
)