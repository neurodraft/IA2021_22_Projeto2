# **Projeto Nº 2**: Manual Técnico

Inteligência Artificial - Escola Superior de Tecnologia de Setúbal
2021/2022

Prof. Joaquim Filipe
Eng. Filipe Mariano

Desenvolvido por:
Bernardo Serra Mota, nº 201900947

Frederico Alcaria, nº201701440

## **Introdução**

Este Manual Técnico é referente ao programa desenvolvido sobre o jogo de puzzle Blockus Duo. Neste segundo projeto, a aplicação desenvolvida na linguaguem LISP permite a um jogador humano competir contra uma inteligência-artificial ou duas dois jogadores artificiais competirem. Para a decisão de melhor jogada foi implementado o algoritmo MiniMax com cortes Alfa-beta e tabela de memoização.

Contém uma explicação geral do sistema desenvolvido, da representação do puzzle e implementação dos algoritmos, devidamente comentados. São listados e explicados os objetos que compõem o projeto, incluindo dados e procedimentos; identificação das limitações e opções técnicas. Apresentaremos uma análise critica dos resultados das execuções do programa, tendo em consideração as limitações do projeto.

## 1.**Arquitetura do Sistema**

### Módulos

O programa está dividido em 3 ficheiros *.lisp:

1.  **algoritmo.lisp** *-* implementação do algoritmo de escolha de melhor jogada Minimax com cortes alfa-beta e tabela de memoização de forma genérica;
2.  **puzzle.lisp** \- implementação das entidades, regras e funções do jogo, tal como funções heuristicas para o problema;
3.  **projeto.lisp** \- implementação de uma interface com utilizador em consola e funções de leitura e escrita do disco;

A compilação e execução do ficheiro **projeto.lisp** é suficiente para utilizar a aplicação porque os outros dois ficheiros são lidos da pasta fornecida pelo utilizador ao iniciar o programa. Esta pasta também é utilizada para a escrita do ficheiro log.dat.

O módulo projeto faz a ligação entre o puzzle e o módulo genérico do algoritmo.

## 2.**Entidades** e sua Implementação

Um nó é uma lista composta por:

- um estado
- a última jogada efetuada

O estado é representado como uma lista composta por:

- uma matriz 14x14 (lista de listas) de valores numéricos {0, 1, 2}
- uma lista com duas listas de 3 valores que representam o número de peças disponíveis para jogar de cada tipo de cada jogador

As peças são funções que devolvem uma lista com uma matriz das casas que a peça ocupa, uma lista de deslocações e um offset relativo ao hotspot (explicação mais detalhada em `4.Descrição das opções tomadas`).

### procura.lisp

| Símbolo | Argumentos | Descrição |
| --- | --- | --- |
| alfabeta | (no f-sucessores f-utilidade profundidade &optional (alfa -999999999999) (beta 999999999999) (jogador-max t) (inicio t)) | Implementação recursiva do algoritmo Minimax com cortes alfabeta fail-soft e memoização. Recebendo um nó, uma função de geração de sucessores (que deverá receber um nó e se o jogador é maximizante e retornar a lista de todos os nós sucessores), uma função de avaliação de utilidade (que deverá receber um nó e retornar o valor de utilidade relativo ao estado) e uma profundidade máxima de análise, retorna o valor máximo correspondente à utilidade do melhor sucessor imediato encontrado. É tambem necessário reiniciar os valores da closure com a função reiniciar-valores e definir o jogador através da função definir-jogador-proprio (1 por defeito) antes de iniciar o algoritmo. Adicionalmente é possível definir um limite de tempo utilizando a função definir-limite-tempo.<br>São também disponibilizadas as funções para obter a melhor jogada e dados estatísticos. |
| obter-melhor-jogada | ()  | Permite obter o melhor sucessor encontrado na última execução do algoritmo. |
| obter-melhor-valor | ()  | Permite obter o melhor valor encontrado na última execução do algoritmo. |
| obter-nos-analisados | ()  | Permite obter o total de nós analisados na última execução do algoritmo. |
| obter-cortes-alfa | ()  | Permite obter o total de cortes alfa efetuados na última execução do algoritmo. |
| obter-cortes-beta | ()  | Permite obter o total de cortes beta efetuados na última execução do algoritmo. |
| obter-limite-tempo-alcancado | ()  | Permite determinar se a última execução do algoritmo foi interrompida devido a limite de tempo. |
| obter-memoizacao-ativada | ()  | Permite obter o total de utilizações de valores presentes na tabela de memoização durante a execução do algoritmo. |
| reiniciar-valores | ()  | Permite reiniciar os valores da closure associada ao algoritmo. |
| definir-limite-tempo | (milisegundos) | Permite definir o limite de tempo para a execução do algoritmo. |
| definir-jogador-proprio | (jogador) | Definir qual o número de jogador. Por defeito assume valor 1. |
| limpar-memoizacao | ()  | Permite limpar a tabela de memoização. |

### puzzle.lisp

| Símbolo | Argumentos | Descrição |
| --- | --- | --- |
| no-inicial | ()  | Gera um nó inicial com um estado inicial de jogo Blockus Duo |
| estado-inicial | ()  | Gera um estado inicial do jogo Blockus Duo |
| criar-estado | (tabuleiro pecas-jogador-1 pecas-jogador-2) |     |
| criar-no | (estado ultima-jogada) |     |
| no-estado |     | Obtem o estado de um nó |
| no-jogada |     | Obtem a última jogada de um nó |
| estado-tabuleiro |     | Obtem o tabuleiro de um estado |
| estado-pecas-jogador |     | Obtem as peças do jogador especificado de um estado |
| peca-offset-hotspot |     | Obtem o offset relativo ao hotspot de uma peca |
| tabuleiro-vazio |     | Retorna um tabuleiro Blockus Duo vazio |
| pecas-iniciais |     | Retorna uma mão de jogo com todas as pecas |
| peca-c-h | ()  | Peça C horizontal descrita como uma matriz 3x2, uma lista de 4 deslocações e um offset relativo ao hotspot.<br>Cada deslocação é descrita com uma lista de direcões diagonais de contato e um offset em x e y da forma da peça relativamente a posicao no tabuleiro |
| peca-c-v | ()  | Peça C vertical descrita como uma matriz 2x3, uma lista de 4 deslocações e um offset relativo ao hotspot.<br>Cada deslocação é descrita com uma lista de direcões diagonais de contato e um offset em x e y da forma da peça relativamente a posicao no tabuleiro |
| peca-a | ()  | Peça A descrita como uma matriz 1x1, uma lista de 1 deslocações e um offset relativo ao hotspot.<br>Cada deslocação é descrita com uma lista de direcões diagonais de contato e um offset em x e y da forma da peça relativamente a posicao no tabuleiro. |
| peca-b | ()  | Peça B descrita como uma matriz 2x2, uma lista de 4 deslocações e um offset relativo ao hotspot.<br>Cada deslocação é descrita com uma lista de direcões diagonais de contato e um offset em x e y da forma da peça relativamente a posicao no tabuleiro |
| operadores | ()  | Devolve os simbolos relativos às 4 peças concretas do jogo |
| obter-vizinhanca | (tabuleiro x y) | Obtem uma matriz 3x3 que representa a vizinhança de uma célula no tabuleiro. Representa espaços fora do tabuleiro com o valor -1 |
| espacos-validos | (tabuleiro jogador) | Procura espaços validos para o jogador especificado jogar no tabuleiro. Devolve lista de listas com par de coordenadas e lista de direções diagonais de contacto (sup-esq, sup-dir, inf-esq, inf-dir) |
| tabuleiro-vaziop | (tabuleiro &optional (jogador 1)) | Função que avalia se um tabuleiro fornecido não tem peças colocadas pelo jogador (1 por defeito) |
| deslocacoes-peca | (peca) | Obtem a lista de deslocações (offsets) da peça relativamente aos pontos de contacto |
| eliminar-duplicados | (L) | Elimina elementos duplicados numa lista L |
| potenciais-colocacoes-com-peca | ( posicoes peca) | Obtem uma lista de potenciais colocações da peça no tabuleiro que deve ainda ser testada na prática |
| potenciais-colocacoes | (posicao deslocacoes) | Devolve a resolução em colocações concretas da peça ao comparar a lista de deslocacoes da peça com os contatos da posicao recebida. |
| lista-contem-todos | (lista elementos) | Verifica se a lista recebida contém todos os elementos |
| potenciais-colocacoes-por-peca | (estado operadores jogador) | Obtém todas as potenciais colocações por peça no tabuleiro considerando as peças restantes na mão do jogador específicado. Todas as colocações devolvidas devem primeiro ser testadas em prática. Devolve lista de listas com operador e uma lista de potencias colocações em listas de coordenas x y. |
| tem-peca | (peca mao) | Valida se uma peça existe na mão do jogador |
| peca-casas-ocupadas | (x y peca) | Retorna uma lista de listas de coordenas que são as casa ocupadas concretamente de jogar a peça nas posições x y |
| celula | (row col tabuleiro) | Retorna uma celula na linha e coluna do tabuleiro |
| substituir-posicao | (idx line &optional (value 1) | Substitui a célula na posição idx da linha recebida pelo valor |
| substituir | (row col tabuleiro &optional (value 1) | Substitui a célula na posição row col recebida pelo valor |
| valida-casas | (tabuleiro casas) | Valida se é possível ao jogador específicado jogar no tabuleiro nas casas |
| list-0-to-n | (n) | Devolve uma lista de 0 a n |
| remove-from-list | (l index &optional (i 0) | Remove da lista l o elemento de indice index, devolvendo uma lista de dimensão (1- (length l)) |
| shuffle-list | (l &optional (shuffled-list nil) | Baralha a lista l aleatóriamente |
| sucessores | (no jogador operadores) | Com base no nó, no jogador e nos operadores disponíveis, devolve uma lista de sucessores válidos. |
| atualizar-mao | (mao peca-jogada) | Devolve uma nova mão sem a peça jogada |
| ocupar-casas | (tabuleiro casas jogador) | Imprime a peça sobre o tabuleiro com o numero do jogado específicado, devolvendo um novo tabuleiro |
| criar-f-sucessores | (jogador-proprio jogador-adversario) | Criar uma função de geração de sucessores com o jogador e o adversário definidos em closure. |
| criar-novo-estado | (estado casas-a-ocupar peca jogador) | Cria um novo estado de Blockus Duo com um novo estado resultante da impressão das casas-a-ocupar sobre o tabuleiro do estado fornecido, uma mão atualizada sem a peça usada. |
| criar-f-utilidade | (jogador-proprio jogador-adversario) | Criar uma função de utilidade com o jogador e o adversário definidos em closure. |
| pontuacao | (estado jogador) | Retorna a pontuação do jogador no estado fornecido. |

## jogo.lisp

| Símbolo | Argumentos | Descrição |
| --- | --- | --- |
| iniciar | ()  | Função que inicia o programa |
| definir-pasta | ()  | Pede o path da localização do projeto e compila os ficheiros puzzle.lisp e procura.lisp |
| mostrar-menu-inicial | ()  | Imprime no listener o menu inicial |
| mostrar-menu-jogador | ()  | Imprime no listener o menu que permite ao jogador escolher qual o jogador que deseja ser |
| mostrar-menu-tempo-limite | ()  | Imprime no listener o menu que permite inserir o limite de tempo |
| menu-inicial | ()  | Chama a função mostrar-menu-inicial, lê o input do utilizador e redireciona para o menu respetivo |
| menu-escolher-jogador | ()  | Chama a função mostrar-menu-jogador, lê o input do utilizador e redireciona para o menu respetivo |
| menu-limite-tempo | (&optional jogador) | Chama a funçãoo mostrar-menu-tempo-limite e lê o input do utilizador |
| jogo-humano-vs-pc | (jogador-humano tempo-limite) | Função que gere o jogo entre o humano e o computador, gere as jogadas e alterna entre os jogadores |
| jogada-computador | (no jogador adversario profundidade tempo-limite) | Representa uma jogada do computador, chama o algortimo que possibilita a jogada |
| escolher-peca | ()  | Mostra as opções para escolher uma peça e lê o input do utilizador |
| escolher-linha | ()  | Pede ao utilizador para inserir a linha onde a jogada vai ser feita e lê o input do utilizador |
| escolher-coluna | ()  | Pede ao utilizador para inserir a coluna onde a jogada vai ser feita e lê o input do utilizador |
| jogada-humano | (no jogador) | Representa uma jogada por parte de um jogador humano, trata da interação com o utlizador para permitir realizar uma jogada |
| jogo-pc-vs-pc | (tempo-limite) | Função que gere o jogo entre o computador e o computador, gere as jogadas e alterna entre os jogadores |
| mostrar-jogador-passou | (jogador) | imprime no caso de o jogador passar a jogada |
| registar-jogador-passou | (jogador) | regista no log.dat no caso de o jogador passar a jogada |
| mostrar-estatisticas | (melhor-valor nos-analisados cortes-alfa cortes-beta limite-tempo-alcancado memoizacao-ativada) | imprime as estatisticas relativas a uma jogada feita pelo computador |
| mostrar-turno-jogador | (jogador) | imprime a imformação de qual o jogador que pertence o turno |
| registar-turno-jogador | (jogador) | regista a imformação de qual o jogador que pertence o turno |
| registar-estatisticas | (melhor-valor nos-analisados cortes-alfa cortes-beta limite-tempo-alcancado memoizacao-ativada) | regista as estatisticas relativas a uma jogada feita pelo computador |
| mostrar-no | (no) | Imprime no listener as informações do nó atual |
| mostrar-pontuacoes | (estado) | Imprime no listener as Pontuações |
| registar-pontuacoes | (estado) | regista as Pontuações no ficheiro log.dat |
| registar-cabecalho | (tipo tempo) | regista o cabeçalho no ficheiro log.dat |
| registar-no | (no) | Regista no ficheiro log.dat as informações do nó atual |
| tabuleiro-letras | (tabuleiro) | Percorre o tabuleiro e troca os números por símbolos |
| mostrar-tabuleiro | (tabuleiro) | Imprime no listener o estado do tabuleiro |
| registar-tabuleiro | (tabuleiro) | Regista no ficheiro log.dat o estado do tabuleiro |
| diretorio-resultados | ()  | Devolve o path para o ficheiro resultados.dat |
| profundidade-max-para-tempo | (milisegundos) | Ao receber o tempo retorna a profundidade a ser usada para esse tempo limite definido |

## 3.**Algoritmos** e sua Implementação

### Minimax com cortes alfa-beta

```CommonLisp
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
```

*Excerto 1 - Implementação do algoritmo minimax com cortes alfa-beta e tabela de memoização.*

O algoritmo de procura Minimax com cortes alfa-beta procura diminuir o número de nós que são avaliados na árvore de procura quando comparado a uma implementação base do Minimax. Com base nas implicações lógicas resultantes da alternância antre a seleção de um valor máximo e mínimo é possível escolher sub-árvores que podem não ser exploradas sem influenciar o valor obtido.

Foi implementado recursivamente com o auxilio de uma closure para preservar a melhor  jogada encontrada e todos os valores estatísticos relativos à execução. Opcionalmente também contém um limite de tempo que poderá ser definido previamente à execução e será tomado em consideração. Esta closure contém uma hash-table de memoização na versão abordada neste manual mas é também apresentada uma versão mais simples sem memoização.

Relativamente à limitação de tempo, o algoritmo será capaz de devolver o melhor resultado desde que seja capaz de obter o valor de pelo menos um nó no primeiro nível (próxima jogada) dentro do tempo disponível. Caso tal não aconteça devolve `nil`.

A implementação da memoização revelou-se mais complicada do que esperado:

- Devido ao mecanismo usado na closure para identificar a melhor jogada na construção dos sucessores, não foi possível usar memoização no primeiro nível da árvore de procura mas em todos os restante níveis é possível obter um valor para o nó se este existir na tabela;
- Para preservar a consistência dos valores e o seu contexto foi necessário representar a chave da tabela como uma lista do estado, a profundidade restante de pesquisa, o jogador atual e se é um nível max ou min
    - Se para o mesmo estado profundidades diferentes dessem match estes valores não seriam comparáveis (valores menores seriam menos informados, mesmo valores maiores podiam corresponder a uma paregem num nível min em vez de max ou vice-versa;
    - Inicialmente não se pretendia informar do algoritmo sobre a existência de jogadores (através de *lambda-over-lambda* nas funções de geração de sucessores e avaliação de utilidade) mas foi necessário incluir esta informação na chave pois o tabuleiro será "visto" de forma diferente consoante quem está a jogar;
    - o mesmo relativamente ao nível min/max.
- Desta forma é possível preservar indefinidamente a tabela de memoização sem conflitos que obriguem a sua reinicialização entre turnos.

## 4.**Descrição das opções tomadas**

Todas as optimizações apresentadas no manual técnico do projeto anterior  (Procura de Espaço de Estados) relativamente à identificação de jogadas válidas no tabuleiro mantêm-se mas é de notar a alteração necessária na definição do operador peça para respeitar o hotspot da peça agora introduzido:

Cada peça, para além de ser definida pelas casa que ocupa numa matriz de dimensão variável com espaços nao ocupados representados por 0 e ocupados po 1 e um offset desta matriz relativo ao hotspot para permitir imprimir a peça corretamente, tem também uma lista de deslocações em coordenadas X e Y para cada combinação de contatos diagonais, ou seja, o offtset aplicado ao inicio do ciclo de "impressão" da peça sobre o tabuleiro (já tendo em consideração o offset do hotspot) de forma a que a casa fique ocupada com uma extremidade da peça que corresponda à diagonal esperada.

```CommonLisp
(
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
```

*Excerto 2 - Representação da peça C horizontal no programa*

Muitas das funções de manipulação do estado de jogo implementadas no projeto anterior necessitaram nesta fase de serem refatorizadas de forma a suportarem a alternância entre jogadores.

Para lidar com o facto da função de geração de sucessores ter um comportamento distinto consoante quem é o jogador e quem é o adversário, foi necessário implementar uma função para criar a função objetivo (**criar-f-sucessores**) que opera como *lambda-over-lambda*, devolvendo uma função lamba que contém uma closure com o número do jogador e do adversário de forma a ser genérica face ao algoritmo alfabeta.

O mesmo sucede com a função de avaliação de utilizadade e foi também implementada uma função *lambda-over-lambda* **criar-f-utilidade**. Definimos como função de avaliação de utilidade a subtração entre a pontuação do adversário à do jogador (visto ganhar quem tem menos pontos).

Foi guardado no nó a última jogada efetuado (ou seja, a jogada (PECA (X Y)) que gerou o estado contido no nó) para facilitar a obtenção da melhor jogada no fim da execução do algoritmo. Como a função sucessores é que gera os nós sucessores, e esta é independente do algoritmo implementado, ficou da sua responsabilidade guardar que operador foi utilizado em que posição dentro do nó.

Quando é definido um tempo limite no programa, prócuramos ajustar um pouco a profundidade máxima do algoritmo de forma a tomar proveito do tempo adicional de pesquisa com a função `profundidade-max-para-tempo`. Os valores aqui definidos resultaram apenas de alguns testes informais mas notamos que a principal limitação no aumento da profundidade máxima foi a limitação de memória HEAP do LispWorks. Estas limitações de memória também se tornaram mais evidentes após a implementação da tabela de memoização e foi por isso fornecida uma opção de reinicialização no menu principal.

## 5.**Limitações técnicas e ideias para desenvolvimento futuro**

Algumas ideias de desenvolvimento futuro:

- A interação com o utilizador durante o ciclo de jogo Humano vs Computador poderia ser melhorado com identificação de jogadas e uma representação do tabuleiro com número de linhas e colunas;
- A forma como a profundidade máxima é alterada consoante o tempo limite está demasiado simples e deveria tomar em consideração o estado atual do tabuleiro para tomar maior partido do tempo quando a árvore seria menos larga (geralmente mais para o fim do jogo);
- A tabela de memoização poderia ter um mecanismo mais ágil de remoção de valores que já não são relevantes;
- A função de avaliação de utilidade poderia considerar mais do que a pontuação atual dos jogadores, tal como a facilidade de fazer novas jogadas.

### Requisitos não implementados

Com exceção da procura quisciente e ordenação do nós (é utilizada uma função de ordenação aleatória) todos os restantes requisitos obrigatórios foram implementados.

### Potencial refactoring

- No ficheiro **projeto.lisp** é definida uma variável global *\*path\** para dar persistência em todas as operações do programa de leitura ou escrita do disco da pasta introduzida pelo utilizador no início da execução. Consideramos que tenha impacto minimo mas mesmo assim poderia ser resolvido de outra forma.
- As dependências entre a execução do algoritmo e a definição de valores na closure com funções auxiliares poderia ser melhorado.

### Melhoramentos potenciais de desempenho

- Implementação da ordenação de sucessores a analisar;
- Pesquisa quiescente.

## 6.**Resultados**

### Resultado 1: 1 segundo (profundidade máxima 3)

```
/////////////////////////////////////////////////////////////////////////
Jogo: Computador VS Computador 
Tempo limite: 1000 milisegundos 
/////////////////////////////////////////////////////////////////////////
Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (0 0) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
Peças disponiveis: 
Jogador 1: (10 9 15) 
Jogador 2: (10 10 15) 
 
 Melhor valor: 4 
Número nós analisados: 44 
Número cortes-alfa: 4 
Número cortes-beta: 1 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (12 12) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 9 15) 
Jogador 2: (10 9 15) 
 
 Melhor valor: 0 
Número nós analisados: 43 
Número cortes-alfa: 4 
Número cortes-beta: 2 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (2 2) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 9 14) 
Jogador 2: (10 9 15) 
 
 Melhor valor: 4 
Número nós analisados: 126 
Número cortes-alfa: 7 
Número cortes-beta: 3 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (10 10) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 9 14) 
Jogador 2: (10 8 15) 
 
 Melhor valor: 0 
Número nós analisados: 144 
Número cortes-alfa: 20 
Número cortes-beta: 4 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (2 5) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 9 13) 
Jogador 2: (10 8 15) 
 
 Melhor valor: 4 
Número nós analisados: 652 
Número cortes-alfa: 12 
Número cortes-beta: 20 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (8 8) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 9 13) 
Jogador 2: (10 7 15) 
 
 Melhor valor: 0 
Número nós analisados: 440 
Número cortes-alfa: 46 
Número cortes-beta: 11 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (4 1) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ _ X _ X X _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 8 13) 
Jogador 2: (10 7 15) 
 
 Melhor valor: 4 
Número nós analisados: 887 
Número cortes-alfa: 43 
Número cortes-beta: 22 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (5 11) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ _ X _ X X _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 8 13) 
Jogador 2: (10 7 14) 
 
 Melhor valor: 0 
Número nós analisados: 877 
Número cortes-alfa: 47 
Número cortes-beta: 21 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (4 8) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ _ X _ X X _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ X X _ _ _ _ _ _ _
_ _ _ _ X X _ _ O O _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 8 12) 
Jogador 2: (10 7 14) 
 
 Melhor valor: 4 
Número nós analisados: 812 
Número cortes-alfa: 32 
Número cortes-beta: 22 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (12 8) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ _ X _ X X _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ X X _ _ _ _ _ _ _
_ _ _ _ X X _ _ O O _ _ O O
_ _ _ _ _ _ _ _ O O _ _ O O
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 8 12) 
Jogador 2: (10 6 14) 
 
 Melhor valor: 0 
Número nós analisados: 1270 
Número cortes-alfa: 64 
Número cortes-beta: 27 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (7 6) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ _ X _ X X _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ X X _ _ _ _
_ _ X X _ _ _ X X _ _ _ _ _
_ _ _ X _ X X _ _ _ _ _ _ _
_ _ _ _ X X _ _ O O _ _ O O
_ _ _ _ _ _ _ _ O O _ _ O O
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 8 11) 
Jogador 2: (10 6 14) 
 
 Melhor valor: 4 
Número nós analisados: 1044 
Número cortes-alfa: 28 
Número cortes-beta: 28 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (7 12) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ _ X _ X X _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ X X _ _ _ _
_ _ X X _ _ _ X X _ _ _ _ _
_ _ _ X _ X X _ _ _ _ _ _ _
_ _ _ _ X X _ _ O O _ _ O O
_ _ _ _ _ _ _ _ O O _ _ O O
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ _ O O _ _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 8 11) 
Jogador 2: (10 5 14) 
 
 Melhor valor: 0 
Número nós analisados: 886 
Número cortes-alfa: 41 
Número cortes-beta: 23 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (1 8) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ _ X _ X X _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ X X _ _ _ _
_ _ X X _ _ _ X X _ _ _ _ _
_ _ _ X _ X X _ _ _ _ _ _ _
_ X X _ X X _ _ O O _ _ O O
_ X X _ _ _ _ _ O O _ _ O O
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ _ O O _ _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 7 11) 
Jogador 2: (10 5 14) 
 
 Melhor valor: 4 
Número nós analisados: 1972 
Número cortes-alfa: 17 
Número cortes-beta: 38 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (2 11) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ _ X _ X X _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ X X _ _ _ _
_ _ X X _ _ _ X X _ _ _ _ _
_ _ _ X _ X X _ _ _ _ _ _ _
_ X X _ X X _ _ O O _ _ O O
_ X X _ _ _ _ _ O O _ _ O O
_ _ _ O O _ O O _ _ O O _ _
_ _ O O _ O O _ _ _ O O _ _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 7 11) 
Jogador 2: (10 5 13) 
 
 Melhor valor: 0 
Número nós analisados: 458 
Número cortes-alfa: 31 
Número cortes-beta: 15 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (10 3) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ _ X _ X X _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ X X _ _
_ _ _ X _ _ _ _ _ _ X X _ _
_ _ X _ _ _ _ _ X X _ _ _ _
_ _ X X _ _ _ X X _ _ _ _ _
_ _ _ X _ X X _ _ _ _ _ _ _
_ X X _ X X _ _ O O _ _ O O
_ X X _ _ _ _ _ O O _ _ O O
_ _ _ O O _ O O _ _ O O _ _
_ _ O O _ O O _ _ _ O O _ _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 6 11) 
Jogador 2: (10 5 13) 
 
 Melhor valor: 4 
Número nós analisados: 1227 
Número cortes-alfa: 26 
Número cortes-beta: 29 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (10 5) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ _ X _ X X _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ X X _ _
_ _ _ X _ _ _ _ _ _ X X _ _
_ _ X _ _ _ _ _ X X O _ _ _
_ _ X X _ _ _ X X _ O O _ _
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ _ O O _ _ O O
_ X X _ _ _ _ _ O O _ _ O O
_ _ _ O O _ O O _ _ O O _ _
_ _ O O _ O O _ _ _ O O _ _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 6 11) 
Jogador 2: (10 5 12) 
 
 Melhor valor: 0 
Número nós analisados: 465 
Número cortes-alfa: 78 
Número cortes-beta: 10 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (11 0) 
X X _ _ _ _ _ _ _ _ _ X _ _
X X _ _ X X _ _ _ _ _ X X _
_ _ X _ X X _ _ _ _ _ _ X _
_ _ X X _ _ _ _ _ _ X X _ _
_ _ _ X _ _ _ _ _ _ X X _ _
_ _ X _ _ _ _ _ X X O _ _ _
_ _ X X _ _ _ X X _ O O _ _
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ _ O O _ _ O O
_ X X _ _ _ _ _ O O _ _ O O
_ _ _ O O _ O O _ _ O O _ _
_ _ O O _ O O _ _ _ O O _ _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 6 10) 
Jogador 2: (10 5 12) 
 
 Melhor valor: 4 
Número nós analisados: 1299 
Número cortes-alfa: 31 
Número cortes-beta: 30 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (8 2) 
X X _ _ _ _ _ _ _ _ _ X _ _
X X _ _ X X _ _ _ _ _ X X _
_ _ X _ X X _ _ O _ _ _ X _
_ _ X X _ _ _ _ O O X X _ _
_ _ _ X _ _ _ _ _ O X X _ _
_ _ X _ _ _ _ _ X X O _ _ _
_ _ X X _ _ _ X X _ O O _ _
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ _ O O _ _ O O
_ X X _ _ _ _ _ O O _ _ O O
_ _ _ O O _ O O _ _ O O _ _
_ _ O O _ O O _ _ _ O O _ _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 6 10) 
Jogador 2: (10 5 11) 
 
 Melhor valor: 0 
Número nós analisados: 384 
Número cortes-alfa: 53 
Número cortes-beta: 11 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (5 4) 
X X _ _ _ _ _ _ _ _ _ X _ _
X X _ _ X X _ _ _ _ _ X X _
_ _ X _ X X _ _ O _ _ _ X _
_ _ X X _ _ X X O O X X _ _
_ _ _ X _ X X _ _ O X X _ _
_ _ X _ _ _ _ _ X X O _ _ _
_ _ X X _ _ _ X X _ O O _ _
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ _ O O _ _ O O
_ X X _ _ _ _ _ O O _ _ O O
_ _ _ O O _ O O _ _ O O _ _
_ _ O O _ O O _ _ _ O O _ _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 6 9) 
Jogador 2: (10 5 11) 
 
 Melhor valor: 4 
Número nós analisados: 867 
Número cortes-alfa: 44 
Número cortes-beta: 23 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (9 0) 
X X _ _ _ _ _ _ _ O _ X _ _
X X _ _ X X _ _ _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ _
_ _ _ X _ X X _ _ O X X _ _
_ _ X _ _ _ _ _ X X O _ _ _
_ _ X X _ _ _ X X _ O O _ _
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ _ O O _ _ O O
_ X X _ _ _ _ _ O O _ _ O O
_ _ _ O O _ O O _ _ O O _ _
_ _ O O _ O O _ _ _ O O _ _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 6 9) 
Jogador 2: (10 5 10) 
 
 Melhor valor: 0 
Número nós analisados: 613 
Número cortes-alfa: 56 
Número cortes-beta: 16 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (0 10) 
X X _ _ _ _ _ _ _ O _ X _ _
X X _ _ X X _ _ _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ _
_ _ _ X _ X X _ _ O X X _ _
_ _ X _ _ _ _ _ X X O _ _ _
_ _ X X _ _ _ X X _ O O _ _
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ _ O O _ _ O O
_ X X _ _ _ _ _ O O _ _ O O
X _ _ O O _ O O _ _ O O _ _
X X O O _ O O _ _ _ O O _ _
_ X _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 6 8) 
Jogador 2: (10 5 10) 
 
 Melhor valor: 4 
Número nós analisados: 345 
Número cortes-alfa: 21 
Número cortes-beta: 14 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (12 4) 
X X _ _ _ _ _ _ _ O _ X _ _
X X _ _ X X _ _ _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ _
_ _ _ X _ X X _ _ O X X O _
_ _ X _ _ _ _ _ X X O _ O O
_ _ X X _ _ _ X X _ O O _ O
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ _ O O _ _ O O
_ X X _ _ _ _ _ O O _ _ O O
X _ _ O O _ O O _ _ O O _ _
X X O O _ O O _ _ _ O O _ _
_ X _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (10 6 8) 
Jogador 2: (10 5 9) 
 
 Melhor valor: 0 
Número nós analisados: 269 
Número cortes-alfa: 39 
Número cortes-beta: 8 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (1 4) 
X X _ _ _ _ _ _ _ O _ X _ _
X X _ _ X X _ _ _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ _
_ X _ X _ X X _ _ O X X O _
_ _ X _ _ _ _ _ X X O _ O O
_ _ X X _ _ _ X X _ O O _ O
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ _ O O _ _ O O
_ X X _ _ _ _ _ O O _ _ O O
X _ _ O O _ O O _ _ O O _ _
X X O O _ O O _ _ _ O O _ _
_ X _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (9 6 8) 
Jogador 2: (10 5 9) 
 
 Melhor valor: 1 
Número nós analisados: 491 
Número cortes-alfa: 22 
Número cortes-beta: 13 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (13 3) 
X X _ _ _ _ _ _ _ O _ X _ _
X X _ _ X X _ _ _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X _ _ O X X O _
_ _ X _ _ _ _ _ X X O _ O O
_ _ X X _ _ _ X X _ O O _ O
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ _ O O _ _ O O
_ X X _ _ _ _ _ O O _ _ O O
X _ _ O O _ O O _ _ O O _ _
X X O O _ O O _ _ _ O O _ _
_ X _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (9 6 8) 
Jogador 2: (9 5 9) 
 
 Melhor valor: 0 
Número nós analisados: 331 
Número cortes-alfa: 53 
Número cortes-beta: 7 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (7 8) 
X X _ _ _ _ _ _ _ O _ X _ _
X X _ _ X X _ _ _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X _ _ O X X O _
_ _ X _ _ _ _ _ X X O _ O O
_ _ X X _ _ _ X X _ O O _ O
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ X O O _ _ O O
_ X X _ _ _ _ _ O O _ _ O O
X _ _ O O _ O O _ _ O O _ _
X X O O _ O O _ _ _ O O _ _
_ X _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (8 6 8) 
Jogador 2: (9 5 9) 
 
 Melhor valor: 1 
Número nós analisados: 348 
Número cortes-alfa: 15 
Número cortes-beta: 13 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (5 9) 
X X _ _ _ _ _ _ _ O _ X _ _
X X _ _ X X _ _ _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X _ _ O X X O _
_ _ X _ _ _ _ _ X X O _ O O
_ _ X X _ _ _ X X _ O O _ O
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ X O O _ _ O O
_ X X _ _ O _ _ O O _ _ O O
X _ _ O O _ O O _ _ O O _ _
X X O O _ O O _ _ _ O O _ _
_ X _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (8 6 8) 
Jogador 2: (8 5 9) 
 
 Melhor valor: 0 
Número nós analisados: 216 
Número cortes-alfa: 31 
Número cortes-beta: 6 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (3 0) 
X X _ X _ _ _ _ _ O _ X _ _
X X _ _ X X _ _ _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X _ _ O X X O _
_ _ X _ _ _ _ _ X X O _ O O
_ _ X X _ _ _ X X _ O O _ O
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ X O O _ _ O O
_ X X _ _ O _ _ O O _ _ O O
X _ _ O O _ O O _ _ O O _ _
X X O O _ O O _ _ _ O O _ _
_ X _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (7 6 8) 
Jogador 2: (8 5 9) 
 
 Melhor valor: 1 
Número nós analisados: 376 
Número cortes-alfa: 15 
Número cortes-beta: 12 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (6 0) 
X X _ X _ _ O O _ O _ X _ _
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X _ _ O X X O _
_ _ X _ _ _ _ _ X X O _ O O
_ _ X X _ _ _ X X _ O O _ O
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ X O O _ _ O O
_ X X _ _ O _ _ O O _ _ O O
X _ _ O O _ O O _ _ O O _ _
X X O O _ O O _ _ _ O O _ _
_ X _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (7 6 8) 
Jogador 2: (8 4 9) 
 
 Melhor valor: 0 
Número nós analisados: 189 
Número cortes-alfa: 25 
Número cortes-beta: 6 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (2 13) 
X X _ X _ _ O O _ O _ X _ _
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X _ _ O X X O _
_ _ X _ _ _ _ _ X X O _ O O
_ _ X X _ _ _ X X _ O O _ O
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ X O O _ _ O O
_ X X _ _ O _ _ O O _ _ O O
X _ _ O O _ O O _ _ O O _ _
X X O O _ O O _ _ _ O O _ _
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (7 6 7) 
Jogador 2: (8 4 9) 
 
 Melhor valor: 1 
Número nós analisados: 257 
Número cortes-alfa: 13 
Número cortes-beta: 9 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (7 4) 
X X _ X _ _ O O _ O _ X _ _
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X O _ O X X O _
_ _ X _ _ _ _ _ X X O _ O O
_ _ X X _ _ _ X X _ O O _ O
_ _ _ X _ X X _ _ _ _ O _ _
_ X X _ X X _ X O O _ _ O O
_ X X _ _ O _ _ O O _ _ O O
X _ _ O O _ O O _ _ O O _ _
X X O O _ O O _ _ _ O O _ _
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (7 6 7) 
Jogador 2: (7 4 9) 
 
 Melhor valor: 3 
Número nós analisados: 70 
Número cortes-alfa: 18 
Número cortes-beta: 2 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (9 7) 
X X _ X _ _ O O _ O _ X _ _
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X O _ O X X O _
_ _ X _ _ _ _ _ X X O _ O O
_ _ X X _ _ _ X X _ O O _ O
_ _ _ X _ X X _ _ X _ O _ _
_ X X _ X X _ X O O _ _ O O
_ X X _ _ O _ _ O O _ _ O O
X _ _ O O _ O O _ _ O O _ _
X X O O _ O O _ _ _ O O _ _
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (6 6 7) 
Jogador 2: (7 4 9) 
 
 Melhor valor: 1 
Número nós analisados: 157 
Número cortes-alfa: 8 
Número cortes-beta: 8 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (5 5) 
X X _ X _ _ O O _ O _ X _ _
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X O _ O X X O _
_ _ X _ _ O O _ X X O _ O O
_ _ X X _ O O X X _ O O _ O
_ _ _ X _ X X _ _ X _ O _ _
_ X X _ X X _ X O O _ _ O O
_ X X _ _ O _ _ O O _ _ O O
X _ _ O O _ O O _ _ O O _ _
X X O O _ O O _ _ _ O O _ _
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (6 6 7) 
Jogador 2: (7 3 9) 
 
 Melhor valor: 0 
Número nós analisados: 142 
Número cortes-alfa: 28 
Número cortes-beta: 5 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (10 8) 
X X _ X _ _ O O _ O _ X _ _
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X O _ O X X O _
_ _ X _ _ O O _ X X O _ O O
_ _ X X _ O O X X _ O O _ O
_ _ _ X _ X X _ _ X _ O _ _
_ X X _ X X _ X O O X X O O
_ X X _ _ O _ _ O O X X O O
X _ _ O O _ O O _ _ O O _ _
X X O O _ O O _ _ _ O O _ _
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (6 5 7) 
Jogador 2: (7 3 9) 
 
 Melhor valor: 4 
Número nós analisados: 175 
Número cortes-alfa: 4 
Número cortes-beta: 10 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (6 8) 
X X _ X _ _ O O _ O _ X _ _
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X O _ O X X O _
_ _ X _ _ O O _ X X O _ O O
_ _ X X _ O O X X _ O O _ O
_ _ _ X _ X X _ _ X _ O _ _
_ X X _ X X O X O O X X O O
_ X X _ _ O _ _ O O X X O O
X _ _ O O _ O O _ _ O O _ _
X X O O _ O O _ _ _ O O _ _
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (6 5 7) 
Jogador 2: (6 3 9) 
 
 Melhor valor: -3 
Número nós analisados: 94 
Número cortes-alfa: 18 
Número cortes-beta: 4 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (12 10) 
X X _ X _ _ O O _ O _ X _ _
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X O _ O X X O _
_ _ X _ _ O O _ X X O _ O O
_ _ X X _ O O X X _ O O _ O
_ _ _ X _ X X _ _ X _ O _ _
_ X X _ X X O X O O X X O O
_ X X _ _ O _ _ O O X X O O
X _ _ O O _ O O _ _ O O X X
X X O O _ O O _ _ _ O O X X
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (6 4 7) 
Jogador 2: (6 3 9) 
 
 Melhor valor: 7 
Número nós analisados: 275 
Número cortes-alfa: 6 
Número cortes-beta: 13 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (1 10) 
X X _ X _ _ O O _ O _ X _ _
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X O _ O X X O _
_ _ X _ _ O O _ X X O _ O O
_ _ X X _ O O X X _ O O _ O
_ _ _ X _ X X _ _ X _ O _ _
_ X X _ X X O X O O X X O O
_ X X _ _ O _ _ O O X X O O
X O _ O O _ O O _ _ O O X X
X X O O _ O O _ _ _ O O X X
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (6 4 7) 
Jogador 2: (5 3 9) 
 
 Melhor valor: -6 
Número nós analisados: 82 
Número cortes-alfa: 18 
Número cortes-beta: 3 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (8 10) 
X X _ X _ _ O O _ O _ X _ _
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X O _ O X X O _
_ _ X _ _ O O _ X X O _ O O
_ _ X X _ O O X X _ O O _ O
_ _ _ X _ X X _ _ X _ O _ _
_ X X _ X X O X O O X X O O
_ X X _ _ O _ _ O O X X O O
X O _ O O _ O O X X O O X X
X X O O _ O O _ X X O O X X
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (6 3 7) 
Jogador 2: (5 3 9) 
 
 Melhor valor: 10 
Número nós analisados: 290 
Número cortes-alfa: 3 
Número cortes-beta: 15 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (4 7) 
X X _ X _ _ O O _ O _ X _ _
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X O _ O X X O _
_ _ X _ _ O O _ X X O _ O O
_ _ X X _ O O X X _ O O _ O
_ _ _ X O X X _ _ X _ O _ _
_ X X _ X X O X O O X X O O
_ X X _ _ O _ _ O O X X O O
X O _ O O _ O O X X O O X X
X X O O _ O O _ X X O O X X
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (6 3 7) 
Jogador 2: (4 3 9) 
 
 Melhor valor: -9 
Número nós analisados: 78 
Número cortes-alfa: 17 
Número cortes-beta: 3 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (13 0) 
X X _ X _ _ O O _ O _ X _ X
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X O _ O X X O _
_ _ X _ _ O O _ X X O _ O O
_ _ X X _ O O X X _ O O _ O
_ _ _ X O X X _ _ X _ O _ _
_ X X _ X X O X O O X X O O
_ X X _ _ O _ _ O O X X O O
X O _ O O _ O O X X O O X X
X X O O _ O O _ X X O O X X
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (5 3 7) 
Jogador 2: (4 3 9) 
 
 Melhor valor: 10 
Número nós analisados: 228 
Número cortes-alfa: 3 
Número cortes-beta: 13 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (3 8) 
X X _ X _ _ O O _ O _ X _ X
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X O _ O X X O _
_ _ X _ _ O O _ X X O _ O O
_ _ X X _ O O X X _ O O _ O
_ _ _ X O X X _ _ X _ O _ _
_ X X O X X O X O O X X O O
_ X X _ _ O _ _ O O X X O O
X O _ O O _ O O X X O O X X
X X O O _ O O _ X X O O X X
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (5 3 7) 
Jogador 2: (3 3 9) 
 
 Melhor valor: -9 
Número nós analisados: 86 
Número cortes-alfa: 21 
Número cortes-beta: 3 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (6 9) 
X X _ X _ _ O O _ O _ X _ X
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X O _ O X X O _
_ _ X _ _ O O _ X X O _ O O
_ _ X X _ O O X X _ O O _ O
_ _ _ X O X X _ _ X _ O _ _
_ X X O X X O X O O X X O O
_ X X _ _ O X _ O O X X O O
X O _ O O _ O O X X O O X X
X X O O _ O O _ X X O O X X
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (4 3 7) 
Jogador 2: (3 3 9) 
 
 Melhor valor: 10 
Número nós analisados: 204 
Número cortes-alfa: 3 
Número cortes-beta: 12 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (2 7) 
X X _ X _ _ O O _ O _ X _ X
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X O _ O X X O _
_ _ X _ _ O O _ X X O _ O O
_ _ X X _ O O X X _ O O _ O
_ _ O X O X X _ _ X _ O _ _
_ X X O X X O X O O X X O O
_ X X _ _ O X _ O O X X O O
X O _ O O _ O O X X O O X X
X X O O _ O O _ X X O O X X
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (4 3 7) 
Jogador 2: (2 3 9) 
 
 Melhor valor: -6 
Número nós analisados: 81 
Número cortes-alfa: 23 
Número cortes-beta: 2 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (0 5) 
X X _ X _ _ O O _ O _ X _ X
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X _ X X O _ O X X O _
X _ X _ _ O O _ X X O _ O O
_ _ X X _ O O X X _ O O _ O
_ _ O X O X X _ _ X _ O _ _
_ X X O X X O X O O X X O O
_ X X _ _ O X _ O O X X O O
X O _ O O _ O O X X O O X X
X X O O _ O O _ X X O O X X
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (3 3 7) 
Jogador 2: (2 3 9) 
 
 Melhor valor: 10 
Número nós analisados: 228 
Número cortes-alfa: 9 
Número cortes-beta: 11 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (4 4) 
X X _ X _ _ O O _ O _ X _ X
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
_ _ X X _ _ X X O O X X _ O
_ X _ X O X X O _ O X X O _
X _ X _ _ O O _ X X O _ O O
_ _ X X _ O O X X _ O O _ O
_ _ O X O X X _ _ X _ O _ _
_ X X O X X O X O O X X O O
_ X X _ _ O X _ O O X X O O
X O _ O O _ O O X X O O X X
X X O O _ O O _ X X O O X X
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (3 3 7) 
Jogador 2: (1 3 9) 
 
 Melhor valor: -9 
Número nós analisados: 77 
Número cortes-alfa: 17 
Número cortes-beta: 3 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (0 3) 
X X _ X _ _ O O _ O _ X _ X
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
X _ X X _ _ X X O O X X _ O
_ X _ X O X X O _ O X X O _
X _ X _ _ O O _ X X O _ O O
_ _ X X _ O O X X _ O O _ O
_ _ O X O X X _ _ X _ O _ _
_ X X O X X O X O O X X O O
_ X X _ _ O X _ O O X X O O
X O _ O O _ O O X X O O X X
X X O O _ O O _ X X O O X X
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (2 3 7) 
Jogador 2: (1 3 9) 
 
 Melhor valor: 10 
Número nós analisados: 176 
Número cortes-alfa: 4 
Número cortes-beta: 11 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (1 6) 
X X _ X _ _ O O _ O _ X _ X
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
X _ X X _ _ X X O O X X _ O
_ X _ X O X X O _ O X X O _
X _ X _ _ O O _ X X O _ O O
_ O X X _ O O X X _ O O _ O
_ _ O X O X X _ _ X _ O _ _
_ X X O X X O X O O X X O O
_ X X _ _ O X _ O O X X O O
X O _ O O _ O O X X O O X X
X X O O _ O O _ X X O O X X
_ X _ X X _ _ O O _ _ _ O O
_ _ X X _ _ _ O O _ _ _ O O
Peças disponiveis: 
Jogador 1: (2 3 7) 
Jogador 2: (0 3 9) 
 
 Melhor valor: -10 
Número nós analisados: 36 
Número cortes-alfa: 0 
Número cortes-beta: 4 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (10 12) 
X X _ X _ _ O O _ O _ X _ X
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
X _ X X _ _ X X O O X X _ O
_ X _ X O X X O _ O X X O _
X _ X _ _ O O _ X X O _ O O
_ O X X _ O O X X _ O O _ O
_ _ O X O X X _ _ X _ O _ _
_ X X O X X O X O O X X O O
_ X X _ _ O X _ O O X X O O
X O _ O O _ O O X X O O X X
X X O O _ O O _ X X O O X X
_ X _ X X _ _ O O _ X X O O
_ _ X X _ _ _ O O _ X X O O
Peças disponiveis: 
Jogador 1: (2 2 7) 
Jogador 2: (0 3 9) 
 
 Melhor valor: 10 
Número nós analisados: 13 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 

Jogador 2 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (12 7) 
X X _ X _ _ O O _ O _ X _ X
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
X _ X X _ _ X X O O X X _ O
_ X _ X O X X O _ O X X O _
X _ X _ _ O O _ X X O _ O O
_ O X X _ O O X X _ O O _ O
_ _ O X O X X _ _ X _ O X _
_ X X O X X O X O O X X O O
_ X X _ _ O X _ O O X X O O
X O _ O O _ O O X X O O X X
X X O O _ O O _ X X O O X X
_ X _ X X _ _ O O _ X X O O
_ _ X X _ _ _ O O _ X X O O
Peças disponiveis: 
Jogador 1: (1 2 7) 
Jogador 2: (0 3 9) 
 
 Melhor valor: 11 
Número nós analisados: 9 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 

Jogador 2 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (0 7) 
X X _ X _ _ O O _ O _ X _ X
X X _ _ X X O O _ O O X X _
_ _ X _ X X _ _ O _ O _ X _
X _ X X _ _ X X O O X X _ O
_ X _ X O X X O _ O X X O _
X _ X _ _ O O _ X X O _ O O
_ O X X _ O O X X _ O O _ O
X _ O X O X X _ _ X _ O X _
_ X X O X X O X O O X X O O
_ X X _ _ O X _ O O X X O O
X O _ O O _ O O X X O O X X
X X O O _ O O _ X X O O X X
_ X _ X X _ _ O O _ X X O O
_ _ X X _ _ _ O O _ X X O O
Peças disponiveis: 
Jogador 1: (0 2 7) 
Jogador 2: (0 3 9) 
 
 Melhor valor: 12 
Número nós analisados: 8 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Pontuações: 
Jogador 1: 36 pontos 
Jogador 2: 48 pontos
```

### Resultado 2: 4 segundos (profundidade máxima 3)

```
/////////////////////////////////////////////////////////////////////////
Jogo: Computador VS Computador 
Tempo limite: 4000 milisegundos 
/////////////////////////////////////////////////////////////////////////
Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (0 0) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
Peças disponiveis: 
Jogador 1: (10 10 14) 
Jogador 2: (10 10 15) 
 
 Melhor valor: 4 
Número nós analisados: 38 
Número cortes-alfa: 2 
Número cortes-beta: 2 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (12 11) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 14) 
Jogador 2: (10 10 14) 
 
 Melhor valor: 0 
Número nós analisados: 41 
Número cortes-alfa: 7 
Número cortes-beta: 2 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (0 3) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 13) 
Jogador 2: (10 10 14) 
 
 Melhor valor: 4 
Número nós analisados: 222 
Número cortes-alfa: 9 
Número cortes-beta: 7 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (9 11) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 13) 
Jogador 2: (10 10 13) 
 
 Melhor valor: 0 
Número nós analisados: 170 
Número cortes-alfa: 9 
Número cortes-beta: 7 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (2 6) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 13) 
Jogador 2: (10 10 13) 
 
 Melhor valor: 4 
Número nós analisados: 240 
Número cortes-alfa: 18 
Número cortes-beta: 9 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (12 8) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 13) 
Jogador 2: (10 9 13) 
 
 Melhor valor: 0 
Número nós analisados: 612 
Número cortes-alfa: 23 
Número cortes-beta: 18 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (4 8) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ _ X _ _ _ _ _ _ _ O O
_ _ _ _ X X _ _ _ _ _ _ O O
_ _ _ _ _ X _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 12) 
Jogador 2: (10 9 13) 
 
 Melhor valor: 4 
Número nós analisados: 704 
Número cortes-alfa: 35 
Número cortes-beta: 16 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (10 6) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ O O _ _
_ _ X X _ _ _ _ _ _ O O _ _
_ _ _ _ X _ _ _ _ _ _ _ O O
_ _ _ _ X X _ _ _ _ _ _ O O
_ _ _ _ _ X _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 12) 
Jogador 2: (10 8 13) 
 
 Melhor valor: 0 
Número nós analisados: 599 
Número cortes-alfa: 34 
Número cortes-beta: 18 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (4 4) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X _ _ _ _ _ _ _ _ _
_ X _ _ X X _ _ _ _ _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
_ _ X X _ _ _ _ _ _ O O _ _
_ _ _ _ X _ _ _ _ _ _ _ O O
_ _ _ _ X X _ _ _ _ _ _ O O
_ _ _ _ _ X _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 11) 
Jogador 2: (10 8 13) 
 
 Melhor valor: 4 
Número nós analisados: 1662 
Número cortes-alfa: 50 
Número cortes-beta: 33 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (8 8) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X _ _ _ _ _ _ _ _ _
_ X _ _ X X _ _ _ _ _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
_ _ X X _ _ _ _ _ _ O O _ _
_ _ _ _ X _ _ _ O O _ _ O O
_ _ _ _ X X _ _ O O _ _ O O
_ _ _ _ _ X _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 11) 
Jogador 2: (10 7 13) 
 
 Melhor valor: 0 
Número nós analisados: 858 
Número cortes-alfa: 39 
Número cortes-beta: 23 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (5 2) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ X X _ _ _ _ _ _ _
X _ _ _ _ X X _ _ _ _ _ _ _
X X _ _ X _ _ _ _ _ _ _ _ _
_ X _ _ X X _ _ _ _ _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
_ _ X X _ _ _ _ _ _ O O _ _
_ _ _ _ X _ _ _ O O _ _ O O
_ _ _ _ X X _ _ O O _ _ O O
_ _ _ _ _ X _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 11) 
Jogador 2: (10 7 13) 
 
 Melhor valor: 4 
Número nós analisados: 1968 
Número cortes-alfa: 49 
Número cortes-beta: 36 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (7 12) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ X X _ _ _ _ _ _ _
X _ _ _ _ X X _ _ _ _ _ _ _
X X _ _ X _ _ _ _ _ _ _ _ _
_ X _ _ X X _ _ _ _ _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
_ _ X X _ _ _ _ _ _ O O _ _
_ _ _ _ X _ _ _ O O _ _ O O
_ _ _ _ X X _ _ O O _ _ O O
_ _ _ _ _ X _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 11) 
Jogador 2: (10 6 13) 
 
 Melhor valor: 0 
Número nós analisados: 799 
Número cortes-alfa: 37 
Número cortes-beta: 22 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (0 7) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ X X _ _ _ _ _ _ _
X _ _ _ _ X X _ _ _ _ _ _ _
X X _ _ X _ _ _ _ _ _ _ _ _
_ X _ _ X X _ _ _ _ _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ _ _ _ _ O O _ _
X X _ _ X _ _ _ O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ _ _ _ X _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 10) 
Jogador 2: (10 6 13) 
 
 Melhor valor: 4 
Número nós analisados: 1661 
Número cortes-alfa: 21 
Número cortes-beta: 36 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (8 5) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ X X _ _ _ _ _ _ _
X _ _ _ _ X X _ _ _ _ _ _ _
X X _ _ X _ _ _ _ O O _ _ _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ _ _ _ _ O O _ _
X X _ _ X _ _ _ O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ _ _ _ X _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 10) 
Jogador 2: (10 6 12) 
 
 Melhor valor: 0 
Número nós analisados: 637 
Número cortes-alfa: 33 
Número cortes-beta: 18 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (6 7) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ X X _ _ _ _ _ _ _
X _ _ _ _ X X _ _ _ _ _ _ _
X X _ _ X _ _ _ _ O O _ _ _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ _ _ _ X _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 10) 
Jogador 2: (10 6 12) 
 
 Melhor valor: 4 
Número nós analisados: 1451 
Número cortes-alfa: 45 
Número cortes-beta: 31 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (7 2) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ X X O O _ _ _ _ _
X _ _ _ _ X X O O _ _ _ _ _
X X _ _ X _ _ _ _ O O _ _ _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ _ _ _ X _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 10) 
Jogador 2: (10 5 12) 
 
 Melhor valor: 0 
Número nós analisados: 619 
Número cortes-alfa: 54 
Número cortes-beta: 18 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (1 11) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ X X O O _ _ _ _ _
X _ _ _ _ X X O O _ _ _ _ _
X X _ _ X _ _ _ _ O O _ _ _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 9) 
Jogador 2: (10 5 12) 
 
 Melhor valor: 4 
Número nós analisados: 778 
Número cortes-alfa: 21 
Número cortes-beta: 25 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (11 2) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ X X O O _ _ O _ _
X _ _ _ _ X X O O _ _ O O _
X X _ _ X _ _ _ _ O O _ O _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 9) 
Jogador 2: (10 5 11) 
 
 Melhor valor: 0 
Número nós analisados: 735 
Número cortes-alfa: 39 
Número cortes-beta: 21 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (7 0) 
X _ _ _ _ _ _ X X _ _ _ _ _
X X _ _ _ _ _ X X _ _ _ _ _
_ X _ _ _ X X O O _ _ O _ _
X _ _ _ _ X X O O _ _ O O _
X X _ _ X _ _ _ _ O O _ O _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 6 9) 
Jogador 2: (10 5 11) 
 
 Melhor valor: 4 
Número nós analisados: 575 
Número cortes-alfa: 19 
Número cortes-beta: 20 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (12 0) 
X _ _ _ _ _ _ X X _ _ _ O O
X X _ _ _ _ _ X X _ _ _ O O
_ X _ _ _ X X O O _ _ O _ _
X _ _ _ _ X X O O _ _ O O _
X X _ _ X _ _ _ _ O O _ O _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 6 9) 
Jogador 2: (10 4 11) 
 
 Melhor valor: 0 
Número nós analisados: 534 
Número cortes-alfa: 43 
Número cortes-beta: 16 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (9 2) 
X _ _ _ _ _ _ X X _ _ _ O O
X X _ _ _ _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ _ O O _ O _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 5 9) 
Jogador 2: (10 4 11) 
 
 Melhor valor: 4 
Número nós analisados: 555 
Número cortes-alfa: 22 
Número cortes-beta: 20 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (4 12) 
X _ _ _ _ _ _ X X _ _ _ O O
X X _ _ _ _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ _ O O _ O _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X _ _ O O _ _ O O _ O _
_ _ _ _ O O _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 5 9) 
Jogador 2: (10 4 10) 
 
 Melhor valor: 0 
Número nós analisados: 362 
Número cortes-alfa: 36 
Número cortes-beta: 11 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (3 1) 
X _ _ _ X X _ X X _ _ _ O O
X X _ X X _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ _ O O _ O _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X _ _ O O _ _ O O _ O _
_ _ _ _ O O _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 5 8) 
Jogador 2: (10 4 10) 
 
 Melhor valor: 4 
Número nós analisados: 467 
Número cortes-alfa: 31 
Número cortes-beta: 14 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (3 11) 
X _ _ _ X X _ X X _ _ _ O O
X X _ X X _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ _ O O _ O _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X O _ O O _ _ O O _ O _
_ _ _ _ O O _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 5 8) 
Jogador 2: (9 4 10) 
 
 Melhor valor: -3 
Número nós analisados: 617 
Número cortes-alfa: 58 
Número cortes-beta: 11 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (8 4) 
X _ _ _ X X _ X X _ _ _ O O
X X _ X X _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O _ O _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X O _ O O _ _ O O _ O _
_ _ _ _ O O _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (9 5 8) 
Jogador 2: (9 4 10) 
 
 Melhor valor: 4 
Número nós analisados: 505 
Número cortes-alfa: 38 
Número cortes-beta: 12 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (0 13) 
X _ _ _ X X _ X X _ _ _ O O
X X _ X X _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O _ O _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X O _ O O _ _ O O _ O _
_ O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (9 5 8) 
Jogador 2: (9 4 9) 
 
 Melhor valor: 0 
Número nós analisados: 454 
Número cortes-alfa: 39 
Número cortes-beta: 12 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (0 12) 
X _ _ _ X X _ X X _ _ _ O O
X X _ X X _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O _ O _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (8 5 8) 
Jogador 2: (9 4 9) 
 
 Melhor valor: 1 
Número nós analisados: 662 
Número cortes-alfa: 61 
Número cortes-beta: 11 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (11 13) 
X _ _ _ X X _ X X _ _ _ O O
X X _ X X _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O _ O _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ _ _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (8 5 8) 
Jogador 2: (8 4 9) 
 
 Melhor valor: 0 
Número nós analisados: 434 
Número cortes-alfa: 38 
Número cortes-beta: 11 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (8 6) 
X _ _ _ X X _ X X _ _ _ O O
X X _ X X _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O _ O _
_ X _ _ X X _ _ O O _ _ _ _
_ _ X X _ X _ _ X _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (7 5 8) 
Jogador 2: (8 4 9) 
 
 Melhor valor: 1 
Número nós analisados: 527 
Número cortes-alfa: 52 
Número cortes-beta: 10 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (13 5) 
X _ _ _ X X _ X X _ _ _ O O
X X _ X X _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O _ O _
_ X _ _ X X _ _ O O _ _ _ O
_ _ X X _ X _ _ X _ O O _ _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (7 5 8) 
Jogador 2: (7 4 9) 
 
 Melhor valor: 0 
Número nós analisados: 560 
Número cortes-alfa: 55 
Número cortes-beta: 9 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (11 4) 
X _ _ _ X X _ X X _ _ _ O O
X X _ X X _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O X O _
_ X _ _ X X _ _ O O _ X X O
_ _ X X _ X _ _ X _ O O X _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ _ _ _ O O _ _
_ X X O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (7 5 7) 
Jogador 2: (7 4 9) 
 
 Melhor valor: 1 
Número nós analisados: 639 
Número cortes-alfa: 57 
Número cortes-beta: 10 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (7 10) 
X _ _ _ X X _ X X _ _ _ O O
X X _ X X _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O X O _
_ X _ _ X X _ _ O O _ X X O
_ _ X X _ X _ _ X _ O O X _
X _ X X _ _ X X _ _ O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ O _ _ O O _ _
_ X X O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (7 5 7) 
Jogador 2: (6 4 9) 
 
 Melhor valor: 0 
Número nós analisados: 176 
Número cortes-alfa: 10 
Número cortes-beta: 9 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (9 7) 
X _ _ _ X X _ X X _ _ _ O O
X X _ X X _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O X O _
_ X _ _ X X _ _ O O _ X X O
_ _ X X _ X _ _ X _ O O X _
X _ X X _ _ X X _ X O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X _ _ O O _ _ O O
_ _ X X _ X _ O _ _ O O _ _
_ X X O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (6 5 7) 
Jogador 2: (6 4 9) 
 
 Melhor valor: 4 
Número nós analisados: 339 
Número cortes-alfa: 39 
Número cortes-beta: 9 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (6 9) 
X _ _ _ X X _ X X _ _ _ O O
X X _ X X _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O X O _
_ X _ _ X X _ _ O O _ X X O
_ _ X X _ X _ _ X _ O O X _
X _ X X _ _ X X _ X O O _ _
X X _ _ X _ X X O O _ _ O O
_ X _ _ X X O _ O O _ _ O O
_ _ X X _ X _ O _ _ O O _ _
_ X X O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (6 5 7) 
Jogador 2: (5 4 9) 
 
 Melhor valor: -3 
Número nós analisados: 371 
Número cortes-alfa: 43 
Número cortes-beta: 9 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (10 8) 
X _ _ _ X X _ X X _ _ _ O O
X X _ X X _ _ X X _ _ _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O X O _
_ X _ _ X X _ _ O O _ X X O
_ _ X X _ X _ _ X _ O O X _
X _ X X _ _ X X _ X O O _ _
X X _ _ X _ X X O O X X O O
_ X _ _ X X O _ O O X X O O
_ _ X X _ X _ O _ _ O O _ _
_ X X O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (6 4 7) 
Jogador 2: (5 4 9) 
 
 Melhor valor: 7 
Número nós analisados: 466 
Número cortes-alfa: 32 
Número cortes-beta: 10 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (9 0) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O X O _
_ X _ _ X X _ _ O O _ X X O
_ _ X X _ X _ _ X _ O O X _
X _ X X _ _ X X _ X O O _ _
X X _ _ X _ X X O O X X O O
_ X _ _ X X O _ O O X X O O
_ _ X X _ X _ O _ _ O O _ _
_ X X O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (6 4 7) 
Jogador 2: (5 3 9) 
 
 Melhor valor: -3 
Número nós analisados: 390 
Número cortes-alfa: 48 
Número cortes-beta: 9 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (7 11) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O X O _
_ X _ _ X X _ _ O O _ X X O
_ _ X X _ X _ _ X _ O O X _
X _ X X _ _ X X _ X O O _ _
X X _ _ X _ X X O O X X O O
_ X _ _ X X O _ O O X X O O
_ _ X X _ X _ O X X O O _ _
_ X X O _ O O X X O O _ O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (6 4 6) 
Jogador 2: (5 3 9) 
 
 Melhor valor: 7 
Número nós analisados: 335 
Número cortes-alfa: 16 
Número cortes-beta: 11 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (4 6) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O X O _
_ X _ _ X X _ _ O O _ X X O
_ _ X X O X _ _ X _ O O X _
X _ X X O O X X _ X O O _ _
X X _ _ X O X X O O X X O O
_ X _ _ X X O _ O O X X O O
_ _ X X _ X _ O X X O O _ _
_ X X O _ O O X X O O _ O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (6 4 6) 
Jogador 2: (5 3 8) 
 
 Melhor valor: -3 
Número nós analisados: 162 
Número cortes-alfa: 24 
Número cortes-beta: 6 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (12 10) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ _ _ X X O O X X O _ _
X _ _ _ _ X X O O X X O O _
X X _ _ X _ _ _ X O O X O _
_ X _ _ X X _ _ O O _ X X O
_ _ X X O X _ _ X _ O O X _
X _ X X O O X X _ X O O _ _
X X _ _ X O X X O O X X O O
_ X _ _ X X O _ O O X X O O
_ _ X X _ X _ O X X O O X _
_ X X O _ O O X X O O _ O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (5 4 6) 
Jogador 2: (5 3 8) 
 
 Melhor valor: 4 
Número nós analisados: 577 
Número cortes-alfa: 32 
Número cortes-beta: 13 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (2 3) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ _ _ X X O O X X O _ _
X _ O _ _ X X O O X X O O _
X X O O X _ _ _ X O O X O _
_ X _ O X X _ _ O O _ X X O
_ _ X X O X _ _ X _ O O X _
X _ X X O O X X _ X O O _ _
X X _ _ X O X X O O X X O O
_ X _ _ X X O _ O O X X O O
_ _ X X _ X _ O X X O O X _
_ X X O _ O O X X O O _ O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (5 4 6) 
Jogador 2: (5 3 7) 
 
 Melhor valor: 0 
Número nós analisados: 374 
Número cortes-alfa: 41 
Número cortes-beta: 10 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (11 11) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ _ _ X X O O X X O _ _
X _ O _ _ X X O O X X O O _
X X O O X _ _ _ X O O X O _
_ X _ O X X _ _ O O _ X X O
_ _ X X O X _ _ X _ O O X _
X _ X X O O X X _ X O O _ _
X X _ _ X O X X O O X X O O
_ X _ _ X X O _ O O X X O O
_ _ X X _ X _ O X X O O X _
_ X X O _ O O X X O O X O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (4 4 6) 
Jogador 2: (5 3 7) 
 
 Melhor valor: 1 
Número nós analisados: 906 
Número cortes-alfa: 71 
Número cortes-beta: 13 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (2 8) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ _ _ X X O O X X O _ _
X _ O _ _ X X O O X X O O _
X X O O X _ _ _ X O O X O _
_ X _ O X X _ _ O O _ X X O
_ _ X X O X _ _ X _ O O X _
X _ X X O O X X _ X O O _ _
X X O O X O X X O O X X O O
_ X O O X X O _ O O X X O O
_ _ X X _ X _ O X X O O X _
_ X X O _ O O X X O O X O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (4 4 6) 
Jogador 2: (5 2 7) 
 
 Melhor valor: 3 
Número nós analisados: 838 
Número cortes-alfa: 85 
Número cortes-beta: 9 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (3 3) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ _ _ X X O O X X O _ _
X _ O X _ X X O O X X O O _
X X O O X _ _ _ X O O X O _
_ X _ O X X _ _ O O _ X X O
_ _ X X O X _ _ X _ O O X _
X _ X X O O X X _ X O O _ _
X X O O X O X X O O X X O O
_ X O O X X O _ O O X X O O
_ _ X X _ X _ O X X O O X _
_ X X O _ O O X X O O X O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (3 4 6) 
Jogador 2: (5 2 7) 
 
 Melhor valor: -2 
Número nós analisados: 845 
Número cortes-alfa: 65 
Número cortes-beta: 13 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (3 2) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ O _ X X O O X X O _ _
X _ O X _ X X O O X X O O _
X X O O X _ _ _ X O O X O _
_ X _ O X X _ _ O O _ X X O
_ _ X X O X _ _ X _ O O X _
X _ X X O O X X _ X O O _ _
X X O O X O X X O O X X O O
_ X O O X X O _ O O X X O O
_ _ X X _ X _ O X X O O X _
_ X X O _ O O X X O O X O _
X O O _ O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (3 4 6) 
Jogador 2: (4 2 7) 
 
 Melhor valor: 3 
Número nós analisados: 550 
Número cortes-alfa: 53 
Número cortes-beta: 11 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (3 12) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ O _ X X O O X X O _ _
X _ O X _ X X O O X X O O _
X X O O X _ _ _ X O O X O _
_ X _ O X X _ _ O O _ X X O
_ _ X X O X _ _ X _ O O X _
X _ X X O O X X _ X O O _ _
X X O O X O X X O O X X O O
_ X O O X X O _ O O X X O O
_ _ X X _ X _ O X X O O X _
_ X X O _ O O X X O O X O _
X O O X O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (2 4 6) 
Jogador 2: (4 2 7) 
 
 Melhor valor: -2 
Número nós analisados: 594 
Número cortes-alfa: 41 
Número cortes-beta: 12 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (7 6) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ O _ X X O O X X O _ _
X _ O X _ X X O O X X O O _
X X O O X _ _ _ X O O X O _
_ X _ O X X _ _ O O _ X X O
_ _ X X O X _ O X _ O O X _
X _ X X O O X X _ X O O _ _
X X O O X O X X O O X X O O
_ X O O X X O _ O O X X O O
_ _ X X _ X _ O X X O O X _
_ X X O _ O O X X O O X O _
X O O X O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (2 4 6) 
Jogador 2: (3 2 7) 
 
 Melhor valor: 3 
Número nós analisados: 622 
Número cortes-alfa: 63 
Número cortes-beta: 11 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (0 10) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ O _ X X O O X X O _ _
X _ O X _ X X O O X X O O _
X X O O X _ _ _ X O O X O _
_ X _ O X X _ _ O O _ X X O
_ _ X X O X _ O X _ O O X _
X _ X X O O X X _ X O O _ _
X X O O X O X X O O X X O O
_ X O O X X O _ O O X X O O
X _ X X _ X _ O X X O O X _
_ X X O _ O O X X O O X O _
X O O X O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (1 4 6) 
Jogador 2: (3 2 7) 
 
 Melhor valor: -2 
Número nós analisados: 876 
Número cortes-alfa: 70 
Número cortes-beta: 13 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (0 11) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ O _ X X O O X X O _ _
X _ O X _ X X O O X X O O _
X X O O X _ _ _ X O O X O _
_ X _ O X X _ _ O O _ X X O
_ _ X X O X _ O X _ O O X _
X _ X X O O X X _ X O O _ _
X X O O X O X X O O X X O O
_ X O O X X O _ O O X X O O
X _ X X _ X _ O X X O O X _
O X X O _ O O X X O O X O _
X O O X O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (1 4 6) 
Jogador 2: (2 2 7) 
 
 Melhor valor: 3 
Número nós analisados: 565 
Número cortes-alfa: 56 
Número cortes-beta: 10 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (13 4) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ O _ X X O O X X O _ _
X _ O X _ X X O O X X O O _
X X O O X _ _ _ X O O X O X
_ X _ O X X _ _ O O _ X X O
_ _ X X O X _ O X _ O O X _
X _ X X O O X X _ X O O _ _
X X O O X O X X O O X X O O
_ X O O X X O _ O O X X O O
X _ X X _ X _ O X X O O X _
O X X O _ O O X X O O X O _
X O O X O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (0 4 6) 
Jogador 2: (2 2 7) 
 
 Melhor valor: -2 
Número nós analisados: 179 
Número cortes-alfa: 52 
Número cortes-beta: 12 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (4 3) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ O _ X X O O X X O _ _
X _ O X O X X O O X X O O _
X X O O X _ _ _ X O O X O X
_ X _ O X X _ _ O O _ X X O
_ _ X X O X _ O X _ O O X _
X _ X X O O X X _ X O O _ _
X X O O X O X X O O X X O O
_ X O O X X O _ O O X X O O
X _ X X _ X _ O X X O O X _
O X X O _ O O X X O O X O _
X O O X O O _ O O _ _ _ O O
O O _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (0 4 6) 
Jogador 2: (1 2 7) 
 
 Melhor valor: 3 
Número nós analisados: 114 
Número cortes-alfa: 0 
Número cortes-beta: 9 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (9 12) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ O _ X X O O X X O _ _
X _ O X O X X O O X X O O _
X X O O X _ _ _ X O O X O X
_ X _ O X X _ _ O O _ X X O
_ _ X X O X _ O X _ O O X _
X _ X X O O X X _ X O O _ _
X X O O X O X X O O X X O O
_ X O O X X O _ O O X X O O
X _ X X _ X _ O X X O O X _
O X X O _ O O X X O O X O _
X O O X O O _ O O X X _ O O
O O _ _ _ _ _ O O X X O _ O
Peças disponiveis: 
Jogador 1: (0 3 6) 
Jogador 2: (1 2 7) 
 
 Melhor valor: -3 
Número nós analisados: 13 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (0 5) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ O _ X X O O X X O _ _
X _ O X O X X O O X X O O _
X X O O X _ _ _ X O O X O X
O X _ O X X _ _ O O _ X X O
O O X X O X _ O X _ O O X _
X O X X O O X X _ X O O _ _
X X O O X O X X O O X X O O
_ X O O X X O _ O O X X O O
X _ X X _ X _ O X X O O X _
O X X O _ O O X X O O X O _
X O O X O O _ O O X X _ O O
O O _ _ _ _ _ O O X X O _ O
Peças disponiveis: 
Jogador 1: (0 3 6) 
Jogador 2: (1 2 6) 
 
 Melhor valor: 3 
Número nós analisados: 12 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 

Jogador 1 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (6 4) 
X _ _ _ X X _ X X O O _ O O
X X _ X X _ _ X X O O _ O O
_ X _ O _ X X O O X X O _ _
X _ O X O X X O O X X O O _
X X O O X _ O _ X O O X O X
O X _ O X X _ _ O O _ X X O
O O X X O X _ O X _ O O X _
X O X X O O X X _ X O O _ _
X X O O X O X X O O X X O O
_ X O O X X O _ O O X X O O
X _ X X _ X _ O X X O O X _
O X X O _ O O X X O O X O _
X O O X O O _ O O X X _ O O
O O _ _ _ _ _ O O X X O _ O
Peças disponiveis: 
Jogador 1: (0 3 6) 
Jogador 2: (0 2 6) 
 
 Melhor valor: 4 
Número nós analisados: 10 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Pontuações: 
Jogador 1: 36 pontos 
Jogador 2: 32 pontos
```

### Resultado 3: 8 segundos (profundidade máxima 4)

```
/////////////////////////////////////////////////////////////////////////
Jogo: Computador VS Computador 
Tempo limite: 8000 milisegundos 
/////////////////////////////////////////////////////////////////////////
Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (0 0) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
Peças disponiveis: 
Jogador 1: (10 9 15) 
Jogador 2: (10 10 15) 
 
 Melhor valor: 0 
Número nós analisados: 80 
Número cortes-alfa: 1 
Número cortes-beta: 22 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (12 11) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 15) 
Jogador 2: (10 10 14) 
 
 Melhor valor: -4 
Número nós analisados: 129 
Número cortes-alfa: 4 
Número cortes-beta: 19 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (2 2) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 14) 
Jogador 2: (10 10 14) 
 
 Melhor valor: 0 
Número nós analisados: 501 
Número cortes-alfa: 8 
Número cortes-beta: 123 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (9 11) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 14) 
Jogador 2: (10 10 13) 
 
 Melhor valor: -4 
Número nós analisados: 717 
Número cortes-alfa: 11 
Número cortes-beta: 165 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (5 2) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ _ _ _ _
_ _ X X _ X X _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 13) 
Jogador 2: (10 10 13) 
 
 Melhor valor: 0 
Número nós analisados: 1343 
Número cortes-alfa: 19 
Número cortes-beta: 270 
Limite de tempo alcançado: Não 
Memoização ativada 3 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (7 12) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ _ _ _ _
_ _ X X _ X X _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 13) 
Jogador 2: (10 9 13) 
 
 Melhor valor: -4 
Número nós analisados: 2103 
Número cortes-alfa: 29 
Número cortes-beta: 532 
Limite de tempo alcançado: Não 
Memoização ativada 13 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (7 3) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ _ _ _ _
_ _ X X _ X X _ _ _ _ _ _ _
_ _ _ _ _ _ _ X X _ _ _ _ _
_ _ _ _ _ _ _ X X _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 13) 
Jogador 2: (10 9 13) 
 
 Melhor valor: 0 
Número nós analisados: 2180 
Número cortes-alfa: 40 
Número cortes-beta: 459 
Limite de tempo alcançado: Não 
Memoização ativada 14 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (11 7) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ _ _ _ _
_ _ X X _ X X _ _ _ _ _ _ _
_ _ _ _ _ _ _ X X _ _ _ _ _
_ _ _ _ _ _ _ X X _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ O _ _
_ _ _ _ _ _ _ _ _ _ _ O O _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 13) 
Jogador 2: (10 9 12) 
 
 Melhor valor: -4 
Número nós analisados: 2853 
Número cortes-alfa: 44 
Número cortes-beta: 562 
Limite de tempo alcançado: Não 
Memoização ativada 10 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (9 2) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ _ _
_ _ _ _ _ _ _ X X _ _ _ _ _
_ _ _ _ _ _ _ X X _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ O _ _
_ _ _ _ _ _ _ _ _ _ _ O O _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 12) 
Jogador 2: (10 9 12) 
 
 Melhor valor: 0 
Número nós analisados: 3173 
Número cortes-alfa: 47 
Número cortes-beta: 674 
Limite de tempo alcançado: Não 
Memoização ativada 9 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (8 7) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ _ _
_ _ _ _ _ _ _ X X _ _ _ _ _
_ _ _ _ _ _ _ X X _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ O O _ _ _
_ _ _ _ _ _ _ _ O O _ O _ _
_ _ _ _ _ _ _ _ _ _ _ O O _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 12) 
Jogador 2: (10 9 11) 
 
 Melhor valor: -4 
Número nós analisados: 3631 
Número cortes-alfa: 49 
Número cortes-beta: 787 
Limite de tempo alcançado: Não 
Memoização ativada 12 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (12 2) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ _ _ _ X X _ _ _ X X
_ _ _ _ _ _ _ X X _ _ _ _ X
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ O O _ _ _
_ _ _ _ _ _ _ _ O O _ O _ _
_ _ _ _ _ _ _ _ _ _ _ O O _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 11) 
Jogador 2: (10 9 11) 
 
 Melhor valor: 0 
Número nós analisados: 3202 
Número cortes-alfa: 35 
Número cortes-beta: 721 
Limite de tempo alcançado: Não 
Memoização ativada 10 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (11 4) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ _ _ _ X X _ _ _ X X
_ _ _ _ _ _ _ X X _ _ O O X
_ _ _ _ _ _ _ _ _ _ _ O O _
_ _ _ _ _ _ _ _ _ O O _ _ _
_ _ _ _ _ _ _ _ O O _ O _ _
_ _ _ _ _ _ _ _ _ _ _ O O _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 11) 
Jogador 2: (10 8 11) 
 
 Melhor valor: -4 
Número nós analisados: 3124 
Número cortes-alfa: 16 
Número cortes-beta: 975 
Limite de tempo alcançado: Não 
Memoização ativada 22 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (5 4) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ _ _ _ X X _ _ _ X X
_ _ _ _ _ X _ X X _ _ O O X
_ _ _ _ _ X X _ _ _ _ O O _
_ _ _ _ _ _ X _ _ O O _ _ _
_ _ _ _ _ _ _ _ O O _ O _ _
_ _ _ _ _ _ _ _ _ _ _ O O _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 10) 
Jogador 2: (10 8 11) 
 
 Melhor valor: 0 
Número nós analisados: 1847 
Número cortes-alfa: 26 
Número cortes-beta: 434 
Limite de tempo alcançado: Não 
Memoização ativada 16 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (6 8) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ _ _ _ X X _ _ _ X X
_ _ _ _ _ X _ X X _ _ O O X
_ _ _ _ _ X X _ _ _ _ O O _
_ _ _ _ _ _ X _ _ O O _ _ _
_ _ _ _ _ _ _ _ O O _ O _ _
_ _ _ _ _ _ O O _ _ _ O O _
_ _ _ _ _ _ O O _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 10) 
Jogador 2: (10 7 11) 
 
 Melhor valor: -4 
Número nós analisados: 2035 
Número cortes-alfa: 22 
Número cortes-beta: 538 
Limite de tempo alcançado: Não 
Memoização ativada 14 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (3 6) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ _ _ _ X X _ _ _ X X
_ _ _ _ _ X _ X X _ _ O O X
_ _ _ _ _ X X _ _ _ _ O O _
_ _ _ X X _ X _ _ O O _ _ _
_ _ _ X X _ _ _ O O _ O _ _
_ _ _ _ _ _ O O _ _ _ O O _
_ _ _ _ _ _ O O _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 10) 
Jogador 2: (10 7 11) 
 
 Melhor valor: 0 
Número nós analisados: 2525 
Número cortes-alfa: 38 
Número cortes-beta: 570 
Limite de tempo alcançado: Não 
Memoização ativada 22 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (4 12) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ _ _ _ X X _ _ _ X X
_ _ _ _ _ X _ X X _ _ O O X
_ _ _ _ _ X X _ _ _ _ O O _
_ _ _ X X _ X _ _ O O _ _ _
_ _ _ X X _ _ _ O O _ O _ _
_ _ _ _ _ _ O O _ _ _ O O _
_ _ _ _ _ _ O O _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ _ O O _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 10) 
Jogador 2: (10 7 10) 
 
 Melhor valor: -4 
Número nós analisados: 2131 
Número cortes-alfa: 44 
Número cortes-beta: 354 
Limite de tempo alcançado: Não 
Memoização ativada 10 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (2 8) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ _ _ _ X X _ _ _ X X
_ _ _ _ _ X _ X X _ _ O O X
_ _ _ _ _ X X _ _ _ _ O O _
_ _ _ X X _ X _ _ O O _ _ _
_ _ _ X X _ _ _ O O _ O _ _
_ _ X _ _ _ O O _ _ _ O O _
_ _ X X _ _ O O _ _ _ _ O _
_ _ _ X _ _ _ _ _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ _ O O _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 9) 
Jogador 2: (10 7 10) 
 
 Melhor valor: 3 
Número nós analisados: 1835 
Número cortes-alfa: 17 
Número cortes-beta: 535 
Limite de tempo alcançado: Não 
Memoização ativada 12 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (1 12) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ _ _ _ X X _ _ _ X X
_ _ _ _ _ X _ X X _ _ O O X
_ _ _ _ _ X X _ _ _ _ O O _
_ _ _ X X _ X _ _ O O _ _ _
_ _ _ X X _ _ _ O O _ O _ _
_ _ X _ _ _ O O _ _ _ O O _
_ _ X X _ _ O O _ _ _ _ O _
_ _ _ X _ _ _ _ _ _ O O _ _
_ _ O O _ O O _ _ O O _ O _
_ O O _ O O _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 9) 
Jogador 2: (10 7 9) 
 
 Melhor valor: -7 
Número nós analisados: 5079 
Número cortes-alfa: 108 
Número cortes-beta: 498 
Limite de tempo alcançado: Não 
Memoização ativada 44 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (0 9) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ _ _ _ X X _ _ _ X X
_ _ _ _ _ X _ X X _ _ O O X
_ _ _ _ _ X X _ _ _ _ O O _
_ _ _ X X _ X _ _ O O _ _ _
_ _ _ X X _ _ _ O O _ O _ _
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ _ _ _ O _
X X _ X _ _ _ _ _ _ O O _ _
_ X O O _ O O _ _ O O _ O _
_ O O _ O O _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 8) 
Jogador 2: (10 7 9) 
 
 Melhor valor: 6 
Número nós analisados: 2123 
Número cortes-alfa: 35 
Número cortes-beta: 566 
Limite de tempo alcançado: Não 
Memoização ativada 24 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (13 10) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ _ _ _ X X _ _ _ X X
_ _ _ _ _ X _ X X _ _ O O X
_ _ _ _ _ X X _ _ _ _ O O _
_ _ _ X X _ X _ _ O O _ _ _
_ _ _ X X _ _ _ O O _ O _ _
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ _ _ _ O _
X X _ X _ _ _ _ _ _ O O _ O
_ X O O _ O O _ _ O O _ O _
_ O O _ O O _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 8) 
Jogador 2: (9 7 9) 
 
 Melhor valor: -10 
Número nós analisados: 2551 
Número cortes-alfa: 29 
Número cortes-beta: 291 
Limite de tempo alcançado: Não 
Memoização ativada 28 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (0 5) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ _ _ _ X X _ _ _ X X
_ _ _ _ _ X _ X X _ _ O O X
X _ _ _ _ X X _ _ _ _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X _ _ _ O O _ O _ _
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ _ _ _ O _
X X _ X _ _ _ _ _ _ O O _ O
_ X O O _ O O _ _ O O _ O _
_ O O _ O O _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 7) 
Jogador 2: (9 7 9) 
 
 Melhor valor: 9 
Número nós analisados: 1454 
Número cortes-alfa: 24 
Número cortes-beta: 462 
Limite de tempo alcançado: Não 
Memoização ativada 26 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (0 13) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ _ _ _ X X _ _ _ X X
_ _ _ _ _ X _ X X _ _ O O X
X _ _ _ _ X X _ _ _ _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X _ _ _ O O _ O _ _
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ _ _ _ O _
X X _ X _ _ _ _ _ _ O O _ O
_ X O O _ O O _ _ O O _ O _
_ O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 7) 
Jogador 2: (8 7 9) 
 
 Melhor valor: -10 
Número nós analisados: 1622 
Número cortes-alfa: 15 
Número cortes-beta: 206 
Limite de tempo alcançado: Não 
Memoização ativada 7 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (5 0) 
X X _ _ _ X _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ _ _ _ X X _ _ _ X X
_ _ _ _ _ X _ X X _ _ O O X
X _ _ _ _ X X _ _ _ _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X _ _ _ O O _ O _ _
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ _ _ _ O _
X X _ X _ _ _ _ _ _ O O _ O
_ X O O _ O O _ _ O O _ O _
_ O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (9 7 7) 
Jogador 2: (8 7 9) 
 
 Melhor valor: 9 
Número nós analisados: 895 
Número cortes-alfa: 22 
Número cortes-beta: 276 
Limite de tempo alcançado: Não 
Memoização ativada 23 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (5 7) 
X X _ _ _ X _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ _ _ _ X X _ _ _ X X
_ _ _ _ _ X _ X X _ _ O O X
X _ _ _ _ X X _ _ _ _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X O _ _ O O _ O _ _
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ _ _ _ O _
X X _ X _ _ _ _ _ _ O O _ O
_ X O O _ O O _ _ O O _ O _
_ O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (9 7 7) 
Jogador 2: (7 7 9) 
 
 Melhor valor: -10 
Número nós analisados: 1373 
Número cortes-alfa: 13 
Número cortes-beta: 189 
Limite de tempo alcançado: Não 
Memoização ativada 8 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (4 3) 
X X _ _ _ X _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ X _ _ X X _ _ _ X X
_ _ _ _ _ X _ X X _ _ O O X
X _ _ _ _ X X _ _ _ _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X O _ _ O O _ O _ _
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ _ _ _ O _
X X _ X _ _ _ _ _ _ O O _ O
_ X O O _ O O _ _ O O _ O _
_ O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (8 7 7) 
Jogador 2: (7 7 9) 
 
 Melhor valor: 9 
Número nós analisados: 676 
Número cortes-alfa: 11 
Número cortes-beta: 204 
Limite de tempo alcançado: Não 
Memoização ativada 10 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (10 3) 
X X _ _ _ X _ _ _ _ _ _ _ _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ X _ _ X X _ O _ X X
_ _ _ _ _ X _ X X _ _ O O X
X _ _ _ _ X X _ _ _ _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X O _ _ O O _ O _ _
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ _ _ _ O _
X X _ X _ _ _ _ _ _ O O _ O
_ X O O _ O O _ _ O O _ O _
_ O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (8 7 7) 
Jogador 2: (6 7 9) 
 
 Melhor valor: -10 
Número nós analisados: 1282 
Número cortes-alfa: 15 
Número cortes-beta: 197 
Limite de tempo alcançado: Não 
Memoização ativada 18 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (12 0) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ X _ _ X X _ O _ X X
_ _ _ _ _ X _ X X _ _ O O X
X _ _ _ _ X X _ _ _ _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X O _ _ O O _ O _ _
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ _ _ _ O _
X X _ X _ _ _ _ _ _ O O _ O
_ X O O _ O O _ _ O O _ O _
_ O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (7 7 7) 
Jogador 2: (6 7 9) 
 
 Melhor valor: 9 
Número nós analisados: 1208 
Número cortes-alfa: 35 
Número cortes-beta: 283 
Limite de tempo alcançado: Não 
Memoização ativada 32 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (13 7) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ X _ _ X X _ O _ X X
_ _ _ _ _ X _ X X _ _ O O X
X _ _ _ _ X X _ _ _ _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X O _ _ O O _ O _ O
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ _ _ _ O _
X X _ X _ _ _ _ _ _ O O _ O
_ X O O _ O O _ _ O O _ O _
_ O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (7 7 7) 
Jogador 2: (5 7 9) 
 
 Melhor valor: -10 
Número nós analisados: 1552 
Número cortes-alfa: 16 
Número cortes-beta: 251 
Limite de tempo alcançado: Não 
Memoização ativada 13 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (0 12) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ X _ _ X X _ O _ X X
_ _ _ _ _ X _ X X _ _ O O X
X _ _ _ _ X X _ _ _ _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X O _ _ O O _ O _ O
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ _ _ _ O _
X X _ X _ _ _ _ _ _ O O _ O
_ X O O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (6 7 7) 
Jogador 2: (5 7 9) 
 
 Melhor valor: 9 
Número nós analisados: 675 
Número cortes-alfa: 10 
Número cortes-beta: 208 
Limite de tempo alcançado: Não 
Memoização ativada 12 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (8 10) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ _
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ X _ _ X X _ O _ X X
_ _ _ _ _ X _ X X _ _ O O X
X _ _ _ _ X X _ _ _ _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X O _ _ O O _ O _ O
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ _ _ _ O _
X X _ X _ _ _ _ O _ O O _ O
_ X O O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (6 7 7) 
Jogador 2: (4 7 9) 
 
 Melhor valor: -10 
Número nós analisados: 1045 
Número cortes-alfa: 12 
Número cortes-beta: 147 
Limite de tempo alcançado: Não 
Memoização ativada 9 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (13 1) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ X
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ X _ _ X X _ O _ X X
_ _ _ _ _ X _ X X _ _ O O X
X _ _ _ _ X X _ _ _ _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X O _ _ O O _ O _ O
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ _ _ _ O _
X X _ X _ _ _ _ O _ O O _ O
_ X O O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (5 7 7) 
Jogador 2: (4 7 9) 
 
 Melhor valor: 9 
Número nós analisados: 612 
Número cortes-alfa: 9 
Número cortes-beta: 197 
Limite de tempo alcançado: Não 
Memoização ativada 13 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (9 9) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ X
_ _ X X _ X X _ _ X X _ X _
_ _ _ _ X _ _ X X _ O _ X X
_ _ _ _ _ X _ X X _ _ O O X
X _ _ _ _ X X _ _ _ _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X O _ _ O O _ O _ O
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ O _ _ O _
X X _ X _ _ _ _ O _ O O _ O
_ X O O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (5 7 7) 
Jogador 2: (3 7 9) 
 
 Melhor valor: -10 
Número nós analisados: 745 
Número cortes-alfa: 12 
Número cortes-beta: 116 
Limite de tempo alcançado: Não 
Memoização ativada 7 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (1 3) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ X
_ _ X X _ X X _ _ X X _ X _
_ X _ _ X _ _ X X _ O _ X X
_ X X _ _ X _ X X _ _ O O X
X _ X _ _ X X _ _ _ _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X O _ _ O O _ O _ O
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ O _ _ O _
X X _ X _ _ _ _ O _ O O _ O
_ X O O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (5 7 6) 
Jogador 2: (3 7 9) 
 
 Melhor valor: 9 
Número nós analisados: 362 
Número cortes-alfa: 8 
Número cortes-beta: 130 
Limite de tempo alcançado: Não 
Memoização ativada 8 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (11 2) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ X
_ _ X X _ X X _ _ X X O X _
_ X _ _ X _ _ X X _ O _ X X
_ X X _ _ X _ X X _ _ O O X
X _ X _ _ X X _ _ _ _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X O _ _ O O _ O _ O
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ O _ _ O _
X X _ X _ _ _ _ O _ O O _ O
_ X O O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (5 7 6) 
Jogador 2: (2 7 9) 
 
 Melhor valor: -10 
Número nós analisados: 217 
Número cortes-alfa: 6 
Número cortes-beta: 74 
Limite de tempo alcançado: Não 
Memoização ativada 6 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (9 5) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ X
_ _ X X _ X X _ _ X X O X _
_ X _ _ X _ _ X X _ O _ X X
_ X X _ _ X _ X X _ _ O O X
X _ X _ _ X X _ _ X _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X O _ _ O O _ O _ O
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ O _ _ O _
X X _ X _ _ _ _ O _ O O _ O
_ X O O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (4 7 6) 
Jogador 2: (2 7 9) 
 
 Melhor valor: 9 
Número nós analisados: 196 
Número cortes-alfa: 8 
Número cortes-beta: 49 
Limite de tempo alcançado: Não 
Memoização ativada 2 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (11 13) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ X
_ _ X X _ X X _ _ X X O X _
_ X _ _ X _ _ X X _ O _ X X
_ X X _ _ X _ X X _ _ O O X
X _ X _ _ X X _ _ X _ O O _
X X _ X X _ X _ _ O O _ _ _
_ X _ X X O _ _ O O _ O _ O
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ O _ _ O _
X X _ X _ _ _ _ O _ O O _ O
_ X O O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (4 7 6) 
Jogador 2: (1 7 9) 
 
 Melhor valor: -10 
Número nós analisados: 236 
Número cortes-alfa: 7 
Número cortes-beta: 78 
Limite de tempo alcançado: Não 
Memoização ativada 4 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (8 6) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ X
_ _ X X _ X X _ _ X X O X _
_ X _ _ X _ _ X X _ O _ X X
_ X X _ _ X _ X X _ _ O O X
X _ X _ _ X X _ _ X _ O O _
X X _ X X _ X _ X O O _ _ _
_ X _ X X O _ _ O O _ O _ O
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ O _ _ O _
X X _ X _ _ _ _ O _ O O _ O
_ X O O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O _ _ _ _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (3 7 6) 
Jogador 2: (1 7 9) 
 
 Melhor valor: 10 
Número nós analisados: 89 
Número cortes-alfa: 7 
Número cortes-beta: 7 
Limite de tempo alcançado: Não 
Memoização ativada 6 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (3 13) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ X
_ _ X X _ X X _ _ X X O X _
_ X _ _ X _ _ X X _ O _ X X
_ X X _ _ X _ X X _ _ O O X
X _ X _ _ X X _ _ X _ O O _
X X _ X X _ X _ X O O _ _ _
_ X _ X X O _ _ O O _ O _ O
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ O _ _ O _
X X _ X _ _ _ _ O _ O O _ O
_ X O O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O _ _ O _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (3 7 6) 
Jogador 2: (0 7 9) 
 
 Melhor valor: -10 
Número nós analisados: 24 
Número cortes-alfa: 0 
Número cortes-beta: 7 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (7 7) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ X
_ _ X X _ X X _ _ X X O X _
_ X _ _ X _ _ X X _ O _ X X
_ X X _ _ X _ X X _ _ O O X
X _ X _ _ X X _ _ X _ O O _
X X _ X X _ X _ X O O _ _ _
_ X _ X X O _ X O O _ O _ O
_ _ X _ _ _ O O _ _ _ O O _
X _ X X _ _ O O _ O _ _ O _
X X _ X _ _ _ _ O _ O O _ O
_ X O O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O _ _ O _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (2 7 6) 
Jogador 2: (0 7 9) 
 
 Melhor valor: 10 
Número nós analisados: 9 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 

Jogador 2 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (8 8) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ X
_ _ X X _ X X _ _ X X O X _
_ X _ _ X _ _ X X _ O _ X X
_ X X _ _ X _ X X _ _ O O X
X _ X _ _ X X _ _ X _ O O _
X X _ X X _ X _ X O O _ _ _
_ X _ X X O _ X O O _ O _ O
_ _ X _ _ _ O O X _ _ O O _
X _ X X _ _ O O _ O _ _ O _
X X _ X _ _ _ _ O _ O O _ O
_ X O O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O _ _ O _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (1 7 6) 
Jogador 2: (0 7 9) 
 
 Melhor valor: 11 
Número nós analisados: 9 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 

Jogador 2 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (5 8) 
X X _ _ _ X _ _ _ _ _ _ X _
X X _ X X _ X X _ _ X X _ X
_ _ X X _ X X _ _ X X O X _
_ X _ _ X _ _ X X _ O _ X X
_ X X _ _ X _ X X _ _ O O X
X _ X _ _ X X _ _ X _ O O _
X X _ X X _ X _ X O O _ _ _
_ X _ X X O _ X O O _ O _ O
_ _ X _ _ X O O X _ _ O O _
X _ X X _ _ O O _ O _ _ O _
X X _ X _ _ _ _ O _ O O _ O
_ X O O _ O O _ _ O O _ O _
X O O _ O O _ O O _ _ _ O O
O _ _ O _ _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (0 7 6) 
Jogador 2: (0 7 9) 
 
 Melhor valor: 12 
Número nós analisados: 8 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Pontuações: 
Jogador 1: 52 pontos 
Jogador 2: 64 pontos
```

### Resultado 4: 12 segundos (profundidade máxima 4)

```
/////////////////////////////////////////////////////////////////////////
Jogo: Computador VS Computador 
Tempo limite: 12000 milisegundos 
/////////////////////////////////////////////////////////////////////////
Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (0 0) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
Peças disponiveis: 
Jogador 1: (10 10 14) 
Jogador 2: (10 10 15) 
 
 Melhor valor: 0 
Número nós analisados: 89 
Número cortes-alfa: 4 
Número cortes-beta: 17 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (12 11) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 14) 
Jogador 2: (10 10 14) 
 
 Melhor valor: -4 
Número nós analisados: 207 
Número cortes-alfa: 7 
Número cortes-beta: 21 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (2 3) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 13) 
Jogador 2: (10 10 14) 
 
 Melhor valor: 0 
Número nós analisados: 649 
Número cortes-alfa: 7 
Número cortes-beta: 195 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (9 11) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ X _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 13) 
Jogador 2: (10 10 13) 
 
 Melhor valor: -4 
Número nós analisados: 1716 
Número cortes-alfa: 43 
Número cortes-beta: 138 
Limite de tempo alcançado: Não 
Memoização ativada 2 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (0 4) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ _ _ _ _ _ _
X _ X X _ _ _ _ _ _ _ _ _ _
X X _ X _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 12) 
Jogador 2: (10 10 13) 
 
 Melhor valor: 0 
Número nós analisados: 2916 
Número cortes-alfa: 37 
Número cortes-beta: 751 
Limite de tempo alcançado: Não 
Memoização ativada 13 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (8 8) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X _ _ _ _ _ _ _ _ _ _ _
X _ X X _ _ _ _ _ _ _ _ _ _
X X _ X _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 12) 
Jogador 2: (10 9 13) 
 
 Melhor valor: -4 
Número nós analisados: 2110 
Número cortes-alfa: 19 
Número cortes-beta: 619 
Limite de tempo alcançado: Não 
Memoização ativada 19 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (4 2) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ X _ _ _ _ _ _ _ _ _
_ _ X _ X X _ _ _ _ _ _ _ _
X _ X X _ X _ _ _ _ _ _ _ _
X X _ X _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 11) 
Jogador 2: (10 9 13) 
 
 Melhor valor: 0 
Número nós analisados: 2330 
Número cortes-alfa: 22 
Número cortes-beta: 610 
Limite de tempo alcançado: Não 
Memoização ativada 7 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (5 11) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ X _ _ _ _ _ _ _ _ _
_ _ X _ X X _ _ _ _ _ _ _ _
X _ X X _ X _ _ _ _ _ _ _ _
X X _ X _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 11) 
Jogador 2: (10 9 12) 
 
 Melhor valor: -4 
Número nós analisados: 2889 
Número cortes-alfa: 25 
Número cortes-beta: 847 
Limite de tempo alcançado: Não 
Memoização ativada 15 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (2 7) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ X _ _ _ _ _ _ _ _ _
_ _ X _ X X _ _ _ _ _ _ _ _
X _ X X _ X _ _ _ _ _ _ _ _
X X _ X _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 11) 
Jogador 2: (10 9 12) 
 
 Melhor valor: 0 
Número nós analisados: 4813 
Número cortes-alfa: 68 
Número cortes-beta: 842 
Limite de tempo alcançado: Não 
Memoização ativada 7 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (7 12) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ X _ _ _ _ _ _ _ _ _
_ _ X _ X X _ _ _ _ _ _ _ _
X _ X X _ X _ _ _ _ _ _ _ _
X X _ X _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 11) 
Jogador 2: (10 8 12) 
 
 Melhor valor: -4 
Número nós analisados: 4583 
Número cortes-alfa: 45 
Número cortes-beta: 1250 
Limite de tempo alcançado: Não 
Memoização ativada 24 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (6 1) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ X _ _ _ _ _ _ _
_ X _ _ X _ X X _ _ _ _ _ _
_ _ X _ X X _ X _ _ _ _ _ _
X _ X X _ X _ _ _ _ _ _ _ _
X X _ X _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ _ _ O O _ _ _ _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 10) 
Jogador 2: (10 8 12) 
 
 Melhor valor: 0 
Número nós analisados: 3568 
Número cortes-alfa: 54 
Número cortes-beta: 673 
Limite de tempo alcançado: Não 
Memoização ativada 15 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (3 10) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ X _ _ _ _ _ _ _
_ X _ _ X _ X X _ _ _ _ _ _
_ _ X _ X X _ X _ _ _ _ _ _
X _ X X _ X _ _ _ _ _ _ _ _
X X _ X _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ O O _ _ _ _
_ _ _ _ O O _ _ O O _ _ _ _
_ _ _ O O _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 10) 
Jogador 2: (10 8 11) 
 
 Melhor valor: -4 
Número nós analisados: 3280 
Número cortes-alfa: 26 
Número cortes-beta: 948 
Limite de tempo alcançado: Não 
Memoização ativada 21 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (8 0) 
X _ _ _ _ _ _ _ X X _ _ _ _
X X _ _ _ _ X _ X X _ _ _ _
_ X _ _ X _ X X _ _ _ _ _ _
_ _ X _ X X _ X _ _ _ _ _ _
X _ X X _ X _ _ _ _ _ _ _ _
X X _ X _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ O O _ _ _ _
_ _ _ _ O O _ _ O O _ _ _ _
_ _ _ O O _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 10) 
Jogador 2: (10 8 11) 
 
 Melhor valor: 0 
Número nós analisados: 2687 
Número cortes-alfa: 33 
Número cortes-beta: 628 
Limite de tempo alcançado: Não 
Memoização ativada 17 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (10 6) 
X _ _ _ _ _ _ _ X X _ _ _ _
X X _ _ _ _ X _ X X _ _ _ _
_ X _ _ X _ X X _ _ _ _ _ _
_ _ X _ X X _ X _ _ _ _ _ _
X _ X X _ X _ _ _ _ _ _ _ _
X X _ X _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ O O _ _
_ _ X X _ _ _ _ _ _ O O _ _
_ _ X X _ _ _ _ O O _ _ _ _
_ _ _ _ O O _ _ O O _ _ _ _
_ _ _ O O _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 10) 
Jogador 2: (10 7 11) 
 
 Melhor valor: -4 
Número nós analisados: 2871 
Número cortes-alfa: 23 
Número cortes-beta: 851 
Limite de tempo alcançado: Não 
Memoização ativada 21 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (9 3) 
X _ _ _ _ _ _ _ X X _ _ _ _
X X _ _ _ _ X _ X X _ _ _ _
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ _ _
X _ X X _ X _ _ _ _ _ _ _ _
X X _ X _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ O O _ _
_ _ X X _ _ _ _ _ _ O O _ _
_ _ X X _ _ _ _ O O _ _ _ _
_ _ _ _ O O _ _ O O _ _ _ _
_ _ _ O O _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ _ _ _ _ O O _ _ _ O O
_ _ _ _ _ _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 9) 
Jogador 2: (10 7 11) 
 
 Melhor valor: 0 
Número nós analisados: 4169 
Número cortes-alfa: 64 
Número cortes-beta: 726 
Limite de tempo alcançado: Não 
Memoização ativada 22 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (3 12) 
X _ _ _ _ _ _ _ X X _ _ _ _
X X _ _ _ _ X _ X X _ _ _ _
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ _ _
X _ X X _ X _ _ _ _ _ _ _ _
X X _ X _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ O O _ _
_ _ X X _ _ _ _ _ _ O O _ _
_ _ X X _ _ _ _ O O _ _ _ _
_ _ _ _ O O _ _ O O _ _ _ _
_ _ _ O O _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 9) 
Jogador 2: (10 6 11) 
 
 Melhor valor: -4 
Número nós analisados: 4599 
Número cortes-alfa: 61 
Número cortes-beta: 1038 
Limite de tempo alcançado: Não 
Memoização ativada 35 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (12 0) 
X _ _ _ _ _ _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ _ _
X _ X X _ X _ _ _ _ _ _ _ _
X X _ X _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ O O _ _
_ _ X X _ _ _ _ _ _ O O _ _
_ _ X X _ _ _ _ O O _ _ _ _
_ _ _ _ O O _ _ O O _ _ _ _
_ _ _ O O _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 9) 
Jogador 2: (10 6 11) 
 
 Melhor valor: 0 
Número nós analisados: 3204 
Número cortes-alfa: 51 
Número cortes-beta: 718 
Limite de tempo alcançado: Não 
Memoização ativada 18 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (6 5) 
X _ _ _ _ _ _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ _ _
X _ X X _ X _ _ _ _ _ _ _ _
X X _ X _ _ O _ _ _ _ _ _ _
_ X _ _ _ _ O O _ _ O O _ _
_ _ X X _ _ _ O _ _ O O _ _
_ _ X X _ _ _ _ O O _ _ _ _
_ _ _ _ O O _ _ O O _ _ _ _
_ _ _ O O _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 9) 
Jogador 2: (10 6 10) 
 
 Melhor valor: -4 
Número nós analisados: 2532 
Número cortes-alfa: 23 
Número cortes-beta: 728 
Limite de tempo alcançado: Não 
Memoização ativada 21 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (12 3) 
X _ _ _ _ _ _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ _ _ _ _ _ X X
X X _ X _ _ O _ _ _ _ _ _ _
_ X _ _ _ _ O O _ _ O O _ _
_ _ X X _ _ _ O _ _ O O _ _
_ _ X X _ _ _ _ O O _ _ _ _
_ _ _ _ O O _ _ O O _ _ _ _
_ _ _ O O _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 6 9) 
Jogador 2: (10 6 10) 
 
 Melhor valor: 0 
Número nós analisados: 2155 
Número cortes-alfa: 46 
Número cortes-beta: 396 
Limite de tempo alcançado: Não 
Memoização ativada 11 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (0 10) 
X _ _ _ _ _ _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ _ _ _ _ _ X X
X X _ X _ _ O _ _ _ _ _ _ _
_ X _ _ _ _ O O _ _ O O _ _
_ _ X X _ _ _ O _ _ O O _ _
_ _ X X _ _ _ _ O O _ _ _ _
_ O O _ O O _ _ O O _ _ _ _
O O _ O O _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 6 9) 
Jogador 2: (10 6 9) 
 
 Melhor valor: -1 
Número nós analisados: 1948 
Número cortes-alfa: 45 
Número cortes-beta: 454 
Limite de tempo alcançado: Não 
Memoização ativada 63 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (8 4) 
X _ _ _ _ _ _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ _ X _ _ _ X X
X X _ X _ _ O _ X X _ _ _ _
_ X _ _ _ _ O O _ X O O _ _
_ _ X X _ _ _ O _ _ O O _ _
_ _ X X _ _ _ _ O O _ _ _ _
_ O O _ O O _ _ O O _ _ _ _
O O _ O O _ O O _ _ O O _ _
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 6 8) 
Jogador 2: (10 6 9) 
 
 Melhor valor: 0 
Número nós analisados: 1430 
Número cortes-alfa: 46 
Número cortes-beta: 128 
Limite de tempo alcançado: Não 
Memoização ativada 11 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (12 8) 
X _ _ _ _ _ _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ _ X _ _ _ X X
X X _ X _ _ O _ X X _ _ _ _
_ X _ _ _ _ O O _ X O O _ _
_ _ X X _ _ _ O _ _ O O _ _
_ _ X X _ _ _ _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 6 8) 
Jogador 2: (10 6 8) 
 
 Melhor valor: -1 
Número nós analisados: 792 
Número cortes-alfa: 21 
Número cortes-beta: 205 
Limite de tempo alcançado: Não 
Memoização ativada 37 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (4 6) 
X _ _ _ _ _ _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ _ X _ _ _ X X
X X _ X _ _ O _ X X _ _ _ _
_ X _ _ X _ O O _ X O O _ _
_ _ X X _ _ _ O _ _ O O _ _
_ _ X X _ _ _ _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (9 6 8) 
Jogador 2: (10 6 8) 
 
 Melhor valor: 0 
Número nós analisados: 290 
Número cortes-alfa: 9 
Número cortes-beta: 72 
Limite de tempo alcançado: Não 
Memoização ativada 5 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (5 7) 
X _ _ _ _ _ _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ _ X _ _ _ X X
X X _ X _ _ O _ X X _ _ _ _
_ X _ _ X _ O O _ X O O _ _
_ _ X X _ O _ O _ _ O O _ _
_ _ X X _ _ _ _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (9 6 8) 
Jogador 2: (9 6 8) 
 
 Melhor valor: -1 
Número nós analisados: 291 
Número cortes-alfa: 13 
Número cortes-beta: 60 
Limite de tempo alcançado: Não 
Memoização ativada 6 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (11 5) 
X _ _ _ _ _ _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ _ X _ _ _ X X
X X _ X _ _ O _ X X _ X _ _
_ X _ _ X _ O O _ X O O _ _
_ _ X X _ O _ O _ _ O O _ _
_ _ X X _ _ _ _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (8 6 8) 
Jogador 2: (9 6 8) 
 
 Melhor valor: 0 
Número nós analisados: 350 
Número cortes-alfa: 16 
Número cortes-beta: 48 
Limite de tempo alcançado: Não 
Memoização ativada 16 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (13 7) 
X _ _ _ _ _ _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ _ X _ _ _ X X
X X _ X _ _ O _ X X _ X _ _
_ X _ _ X _ O O _ X O O _ _
_ _ X X _ O _ O _ _ O O _ O
_ _ X X _ _ _ _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (8 6 8) 
Jogador 2: (8 6 8) 
 
 Melhor valor: -1 
Número nós analisados: 192 
Número cortes-alfa: 5 
Número cortes-beta: 57 
Limite de tempo alcançado: Não 
Memoização ativada 4 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (8 7) 
X _ _ _ _ _ _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ _ X _ _ _ X X
X X _ X _ _ O _ X X _ X _ _
_ X _ _ X _ O O _ X O O _ _
_ _ X X _ O _ O X _ O O _ O
_ _ X X _ _ _ _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (7 6 8) 
Jogador 2: (8 6 8) 
 
 Melhor valor: 0 
Número nós analisados: 258 
Número cortes-alfa: 13 
Número cortes-beta: 39 
Limite de tempo alcançado: Não 
Memoização ativada 10 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (6 8) 
X _ _ _ _ _ _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ _ X _ _ _ X X
X X _ X _ _ O _ X X _ X _ _
_ X _ _ X _ O O _ X O O _ _
_ _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (7 6 8) 
Jogador 2: (7 6 8) 
 
 Melhor valor: -1 
Número nós analisados: 331 
Número cortes-alfa: 12 
Número cortes-beta: 50 
Limite de tempo alcançado: Não 
Memoização ativada 7 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (12 6) 
X _ _ _ _ _ _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ _ X _ _ _ X X
X X _ X _ _ O _ X X _ X _ _
_ X _ _ X _ O O _ X O O X _
_ _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ _ _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (6 6 8) 
Jogador 2: (7 6 8) 
 
 Melhor valor: -3 
Número nós analisados: 210 
Número cortes-alfa: 4 
Número cortes-beta: 40 
Limite de tempo alcançado: Não 
Memoização ativada 16 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (2 11) 
X _ _ _ _ _ _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ _ X _ _ _ X X
X X _ X _ _ O _ X X _ X _ _
_ X _ _ X _ O O _ X O O X _
_ _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (6 6 8) 
Jogador 2: (6 6 8) 
 
 Melhor valor: 2 
Número nós analisados: 133 
Número cortes-alfa: 8 
Número cortes-beta: 30 
Limite de tempo alcançado: Não 
Memoização ativada 3 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (5 0) 
X _ _ _ _ X _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ _ X _ _ _ X X
X X _ X _ _ O _ X X _ X _ _
_ X _ _ X _ O O _ X O O X _
_ _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (5 6 8) 
Jogador 2: (6 6 8) 
 
 Melhor valor: -3 
Número nós analisados: 168 
Número cortes-alfa: 5 
Número cortes-beta: 27 
Limite de tempo alcançado: Não 
Memoização ativada 2 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (7 4) 
X _ _ _ _ X _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X _ _
_ X _ _ X _ O O _ X O O X _
_ _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (5 6 8) 
Jogador 2: (5 6 8) 
 
 Melhor valor: 2 
Número nós analisados: 135 
Número cortes-alfa: 3 
Número cortes-beta: 42 
Limite de tempo alcançado: Não 
Memoização ativada 8 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (2 0) 
X _ X _ _ X _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X _ _
_ X _ _ X _ O O _ X O O X _
_ _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (4 6 8) 
Jogador 2: (5 6 8) 
 
 Melhor valor: -3 
Número nós analisados: 127 
Número cortes-alfa: 6 
Número cortes-beta: 14 
Limite de tempo alcançado: Não 
Memoização ativada 4 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (12 5) 
X _ X _ _ X _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X O _
_ X _ _ X _ O O _ X O O X _
_ _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (4 6 8) 
Jogador 2: (4 6 8) 
 
 Melhor valor: 2 
Número nós analisados: 122 
Número cortes-alfa: 2 
Número cortes-beta: 43 
Limite de tempo alcançado: Não 
Memoização ativada 8 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (0 7) 
X _ X _ _ X _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X _ X _ X X _ X X
X _ X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X O _
_ X _ _ X _ O O _ X O O X _
X _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (3 6 8) 
Jogador 2: (4 6 8) 
 
 Melhor valor: -3 
Número nós analisados: 101 
Número cortes-alfa: 6 
Número cortes-beta: 9 
Limite de tempo alcançado: Não 
Memoização ativada 1 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (6 3) 
X _ X _ _ X _ _ X X _ _ X X
X X _ _ _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X O X _ X X _ X X
X _ X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X O _
_ X _ _ X _ O O _ X O O X _
X _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (3 6 8) 
Jogador 2: (3 6 8) 
 
 Melhor valor: 2 
Número nós analisados: 97 
Número cortes-alfa: 1 
Número cortes-beta: 30 
Limite de tempo alcançado: Não 
Memoização ativada 20 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (3 1) 
X _ X _ _ X _ _ X X _ _ X X
X X _ X _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X O X _ X X _ X X
X _ X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X O _
_ X _ _ X _ O O _ X O O X _
X _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
_ _ _ O O _ _ O O _ _ _ O O
_ _ _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (2 6 8) 
Jogador 2: (3 6 8) 
 
 Melhor valor: -6 
Número nós analisados: 116 
Número cortes-alfa: 8 
Número cortes-beta: 4 
Limite de tempo alcançado: Não 
Memoização ativada 3 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (0 12) 
X _ X _ _ X _ _ X X _ _ X X
X X _ X _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X O X _ X X _ X X
X _ X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X O _
_ X _ _ X _ O O _ X O O X _
X _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O _ O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
O O _ O O _ _ O O _ _ _ O O
O O _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (2 6 8) 
Jogador 2: (3 5 8) 
 
 Melhor valor: 5 
Número nós analisados: 114 
Número cortes-alfa: 0 
Número cortes-beta: 39 
Limite de tempo alcançado: Não 
Memoização ativada 26 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (7 8) 
X _ X _ _ X _ _ X X _ _ X X
X X _ X _ _ X _ X X _ _ X X
_ X _ _ X _ X X _ _ X X _ _
_ _ X _ X X O X _ X X _ X X
X _ X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X O _
_ X _ _ X _ O O _ X O O X _
X _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O X O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
O O _ O O _ _ O O _ _ _ O O
O O _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (1 6 8) 
Jogador 2: (3 5 8) 
 
 Melhor valor: -9 
Número nós analisados: 57 
Número cortes-alfa: 4 
Número cortes-beta: 2 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (4 0) 
X _ X _ O X _ _ X X _ _ X X
X X _ X O O X _ X X _ _ X X
_ X _ _ X O X X _ _ X X _ _
_ _ X _ X X O X _ X X _ X X
X _ X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X O _
_ X _ _ X _ O O _ X O O X _
X _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O X O O _ _ O _
_ O O _ O O _ _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
O O _ O O _ _ O O _ _ _ O O
O O _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (1 6 8) 
Jogador 2: (3 5 7) 
 
 Melhor valor: 9 
Número nós analisados: 56 
Número cortes-alfa: 2 
Número cortes-beta: 4 
Limite de tempo alcançado: Não 
Memoização ativada 9 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (6 9) 
X _ X _ O X _ _ X X _ _ X X
X X _ X O O X _ X X _ _ X X
_ X _ _ X O X X _ _ X X _ _
_ _ X _ X X O X _ X X _ X X
X _ X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X O _
_ X _ _ X _ O O _ X O O X _
X _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O X O O _ _ O _
_ O O _ O O X _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
O O _ O O _ _ O O _ _ _ O O
O O _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (0 6 8) 
Jogador 2: (3 5 7) 
 
 Melhor valor: -9 
Número nós analisados: 12 
Número cortes-alfa: 0 
Número cortes-beta: 1 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (2 1) 
X _ X _ O X _ _ X X _ _ X X
X X O X O O X _ X X _ _ X X
_ X O O X O X X _ _ X X _ _
_ _ X O X X O X _ X X _ X X
X _ X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X O _
_ X _ _ X _ O O _ X O O X _
X _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O X O O _ _ O _
_ O O _ O O X _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
O O _ O O _ _ O O _ _ _ O O
O O _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (0 6 8) 
Jogador 2: (3 5 6) 
 
 Melhor valor: 9 
Número nós analisados: 9 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 

Jogador 1 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (0 2) 
X _ X _ O X _ _ X X _ _ X X
X X O X O O X _ X X _ _ X X
O X O O X O X X _ _ X X _ _
O O X O X X O X _ X X _ X X
X O X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X O _
_ X _ _ X _ O O _ X O O X _
X _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O X O O _ _ O _
_ O O _ O O X _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
O O _ O O _ _ O O _ _ _ O O
O O _ O O _ _ O O _ _ _ _ O
Peças disponiveis: 
Jogador 1: (0 6 8) 
Jogador 2: (3 5 5) 
 
 Melhor valor: 13 
Número nós analisados: 11 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 

Jogador 1 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (11 13) 
X _ X _ O X _ _ X X _ _ X X
X X O X O O X _ X X _ _ X X
O X O O X O X X _ _ X X _ _
O O X O X X O X _ X X _ X X
X O X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X O _
_ X _ _ X _ O O _ X O O X _
X _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O X O O _ _ O _
_ O O _ O O X _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
O O _ O O _ _ O O _ _ _ O O
O O _ O O _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (0 6 8) 
Jogador 2: (2 5 5) 
 
 Melhor valor: 14 
Número nós analisados: 10 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 

Jogador 1 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (8 3) 
X _ X _ O X _ _ X X _ _ X X
X X O X O O X _ X X _ _ X X
O X O O X O X X _ _ X X _ _
O O X O X X O X O X X _ X X
X O X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X O _
_ X _ _ X _ O O _ X O O X _
X _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O X O O _ _ O _
_ O O _ O O X _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
O O _ O O _ _ O O _ _ _ O O
O O _ O O _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (0 6 8) 
Jogador 2: (1 5 5) 
 
 Melhor valor: 15 
Número nós analisados: 9 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 

Jogador 1 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (9 2) 
X _ X _ O X _ _ X X _ _ X X
X X O X O O X _ X X _ _ X X
O X O O X O X X _ O X X _ _
O O X O X X O X O X X _ X X
X O X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X O _
_ X _ _ X _ O O _ X O O X _
X _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O X O O _ _ O _
_ O O _ O O X _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
O O _ O O _ _ O O _ _ _ O O
O O _ O O _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (0 6 8) 
Jogador 2: (0 5 5) 
 
 Melhor valor: 16 
Número nós analisados: 10 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 

Jogador 1 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (10 0) 
X _ X _ O X _ _ X X O O X X
X X O X O O X _ X X O O X X
O X O O X O X X _ O X X _ _
O O X O X X O X O X X _ X X
X O X X _ X _ O X _ _ _ X X
X X _ X _ _ O _ X X _ X O _
_ X _ _ X _ O O _ X O O X _
X _ X X _ O _ O X _ O O _ O
_ _ X X _ _ O X O O _ _ O _
_ O O _ O O X _ O O _ _ O O
O O _ O O _ O O _ _ O O _ O
_ _ O _ _ O O _ _ O O _ O _
O O _ O O _ _ O O _ _ _ O O
O O _ O O _ _ O O _ _ O _ O
Peças disponiveis: 
Jogador 1: (0 6 8) 
Jogador 2: (0 4 5) 
 
 Melhor valor: 20 
Número nós analisados: 3 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Pontuações: 
Jogador 1: 56 pontos 
Jogador 2: 36 pontos
```

### Resultado 5: 16 segundo (profundidade máxima 5)

```
/////////////////////////////////////////////////////////////////////////
Jogo: Computador VS Computador 
Tempo limite: 16000 milisegundos 
/////////////////////////////////////////////////////////////////////////
Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (0 0) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
Peças disponiveis: 
Jogador 1: (10 9 15) 
Jogador 2: (10 10 15) 
 
 Melhor valor: 4 
Número nós analisados: 502 
Número cortes-alfa: 22 
Número cortes-beta: 22 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (12 11) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 15) 
Jogador 2: (10 10 14) 
 
 Melhor valor: 0 
Número nós analisados: 745 
Número cortes-alfa: 166 
Número cortes-beta: 18 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (1 3) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ X X _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 14) 
Jogador 2: (10 10 14) 
 
 Melhor valor: 4 
Número nós analisados: 2882 
Número cortes-alfa: 152 
Número cortes-beta: 101 
Limite de tempo alcançado: Não 
Memoização ativada 2 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (10 9) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ X X _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 14) 
Jogador 2: (10 9 14) 
 
 Melhor valor: 0 
Número nós analisados: 5028 
Número cortes-alfa: 664 
Número cortes-beta: 129 
Limite de tempo alcançado: Não 
Memoização ativada 12 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (3 4) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ X X _ _ _ _ _ _ _ _ _ _ _
_ _ _ X X _ _ _ _ _ _ _ _ _
_ _ _ X X _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 14) 
Jogador 2: (10 9 14) 
 
 Melhor valor: 4 
Número nós analisados: 13519 
Número cortes-alfa: 523 
Número cortes-beta: 407 
Limite de tempo alcançado: Não 
Memoização ativada 20 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (11 6) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ X X _ _ _ _ _ _ _ _ _ _ _
_ _ _ X X _ _ _ _ _ _ _ _ _
_ _ _ X X _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ O _ _
_ _ _ _ _ _ _ _ _ _ _ O O _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 14) 
Jogador 2: (10 9 13) 
 
 Melhor valor: 0 
Número nós analisados: 15376 
Número cortes-alfa: 761 
Número cortes-beta: 448 
Limite de tempo alcançado: Não 
Memoização ativada 22 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (0 4) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ X X _ _ _ _ _ _ _ _ _ _ _
X _ _ X X _ _ _ _ _ _ _ _ _
X X _ X X _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ O _ _
_ _ _ _ _ _ _ _ _ _ _ O O _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 13) 
Jogador 2: (10 9 13) 
 
 Melhor valor: 4 
Número nós analisados: 26393 
Número cortes-alfa: 1009 
Número cortes-beta: 687 
Limite de tempo alcançado: Não 
Memoização ativada 25 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (11 3) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ X X _ _ _ _ _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ _ _ _ _ _ _ O _
_ X _ _ _ _ _ _ _ _ _ O _ _
_ _ _ _ _ _ _ _ _ _ _ O O _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 13) 
Jogador 2: (10 9 12) 
 
 Melhor valor: 0 
Número nós analisados: 25712 
Número cortes-alfa: 669 
Número cortes-beta: 688 
Limite de tempo alcançado: Não 
Memoização ativada 20 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (0 7) 
X X _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ X X _ _ _ _ _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ _ _ _ _ _ _ O _
_ X _ _ _ _ _ _ _ _ _ O _ _
X _ _ _ _ _ _ _ _ _ _ O O _
X X _ _ _ _ _ _ _ _ _ _ O _
_ X _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 12) 
Jogador 2: (10 9 12) 
 
 Melhor valor: 4 
Número nós analisados: 24575 
Número cortes-alfa: 1163 
Número cortes-beta: 625 
Limite de tempo alcançado: Não 
Memoização ativada 21 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (11 0) 
X X _ _ _ _ _ _ _ _ _ O _ _
X X _ _ _ _ _ _ _ _ _ O O _
_ _ X X _ _ _ _ _ _ _ _ O _
_ X X _ _ _ _ _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ _ _ _ _ _ _ O _
_ X _ _ _ _ _ _ _ _ _ O _ _
X _ _ _ _ _ _ _ _ _ _ O O _
X X _ _ _ _ _ _ _ _ _ _ O _
_ X _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 12) 
Jogador 2: (10 9 11) 
 
 Melhor valor: 0 
Número nós analisados: 33356 
Número cortes-alfa: 1429 
Número cortes-beta: 864 
Limite de tempo alcançado: Não 
Memoização ativada 38 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (4 1) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ _ _ _ _ O O _
_ _ X X _ _ _ _ _ _ _ _ O _
_ X X _ _ _ _ _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ _ _ _ _ _ _ O _
_ X _ _ _ _ _ _ _ _ _ O _ _
X _ _ _ _ _ _ _ _ _ _ O O _
X X _ _ _ _ _ _ _ _ _ _ O _
_ X _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 11) 
Jogador 2: (10 9 11) 
 
 Melhor valor: 4 
Número nós analisados: 28239 
Número cortes-alfa: 878 
Número cortes-beta: 717 
Limite de tempo alcançado: Não 
Memoização ativada 37 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (7 9) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ _ _ _ _ O O _
_ _ X X _ _ _ _ _ _ _ _ O _
_ X X _ _ _ _ _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ _ _ _ _ _ _ O _
_ X _ _ _ _ _ _ _ _ _ O _ _
X _ _ _ _ _ _ _ _ _ _ O O _
X X _ _ _ _ _ _ O O _ _ O _
_ X _ _ _ _ _ O O _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 8 11) 
Jogador 2: (10 9 10) 
 
 Melhor valor: 0 
Número nós analisados: 17210 
Número cortes-alfa: 1531 
Número cortes-beta: 456 
Limite de tempo alcançado: Não 
Memoização ativada 38 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (7 1) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ _ _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ _ _ _ _ _ _ O _
_ X _ _ _ _ _ _ _ _ _ O _ _
X _ _ _ _ _ _ _ _ _ _ O O _
X X _ _ _ _ _ _ O O _ _ O _
_ X _ _ _ _ _ O O _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 11) 
Jogador 2: (10 9 10) 
 
 Melhor valor: 4 
Número nós analisados: 29984 
Número cortes-alfa: 1433 
Número cortes-beta: 795 
Limite de tempo alcançado: Não 
Memoização ativada 45 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (8 11) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ _ _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ _ _ _ _ _ _ O _
_ X _ _ _ _ _ _ _ _ _ O _ _
X _ _ _ _ _ _ _ _ _ _ O O _
X X _ _ _ _ _ _ O O _ _ O _
_ X _ _ _ _ _ O O _ O O _ _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ O O _ _ O _
_ _ _ _ _ _ _ _ O O _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 11) 
Jogador 2: (10 8 10) 
 
 Melhor valor: 0 
Número nós analisados: 37333 
Número cortes-alfa: 1194 
Número cortes-beta: 1025 
Limite de tempo alcançado: Não 
Memoização ativada 49 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (2 10) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ _ _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ _ _ _ _ _ _ O _
_ X _ _ _ _ _ _ _ _ _ O _ _
X _ _ _ _ _ _ _ _ _ _ O O _
X X _ _ _ _ _ _ O O _ _ O _
_ X _ X X _ _ O O _ O O _ _
_ _ X X _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ O O _ _ O _
_ _ _ _ _ _ _ _ O O _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 10) 
Jogador 2: (10 8 10) 
 
 Melhor valor: 4 
Número nós analisados: 24844 
Número cortes-alfa: 1246 
Número cortes-beta: 707 
Limite de tempo alcançado: Não 
Memoização ativada 32 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (5 7) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ _ _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ _ _ _ _ _ _ O _
_ X _ _ _ _ _ _ _ _ _ O _ _
X _ _ _ _ O O _ _ _ _ O O _
X X _ _ _ O O _ O O _ _ O _
_ X _ X X _ _ O O _ O O _ _
_ _ X X _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ O O _ _ O _
_ _ _ _ _ _ _ _ O O _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 7 10) 
Jogador 2: (10 7 10) 
 
 Melhor valor: 0 
Número nós analisados: 20910 
Número cortes-alfa: 1131 
Número cortes-beta: 702 
Limite de tempo alcançado: Não 
Memoização ativada 33 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (0 11) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ _ _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ _ _ _ _ _ _ O _
_ X _ _ _ _ _ _ _ _ _ O _ _
X _ _ _ _ O O _ _ _ _ O O _
X X _ _ _ O O _ O O _ _ O _
_ X _ X X _ _ O O _ O O _ _
_ _ X X _ _ _ _ _ _ O O _ _
X X _ _ _ _ _ _ O O _ _ O _
X X _ _ _ _ _ _ O O _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 6 10) 
Jogador 2: (10 7 10) 
 
 Melhor valor: 4 
Número nós analisados: 30248 
Número cortes-alfa: 1883 
Número cortes-beta: 918 
Limite de tempo alcançado: Não 
Memoização ativada 84 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (4 11) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ _ _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ _ _ _ _ _ _ O _
_ X _ _ _ _ _ _ _ _ _ O _ _
X _ _ _ _ O O _ _ _ _ O O _
X X _ _ _ O O _ O O _ _ O _
_ X _ X X _ _ O O _ O O _ _
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X _ _ _ _ _ _ O O _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 6 10) 
Jogador 2: (10 7 9) 
 
 Melhor valor: 0 
Número nós analisados: 16717 
Número cortes-alfa: 1097 
Número cortes-beta: 583 
Limite de tempo alcançado: Não 
Memoização ativada 54 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (5 6) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ _ _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ X X _ _ _ _ O _
_ X _ _ _ X X _ _ _ _ O _ _
X _ _ _ _ O O _ _ _ _ O O _
X X _ _ _ O O _ O O _ _ O _
_ X _ X X _ _ O O _ O O _ _
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X _ _ _ _ _ _ O O _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 6 9) 
Jogador 2: (10 7 9) 
 
 Melhor valor: 4 
Número nós analisados: 11625 
Número cortes-alfa: 1719 
Número cortes-beta: 388 
Limite de tempo alcançado: Não 
Memoização ativada 110 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (7 6) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ _ _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ X X O O _ _ O _
_ X _ _ _ X X O O _ _ O _ _
X _ _ _ _ O O _ _ _ _ O O _
X X _ _ _ O O _ O O _ _ O _
_ X _ X X _ _ O O _ O O _ _
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X _ _ _ _ _ _ O O _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 6 9) 
Jogador 2: (10 7 8) 
 
 Melhor valor: 0 
Número nós analisados: 14191 
Número cortes-alfa: 959 
Número cortes-beta: 524 
Limite de tempo alcançado: Não 
Memoização ativada 73 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (6 3) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ X _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ X X O O _ _ O _
_ X _ _ _ X X O O _ _ O _ _
X _ _ _ _ O O _ _ _ _ O O _
X X _ _ _ O O _ O O _ _ O _
_ X _ X X _ _ O O _ O O _ _
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X _ _ _ _ _ _ O O _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (9 6 9) 
Jogador 2: (10 7 8) 
 
 Melhor valor: 1 
Número nós analisados: 12392 
Número cortes-alfa: 2090 
Número cortes-beta: 425 
Limite de tempo alcançado: Não 
Memoização ativada 237 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (2 12) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ X _ _ _ _ O _ _
X _ _ X X _ _ _ _ _ _ O O _
X X _ X X _ X X O O _ _ O _
_ X _ _ _ X X O O _ _ O _ _
X _ _ _ _ O O _ _ _ _ O O _
X X _ _ _ O O _ O O _ _ O _
_ X _ X X _ _ O O _ O O _ _
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ _ _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (9 6 9) 
Jogador 2: (10 6 8) 
 
 Melhor valor: 3 
Número nós analisados: 11951 
Número cortes-alfa: 517 
Número cortes-beta: 521 
Limite de tempo alcançado: Não 
Memoização ativada 109 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (9 3) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ X _ _ X _ O _ _
X _ _ X X _ _ _ _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ _ _ X X O O _ _ O _ _
X _ _ _ _ O O _ _ _ _ O O _
X X _ _ _ O O _ O O _ _ O _
_ X _ X X _ _ O O _ O O _ _
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ _ _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (9 6 8) 
Jogador 2: (10 6 8) 
 
 Melhor valor: -2 
Número nós analisados: 4212 
Número cortes-alfa: 925 
Número cortes-beta: 164 
Limite de tempo alcançado: Não 
Memoização ativada 152 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (6 4) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ X O O X _ O _ _
X _ _ X X _ O O _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ _ _ X X O O _ _ O _ _
X _ _ _ _ O O _ _ _ _ O O _
X X _ _ _ O O _ O O _ _ O _
_ X _ X X _ _ O O _ O O _ _
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ _ _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (9 6 8) 
Jogador 2: (10 6 7) 
 
 Melhor valor: 6 
Número nós analisados: 5395 
Número cortes-alfa: 275 
Número cortes-beta: 261 
Limite de tempo alcançado: Não 
Memoização ativada 69 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (9 6) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ X O O X _ O _ _
X _ _ X X _ O O _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ _ _ X X O O X _ O _ _
X _ _ _ _ O O _ _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O _ O O _ _
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ _ _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (9 6 7) 
Jogador 2: (10 6 7) 
 
 Melhor valor: -2 
Número nós analisados: 2354 
Número cortes-alfa: 648 
Número cortes-beta: 68 
Limite de tempo alcançado: Não 
Memoização ativada 64 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (2 7) 
X X _ _ _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ X O O X _ O _ _
X _ _ X X _ O O _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ O O X X O O X _ O _ _
X _ O O _ O O _ _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O _ O O _ _
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ _ _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (9 6 7) 
Jogador 2: (10 6 6) 
 
 Melhor valor: 6 
Número nós analisados: 6002 
Número cortes-alfa: 130 
Número cortes-beta: 329 
Limite de tempo alcançado: Não 
Memoização ativada 65 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (3 0) 
X X _ X _ X X _ _ _ _ O _ _
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ X O O X _ O _ _
X _ _ X X _ O O _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ O O X X O O X _ O _ _
X _ O O _ O O _ _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O _ O O _ _
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ _ _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (8 6 7) 
Jogador 2: (10 6 6) 
 
 Melhor valor: -2 
Número nós analisados: 2677 
Número cortes-alfa: 786 
Número cortes-beta: 75 
Limite de tempo alcançado: Não 
Memoização ativada 242 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (13 0) 
X X _ X _ X X _ _ _ _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ X O O X _ O _ _
X _ _ X X _ O O _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ O O X X O O X _ O _ _
X _ O O _ O O _ _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O _ O O _ _
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ _ _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (8 6 7) 
Jogador 2: (9 6 6) 
 
 Melhor valor: 3 
Número nós analisados: 7097 
Número cortes-alfa: 362 
Número cortes-beta: 323 
Limite de tempo alcançado: Não 
Memoização ativada 172 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (9 9) 
X X _ X _ X X _ _ _ _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ X O O X _ O _ _
X _ _ X X _ O O _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ O O X X O O X _ O _ _
X _ O O _ O O _ _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O X O O _ _
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ _ _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (7 6 7) 
Jogador 2: (9 6 6) 
 
 Melhor valor: 1 
Número nós analisados: 2194 
Número cortes-alfa: 577 
Número cortes-beta: 59 
Limite de tempo alcançado: Não 
Memoização ativada 254 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (6 12) 
X X _ X _ X X _ _ _ _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ _ _ O _
_ X X _ _ _ X O O X _ O _ _
X _ _ X X _ O O _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ O O X X O O X _ O _ _
X _ O O _ O O _ _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O X O O _ _
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ O _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (7 6 7) 
Jogador 2: (8 6 6) 
 
 Melhor valor: 3 
Número nós analisados: 6749 
Número cortes-alfa: 376 
Número cortes-beta: 265 
Limite de tempo alcançado: Não 
Memoização ativada 97 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (10 2) 
X X _ X _ X X _ _ _ _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ _
X _ _ X X _ O O _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ O O X X O O X _ O _ _
X _ O O _ O O _ _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O X O O _ _
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ O _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (6 6 7) 
Jogador 2: (8 6 6) 
 
 Melhor valor: -2 
Número nós analisados: 2231 
Número cortes-alfa: 691 
Número cortes-beta: 58 
Limite de tempo alcançado: Não 
Memoização ativada 130 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (13 9) 
X X _ X _ X X _ _ _ _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ _
X _ _ X X _ O O _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ O O X X O O X _ O _ _
X _ O O _ O O _ _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O X O O _ O
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ O _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (6 6 7) 
Jogador 2: (7 6 6) 
 
 Melhor valor: 3 
Número nós analisados: 4931 
Número cortes-alfa: 203 
Número cortes-beta: 246 
Limite de tempo alcançado: Não 
Memoização ativada 88 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (9 0) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ _
X _ _ X X _ O O _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ O O X X O O X _ O _ _
X _ O O _ O O _ _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O X O O _ O
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ O _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (5 6 7) 
Jogador 2: (7 6 6) 
 
 Melhor valor: -2 
Número nós analisados: 689 
Número cortes-alfa: 204 
Número cortes-beta: 22 
Limite de tempo alcançado: Não 
Memoização ativada 41 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (13 3) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ O
X _ _ X X _ O O _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ O O X X O O X _ O _ _
X _ O O _ O O _ _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O X O O _ O
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ O _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (5 6 7) 
Jogador 2: (6 6 6) 
 
 Melhor valor: 3 
Número nós analisados: 2247 
Número cortes-alfa: 38 
Número cortes-beta: 164 
Limite de tempo alcançado: Não 
Memoização ativada 47 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (4 7) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ O
X _ _ X X _ O O _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ O O X X O O X _ O _ _
X _ O O X O O _ _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O X O O _ O
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ O _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (4 6 7) 
Jogador 2: (6 6 6) 
 
 Melhor valor: -2 
Número nós analisados: 792 
Número cortes-alfa: 260 
Número cortes-beta: 16 
Limite de tempo alcançado: Não 
Memoização ativada 82 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (13 6) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ O
X _ _ X X _ O O _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ O O X X O O X _ O _ O
X _ O O X O O _ _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O X O O _ O
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ O _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (4 6 7) 
Jogador 2: (5 6 6) 
 
 Melhor valor: 3 
Número nós analisados: 1646 
Número cortes-alfa: 18 
Número cortes-beta: 137 
Limite de tempo alcançado: Não 
Memoização ativada 58 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (7 7) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ O
X _ _ X X _ O O _ X X O O _
X X _ X X _ X X O O X _ O _
_ X _ O O X X O O X _ O _ O
X _ O O X O O X _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O X O O _ O
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ O _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (3 6 7) 
Jogador 2: (5 6 6) 
 
 Melhor valor: -2 
Número nós analisados: 440 
Número cortes-alfa: 149 
Número cortes-beta: 8 
Limite de tempo alcançado: Não 
Memoização ativada 80 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (5 5) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ O
X _ _ X X _ O O _ X X O O _
X X _ X X O X X O O X _ O _
_ X _ O O X X O O X _ O _ O
X _ O O X O O X _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O X O O _ O
_ _ X X _ O O _ _ _ O O _ _
X X _ _ O O _ _ O O _ _ O _
X X O O _ _ O _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (3 6 7) 
Jogador 2: (4 6 6) 
 
 Melhor valor: 3 
Número nós analisados: 975 
Número cortes-alfa: 6 
Número cortes-beta: 96 
Limite de tempo alcançado: Não 
Memoização ativada 48 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (6 11) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ O
X _ _ X X _ O O _ X X O O _
X X _ X X O X X O O X _ O _
_ X _ O O X X O O X _ O _ O
X _ O O X O O X _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O X O O _ O
_ _ X X _ O O X X _ O O _ _
X X _ _ O O X X O O _ _ O _
X X O O _ _ O _ O O _ _ O O
_ _ O O _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (3 6 6) 
Jogador 2: (4 6 6) 
 
 Melhor valor: -2 
Número nós analisados: 214 
Número cortes-alfa: 79 
Número cortes-beta: 4 
Limite de tempo alcançado: Não 
Memoização ativada 20 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (5 13) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ O
X _ _ X X _ O O _ X X O O _
X X _ X X O X X O O X _ O _
_ X _ O O X X O O X _ O _ O
X _ O O X O O X _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O X O O _ O
_ _ X X _ O O X X _ O O _ _
X X _ _ O O X X O O _ _ O _
X X O O _ _ O _ O O _ _ O O
_ _ O O _ O _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (3 6 6) 
Jogador 2: (3 6 6) 
 
 Melhor valor: 3 
Número nós analisados: 721 
Número cortes-alfa: 20 
Número cortes-beta: 86 
Limite de tempo alcançado: Não 
Memoização ativada 47 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (5 12) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ O
X _ _ X X _ O O _ X X O O _
X X _ X X O X X O O X _ O _
_ X _ O O X X O O X _ O _ O
X _ O O X O O X _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O X O O _ O
_ _ X X _ O O X X _ O O _ _
X X _ _ O O X X O O _ _ O _
X X O O _ X O _ O O _ _ O O
_ _ O O _ O _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (2 6 6) 
Jogador 2: (3 6 6) 
 
 Melhor valor: -2 
Número nós analisados: 160 
Número cortes-alfa: 61 
Número cortes-beta: 4 
Limite de tempo alcançado: Não 
Memoização ativada 13 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (10 13) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ O
X _ _ X X _ O O _ X X O O _
X X _ X X O X X O O X _ O _
_ X _ O O X X O O X _ O _ O
X _ O O X O O X _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ _ O O X O O _ O
_ _ X X _ O O X X _ O O _ _
X X _ _ O O X X O O _ _ O _
X X O O _ X O _ O O _ _ O O
_ _ O O _ O _ _ _ _ O _ _ O
Peças disponiveis: 
Jogador 1: (2 6 6) 
Jogador 2: (2 6 6) 
 
 Melhor valor: 3 
Número nós analisados: 418 
Número cortes-alfa: 10 
Número cortes-beta: 58 
Limite de tempo alcançado: Não 
Memoização ativada 24 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (6 9) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ O
X _ _ X X _ O O _ X X O O _
X X _ X X O X X O O X _ O _
_ X _ O O X X O O X _ O _ O
X _ O O X O O X _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ X O O X O O _ O
_ _ X X _ O O X X _ O O _ _
X X _ _ O O X X O O _ _ O _
X X O O _ X O _ O O _ _ O O
_ _ O O _ O _ _ _ _ O _ _ O
Peças disponiveis: 
Jogador 1: (1 6 6) 
Jogador 2: (2 6 6) 
 
 Melhor valor: -3 
Número nós analisados: 76 
Número cortes-alfa: 6 
Número cortes-beta: 5 
Limite de tempo alcançado: Não 
Memoização ativada 13 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (2 5) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ O
X _ _ X X _ O O _ X X O O _
X X O X X O X X O O X _ O _
_ X _ O O X X O O X _ O _ O
X _ O O X O O X _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ X O O X O O _ O
_ _ X X _ O O X X _ O O _ _
X X _ _ O O X X O O _ _ O _
X X O O _ X O _ O O _ _ O O
_ _ O O _ O _ _ _ _ O _ _ O
Peças disponiveis: 
Jogador 1: (1 6 6) 
Jogador 2: (1 6 6) 
 
 Melhor valor: 3 
Número nós analisados: 59 
Número cortes-alfa: 2 
Número cortes-beta: 5 
Limite de tempo alcançado: Não 
Memoização ativada 10 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (4 13) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ _ _ X X _ X _ O _
_ X X _ _ _ X O O X _ O _ O
X _ _ X X _ O O _ X X O O _
X X O X X O X X O O X _ O _
_ X _ O O X X O O X _ O _ O
X _ O O X O O X _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ X O O X O O _ O
_ _ X X _ O O X X _ O O _ _
X X _ _ O O X X O O _ _ O _
X X O O _ X O _ O O _ _ O O
_ _ O O X O _ _ _ _ O _ _ O
Peças disponiveis: 
Jogador 1: (0 6 6) 
Jogador 2: (1 6 6) 
 
 Melhor valor: -3 
Número nós analisados: 13 
Número cortes-alfa: 0 
Número cortes-beta: 1 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (4 3) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ O O X X _ X _ O _
_ X X _ O O X O O X _ O _ O
X _ _ X X _ O O _ X X O O _
X X O X X O X X O O X _ O _
_ X _ O O X X O O X _ O _ O
X _ O O X O O X _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ X O O X O O _ O
_ _ X X _ O O X X _ O O _ _
X X _ _ O O X X O O _ _ O _
X X O O _ X O _ O O _ _ O O
_ _ O O X O _ _ _ _ O _ _ O
Peças disponiveis: 
Jogador 1: (0 6 6) 
Jogador 2: (1 6 5) 
 
 Melhor valor: 3 
Número nós analisados: 9 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 

Jogador 1 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (7 13) 
X X _ X _ X X _ _ X _ O _ O
X X _ _ X X _ X X _ _ O O _
_ _ X X _ O O X X _ X _ O _
_ X X _ O O X O O X _ O _ O
X _ _ X X _ O O _ X X O O _
X X O X X O X X O O X _ O _
_ X _ O O X X O O X _ O _ O
X _ O O X O O X _ X X O O _
X X _ _ _ O O _ O O X _ O _
_ X _ X X _ X O O X O O _ O
_ _ X X _ O O X X _ O O _ _
X X _ _ O O X X O O _ _ O _
X X O O _ X O _ O O _ _ O O
_ _ O O X O _ O _ _ O _ _ O
Peças disponiveis: 
Jogador 1: (0 6 6) 
Jogador 2: (0 6 5) 
 
 Melhor valor: 4 
Número nós analisados: 5 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Pontuações: 
Jogador 1: 48 pontos 
Jogador 2: 44 pontos
```

### Resultado 6: 20 segundo (profundidade máxima 5)

```
/////////////////////////////////////////////////////////////////////////
Jogo: Computador VS Computador 
Tempo limite: 20000 milisegundos 
/////////////////////////////////////////////////////////////////////////
Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (0 0) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
Peças disponiveis: 
Jogador 1: (10 10 14) 
Jogador 2: (10 10 15) 
 
 Melhor valor: 4 
Número nós analisados: 505 
Número cortes-alfa: 31 
Número cortes-beta: 22 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (12 11) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 14) 
Jogador 2: (10 10 14) 
 
 Melhor valor: 0 
Número nós analisados: 1160 
Número cortes-alfa: 249 
Número cortes-beta: 28 
Limite de tempo alcançado: Não 
Memoização ativada 5 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (0 3) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 13) 
Jogador 2: (10 10 14) 
 
 Melhor valor: 4 
Número nós analisados: 5457 
Número cortes-alfa: 351 
Número cortes-beta: 181 
Limite de tempo alcançado: Não 
Memoização ativada 9 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (10 10) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ O O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 13) 
Jogador 2: (10 10 13) 
 
 Melhor valor: 0 
Número nós analisados: 5034 
Número cortes-alfa: 279 
Número cortes-beta: 168 
Limite de tempo alcançado: Não 
Memoização ativada 6 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (2 6) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ X X _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ O O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 12) 
Jogador 2: (10 10 13) 
 
 Melhor valor: 4 
Número nós analisados: 8747 
Número cortes-alfa: 980 
Número cortes-beta: 223 
Limite de tempo alcançado: Não 
Memoização ativada 27 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (8 9) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ X X _ _ _ _ _ _ _ _ _
_ _ X X _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ _ _ _ _ _
_ _ _ _ _ _ _ _ _ O O _ _ _
_ _ _ _ _ _ _ _ O O _ O O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 12) 
Jogador 2: (10 10 12) 
 
 Melhor valor: 0 
Número nós analisados: 21402 
Número cortes-alfa: 1629 
Número cortes-beta: 570 
Limite de tempo alcançado: Não 
Memoização ativada 41 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (5 6) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ X X _ _ _ _ _ _ _ _ _
_ _ X X _ X _ _ _ _ _ _ _ _
_ _ _ _ _ X X _ _ _ _ _ _ _
_ _ _ _ _ _ X _ _ O O _ _ _
_ _ _ _ _ _ _ _ O O _ O O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 11) 
Jogador 2: (10 10 12) 
 
 Melhor valor: 4 
Número nós analisados: 27416 
Número cortes-alfa: 868 
Número cortes-beta: 733 
Limite de tempo alcançado: Não 
Memoização ativada 19 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (11 7) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ X X _ _ _ _ _ _ _ _ _
_ _ X X _ X _ _ _ _ _ _ O O
_ _ _ _ _ X X _ _ _ _ O O _
_ _ _ _ _ _ X _ _ O O _ _ _
_ _ _ _ _ _ _ _ O O _ O O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 11) 
Jogador 2: (10 10 11) 
 
 Melhor valor: 0 
Número nós analisados: 26335 
Número cortes-alfa: 2411 
Número cortes-beta: 686 
Limite de tempo alcançado: Não 
Memoização ativada 47 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (5 3) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ X _ _ _ _ _ _ _ _
X X _ _ _ X X _ _ _ _ _ _ _
_ X _ X X _ X _ _ _ _ _ _ _
_ _ X X _ X _ _ _ _ _ _ O O
_ _ _ _ _ X X _ _ _ _ O O _
_ _ _ _ _ _ X _ _ O O _ _ _
_ _ _ _ _ _ _ _ O O _ O O _
_ _ _ _ _ _ _ _ _ _ O O _ _
_ _ _ _ _ _ _ _ _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 10) 
Jogador 2: (10 10 11) 
 
 Melhor valor: 4 
Número nós analisados: 51351 
Número cortes-alfa: 1145 
Número cortes-beta: 1195 
Limite de tempo alcançado: Não 
Memoização ativada 47 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (6 10) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ _ _ _ _ _ _ _ _ _ _
_ X _ _ _ _ _ _ _ _ _ _ _ _
X _ _ _ _ X _ _ _ _ _ _ _ _
X X _ _ _ X X _ _ _ _ _ _ _
_ X _ X X _ X _ _ _ _ _ _ _
_ _ X X _ X _ _ _ _ _ _ O O
_ _ _ _ _ X X _ _ _ _ O O _
_ _ _ _ _ _ X _ _ O O _ _ _
_ _ _ _ _ _ _ _ O O _ O O _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ _ O O _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 10) 
Jogador 2: (10 9 11) 
 
 Melhor valor: 0 
Número nós analisados: 30047 
Número cortes-alfa: 1598 
Número cortes-beta: 817 
Limite de tempo alcançado: Não 
Memoização ativada 58 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (3 2) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ X _ X X _ _ _ _ _ _ _ _ _
X _ _ _ _ X _ _ _ _ _ _ _ _
X X _ _ _ X X _ _ _ _ _ _ _
_ X _ X X _ X _ _ _ _ _ _ _
_ _ X X _ X _ _ _ _ _ _ O O
_ _ _ _ _ X X _ _ _ _ O O _
_ _ _ _ _ _ X _ _ O O _ _ _
_ _ _ _ _ _ _ _ O O _ O O _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ _ O O _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 9) 
Jogador 2: (10 9 11) 
 
 Melhor valor: 4 
Número nós analisados: 44887 
Número cortes-alfa: 1452 
Número cortes-beta: 1157 
Limite de tempo alcançado: Não 
Memoização ativada 56 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (7 6) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ X _ X X _ _ _ _ _ _ _ _ _
X _ _ _ _ X _ _ _ _ _ _ _ _
X X _ _ _ X X _ _ _ _ _ _ _
_ X _ X X _ X _ _ _ _ _ _ _
_ _ X X _ X _ O O _ _ _ O O
_ _ _ _ _ X X O O _ _ O O _
_ _ _ _ _ _ X _ _ O O _ _ _
_ _ _ _ _ _ _ _ O O _ O O _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ _ O O _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 9) 
Jogador 2: (10 8 11) 
 
 Melhor valor: 0 
Número nós analisados: 43565 
Número cortes-alfa: 2173 
Número cortes-beta: 1159 
Limite de tempo alcançado: Não 
Memoização ativada 61 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (2 9) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ X _ X X _ _ _ _ _ _ _ _ _
X _ _ _ _ X _ _ _ _ _ _ _ _
X X _ _ _ X X _ _ _ _ _ _ _
_ X _ X X _ X _ _ _ _ _ _ _
_ _ X X _ X _ O O _ _ _ O O
_ _ _ _ _ X X O O _ _ O O _
_ _ _ X X _ X _ _ O O _ _ _
_ _ X X _ _ _ _ O O _ O O _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ _ O O _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 8) 
Jogador 2: (10 8 11) 
 
 Melhor valor: 4 
Número nós analisados: 20825 
Número cortes-alfa: 965 
Número cortes-beta: 626 
Limite de tempo alcançado: Não 
Memoização ativada 31 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (9 4) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ X _ X X _ _ _ _ _ _ _ _ _
X _ _ _ _ X _ _ _ _ _ _ _ _
X X _ _ _ X X _ _ O O _ _ _
_ X _ X X _ X _ _ O O _ _ _
_ _ X X _ X _ O O _ _ _ O O
_ _ _ _ _ X X O O _ _ O O _
_ _ _ X X _ X _ _ O O _ _ _
_ _ X X _ _ _ _ O O _ O O _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ _ O O _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 8) 
Jogador 2: (10 7 11) 
 
 Melhor valor: 0 
Número nós analisados: 23345 
Número cortes-alfa: 1168 
Número cortes-beta: 736 
Limite de tempo alcançado: Não 
Memoização ativada 37 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-V jogada na posição (0 6) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ X _ X X _ _ _ _ _ _ _ _ _
X _ _ _ _ X _ _ _ _ _ _ _ _
X X _ _ _ X X _ _ O O _ _ _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ _
_ _ X X _ _ _ _ O O _ O O _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ _ O O _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 7) 
Jogador 2: (10 7 11) 
 
 Melhor valor: 4 
Número nós analisados: 23783 
Número cortes-alfa: 1409 
Número cortes-beta: 745 
Limite de tempo alcançado: Não 
Memoização ativada 73 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (11 2) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ _ _ _ _ _ _ _
_ X _ X X _ _ _ _ _ _ O _ _
X _ _ _ _ X _ _ _ _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ _
_ _ X X _ _ _ _ O O _ O O _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ _ O O _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 7) 
Jogador 2: (10 7 10) 
 
 Melhor valor: 0 
Número nós analisados: 19744 
Número cortes-alfa: 987 
Número cortes-beta: 664 
Limite de tempo alcançado: Não 
Memoização ativada 41 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (6 2) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ X X _ _ _ _ _
_ X _ X X _ X X _ _ _ O _ _
X _ _ _ _ X _ _ _ _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ _
_ _ X X _ _ _ _ O O _ O O _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ _ O O _ _ _ _ O _
_ _ _ _ _ _ _ _ _ _ _ _ O O
_ _ _ _ _ _ _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 6) 
Jogador 2: (10 7 10) 
 
 Melhor valor: 4 
Número nós analisados: 18002 
Número cortes-alfa: 1539 
Número cortes-beta: 600 
Limite de tempo alcançado: Não 
Memoização ativada 74 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (4 12) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ X X _ _ _ _ _
_ X _ X X _ X X _ _ _ O _ _
X _ _ _ _ X _ _ _ _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ _
_ _ X X _ _ _ _ O O _ O O _
_ _ _ _ _ _ O O _ _ O O _ _
_ _ _ _ _ _ O O _ _ _ _ O _
_ _ _ _ O O _ _ _ _ _ _ O O
_ _ _ _ O O _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 10 6) 
Jogador 2: (10 6 10) 
 
 Melhor valor: 0 
Número nós analisados: 22693 
Número cortes-alfa: 1971 
Número cortes-beta: 745 
Limite de tempo alcançado: Não 
Memoização ativada 112 vezes

Turno do Jogador 1 
 ------------------- 
PECA-B jogada na posição (0 10) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ X X _ _ _ _ _
_ X _ X X _ X X _ _ _ O _ _
X _ _ _ _ X _ _ _ _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ _
_ _ X X _ _ _ _ O O _ O O _
X X _ _ _ _ O O _ _ O O _ _
X X _ _ _ _ O O _ _ _ _ O _
_ _ _ _ O O _ _ _ _ _ _ O O
_ _ _ _ O O _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 6) 
Jogador 2: (10 6 10) 
 
 Melhor valor: 4 
Número nós analisados: 11302 
Número cortes-alfa: 868 
Número cortes-beta: 435 
Limite de tempo alcançado: Não 
Memoização ativada 71 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-H jogada na posição (7 3) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ X X _ _ _ _ _
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ _
_ _ X X _ _ _ _ O O _ O O _
X X _ _ _ _ O O _ _ O O _ _
X X _ _ _ _ O O _ _ _ _ O _
_ _ _ _ O O _ _ _ _ _ _ O O
_ _ _ _ O O _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (10 9 6) 
Jogador 2: (10 6 9) 
 
 Melhor valor: 0 
Número nós analisados: 16761 
Número cortes-alfa: 966 
Número cortes-beta: 689 
Limite de tempo alcançado: Não 
Memoização ativada 98 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (7 9) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ X X _ _ _ _ _
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ _
_ _ X X _ _ _ X O O _ O O _
X X _ _ _ _ O O _ _ O O _ _
X X _ _ _ _ O O _ _ _ _ O _
_ _ _ _ O O _ _ _ _ _ _ O O
_ _ _ _ O O _ _ _ _ _ _ _ O
Peças disponiveis: 
Jogador 1: (9 9 6) 
Jogador 2: (10 6 9) 
 
 Melhor valor: 1 
Número nós analisados: 6036 
Número cortes-alfa: 938 
Número cortes-beta: 245 
Limite de tempo alcançado: Não 
Memoização ativada 77 vezes

Turno do Jogador 2 
 ------------------- 
PECA-C-V jogada na posição (9 11) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ X X _ _ _ _ _
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ _
_ _ X X _ _ _ X O O _ O O _
X X _ _ _ _ O O _ _ O O _ _
X X _ _ _ _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ _ _ O _ _ O
Peças disponiveis: 
Jogador 1: (9 9 6) 
Jogador 2: (10 6 8) 
 
 Melhor valor: 3 
Número nós analisados: 8680 
Número cortes-alfa: 805 
Número cortes-beta: 376 
Limite de tempo alcançado: Não 
Memoização ativada 66 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (3 11) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ X X _ _ _ _ _
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ _
_ _ X X _ _ _ X O O _ O O _
X X _ _ X X O O _ _ O O _ _
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ _ _ O _ _ O
Peças disponiveis: 
Jogador 1: (9 9 5) 
Jogador 2: (10 6 8) 
 
 Melhor valor: 1 
Número nós analisados: 5364 
Número cortes-alfa: 489 
Número cortes-beta: 278 
Limite de tempo alcançado: Não 
Memoização ativada 120 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (13 8) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ X X _ _ _ _ _
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ O
_ _ X X _ _ _ X O O _ O O _
X X _ _ X X O O _ _ O O _ _
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ _ _ O _ _ O
Peças disponiveis: 
Jogador 1: (9 9 5) 
Jogador 2: (9 6 8) 
 
 Melhor valor: 0 
Número nós analisados: 1569 
Número cortes-alfa: 248 
Número cortes-beta: 92 
Limite de tempo alcançado: Não 
Memoização ativada 59 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (8 10) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ X X _ _ _ _ _
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ O
_ _ X X _ _ _ X O O _ O O _
X X _ _ X X O O X _ O O _ _
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ _ _ O _ _ O
Peças disponiveis: 
Jogador 1: (8 9 5) 
Jogador 2: (9 6 8) 
 
 Melhor valor: 1 
Número nós analisados: 1043 
Número cortes-alfa: 98 
Número cortes-beta: 70 
Limite de tempo alcançado: Não 
Memoização ativada 33 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (8 13) 
X _ _ _ _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ X X _ _ _ _ _
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ O
_ _ X X _ _ _ X O O _ O O _
X X _ _ X X O O X _ O O _ _
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ O _ O _ _ O
Peças disponiveis: 
Jogador 1: (8 9 5) 
Jogador 2: (8 6 8) 
 
 Melhor valor: 0 
Número nós analisados: 850 
Número cortes-alfa: 122 
Número cortes-beta: 67 
Limite de tempo alcançado: Não 
Memoização ativada 32 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (3 0) 
X _ _ X _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ X X _ _ _ _ _
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ O
_ _ X X _ _ _ X O O _ O O _
X X _ _ X X O O X _ O O _ _
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ O _ O _ _ O
Peças disponiveis: 
Jogador 1: (7 9 5) 
Jogador 2: (8 6 8) 
 
 Melhor valor: 1 
Número nós analisados: 947 
Número cortes-alfa: 77 
Número cortes-beta: 75 
Limite de tempo alcançado: Não 
Memoização ativada 43 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (13 10) 
X _ _ X _ _ _ _ _ _ _ _ _ _
X X _ _ X X _ X X _ _ _ _ _
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ O
_ _ X X _ _ _ X O O _ O O _
X X _ _ X X O O X _ O O _ O
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ O _ O _ _ O
Peças disponiveis: 
Jogador 1: (7 9 5) 
Jogador 2: (7 6 8) 
 
 Melhor valor: 0 
Número nós analisados: 461 
Número cortes-alfa: 54 
Número cortes-beta: 46 
Limite de tempo alcançado: Não 
Memoização ativada 23 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (9 0) 
X _ _ X _ _ _ _ _ X _ _ _ _
X X _ _ X X _ X X _ _ _ _ _
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ O
_ _ X X _ _ _ X O O _ O O _
X X _ _ X X O O X _ O O _ O
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ O _ O _ _ O
Peças disponiveis: 
Jogador 1: (6 9 5) 
Jogador 2: (7 6 8) 
 
 Melhor valor: 1 
Número nós analisados: 362 
Número cortes-alfa: 38 
Número cortes-beta: 33 
Limite de tempo alcançado: Não 
Memoização ativada 15 vezes

Turno do Jogador 2 
 ------------------- 
PECA-B jogada na posição (12 0) 
X _ _ X _ _ _ _ _ X _ _ O O
X X _ _ X X _ X X _ _ _ O O
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ _ O O _ _ O
_ _ X X _ _ _ X O O _ O O _
X X _ _ X X O O X _ O O _ O
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ O _ O _ _ O
Peças disponiveis: 
Jogador 1: (6 9 5) 
Jogador 2: (7 5 8) 
 
 Melhor valor: 0 
Número nós analisados: 220 
Número cortes-alfa: 27 
Número cortes-beta: 28 
Limite de tempo alcançado: Não 
Memoização ativada 23 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (8 8) 
X _ _ X _ _ _ _ _ X _ _ O O
X X _ _ X X _ X X _ _ _ O O
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ X O O _ _ O
_ _ X X _ _ _ X O O _ O O _
X X _ _ X X O O X _ O O _ O
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ O _ O _ _ O
Peças disponiveis: 
Jogador 1: (5 9 5) 
Jogador 2: (7 5 8) 
 
 Melhor valor: 1 
Número nós analisados: 103 
Número cortes-alfa: 1 
Número cortes-beta: 17 
Limite de tempo alcançado: Não 
Memoização ativada 8 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (10 1) 
X _ _ X _ _ _ _ _ X _ _ O O
X X _ _ X X _ X X _ O _ O O
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ _ _ O O
X X _ _ _ X X O O _ _ O O _
_ X _ X X _ X _ X O O _ _ O
_ _ X X _ _ _ X O O _ O O _
X X _ _ X X O O X _ O O _ O
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ O _ O _ _ O
Peças disponiveis: 
Jogador 1: (5 9 5) 
Jogador 2: (6 5 8) 
 
 Melhor valor: -1 
Número nós analisados: 51 
Número cortes-alfa: 5 
Número cortes-beta: 1 
Limite de tempo alcançado: Não 
Memoização ativada 13 vezes

Turno do Jogador 1 
 ------------------- 
PECA-C-H jogada na posição (9 7) 
X _ _ X _ _ _ _ _ X _ _ O O
X X _ _ X X _ X X _ O _ O O
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ X X O O
X X _ _ _ X X O O X X O O _
_ X _ X X _ X _ X O O _ _ O
_ _ X X _ _ _ X O O _ O O _
X X _ _ X X O O X _ O O _ O
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ O _ O _ _ O
Peças disponiveis: 
Jogador 1: (5 9 4) 
Jogador 2: (6 5 8) 
 
 Melhor valor: 1 
Número nós analisados: 47 
Número cortes-alfa: 0 
Número cortes-beta: 5 
Limite de tempo alcançado: Não 
Memoização ativada 12 vezes

Turno do Jogador 2 
 ------------------- 
PECA-A jogada na posição (5 9) 
X _ _ X _ _ _ _ _ X _ _ O O
X X _ _ X X _ X X _ O _ O O
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ X X O O
X X _ _ _ X X O O X X O O _
_ X _ X X _ X _ X O O _ _ O
_ _ X X _ O _ X O O _ O O _
X X _ _ X X O O X _ O O _ O
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ O _ O _ _ O
Peças disponiveis: 
Jogador 1: (5 9 4) 
Jogador 2: (5 5 8) 
 
 Melhor valor: -1 
Número nós analisados: 8 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (6 0) 
X _ _ X _ _ X _ _ X _ _ O O
X X _ _ X X _ X X _ O _ O O
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ _ _
X _ X X _ X _ O O _ X X O O
X X _ _ _ X X O O X X O O _
_ X _ X X _ X _ X O O _ _ O
_ _ X X _ O _ X O O _ O O _
X X _ _ X X O O X _ O O _ O
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ O _ O _ _ O
Peças disponiveis: 
Jogador 1: (4 9 4) 
Jogador 2: (5 5 8) 
 
 Melhor valor: 1 
Número nós analisados: 7 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 

Jogador 2 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (12 5) 
X _ _ X _ _ X _ _ X _ _ O O
X X _ _ X X _ X X _ O _ O O
_ X _ X X _ X X O O _ O _ _
X _ _ _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ X _
X _ X X _ X _ O O _ X X O O
X X _ _ _ X X O O X X O O _
_ X _ X X _ X _ X O O _ _ O
_ _ X X _ O _ X O O _ O O _
X X _ _ X X O O X _ O O _ O
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ O _ O _ _ O
Peças disponiveis: 
Jogador 1: (3 9 4) 
Jogador 2: (5 5 8) 
 
 Melhor valor: 2 
Número nós analisados: 6 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 

Jogador 2 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (2 3) 
X _ _ X _ _ X _ _ X _ _ O O
X X _ _ X X _ X X _ O _ O O
_ X _ X X _ X X O O _ O _ _
X _ X _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ X _
X _ X X _ X _ O O _ X X O O
X X _ _ _ X X O O X X O O _
_ X _ X X _ X _ X O O _ _ O
_ _ X X _ O _ X O O _ O O _
X X _ _ X X O O X _ O O _ O
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ O _ O _ _ O
Peças disponiveis: 
Jogador 1: (2 9 4) 
Jogador 2: (5 5 8) 
 
 Melhor valor: 3 
Número nós analisados: 7 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 

Jogador 2 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (11 8) 
X _ _ X _ _ X _ _ X _ _ O O
X X _ _ X X _ X X _ O _ O O
_ X _ X X _ X X O O _ O _ _
X _ X _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O _ O _
_ X _ X X _ X _ _ O O _ X _
X _ X X _ X _ O O _ X X O O
X X _ _ _ X X O O X X O O _
_ X _ X X _ X _ X O O X _ O
_ _ X X _ O _ X O O _ O O _
X X _ _ X X O O X _ O O _ O
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ O _ O _ _ O
Peças disponiveis: 
Jogador 1: (1 9 4) 
Jogador 2: (5 5 8) 
 
 Melhor valor: 4 
Número nós analisados: 6 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Turno do Jogador 2 
 ------------------- 

Jogador 2 não conseguiu efetuar jogada 
 ------------------- 
 
Turno do Jogador 1 
 ------------------- 
PECA-A jogada na posição (11 4) 
X _ _ X _ _ X _ _ X _ _ O O
X X _ _ X X _ X X _ O _ O O
_ X _ X X _ X X O O _ O _ _
X _ X _ _ X _ O O _ _ O O _
X X _ _ _ X X _ _ O O X O _
_ X _ X X _ X _ _ O O _ X _
X _ X X _ X _ O O _ X X O O
X X _ _ _ X X O O X X O O _
_ X _ X X _ X _ X O O X _ O
_ _ X X _ O _ X O O _ O O _
X X _ _ X X O O X _ O O _ O
X X _ X X _ O O _ O _ _ O _
_ _ _ _ O O _ _ _ O O _ O O
_ _ _ _ O O _ _ O _ O _ _ O
Peças disponiveis: 
Jogador 1: (0 9 4) 
Jogador 2: (5 5 8) 
 
 Melhor valor: 5 
Número nós analisados: 6 
Número cortes-alfa: 0 
Número cortes-beta: 0 
Limite de tempo alcançado: Não 
Memoização ativada 0 vezes

Pontuações: 
Jogador 1: 52 pontos 
Jogador 2: 57 pontos
```