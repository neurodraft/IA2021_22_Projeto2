(defun iniciar ()
  "Função que inicia o programa"
  (progn
   (definir-pasta)
   (menu-inicial)))

(defun definir-pasta ()
  "Pede o path da localização do projeto e compila os ficheiros puzzle.lisp e algoritmo.lisp"
  (progn
   (format t "Escreva o path da localizacao do projeto entre aspas~%")
   (format t "Exemplo: ''C:/Users/username/Desktop/''~%")
   (let ((path (read)))
     (load (compile-file (concatenate 'string path "puzzle.lisp")))
     (load (compile-file (concatenate 'string path "algoritmo.lisp")))
     (defparameter *path* path)
     path)))

(defun mostrar-menu-inicial ()
  "Imprime no listener o menu inicial"
  (progn
   (format t " ~% _____________________________________")
   (format t " ~%|                                     |")
   (format t " ~%|           JOGO DO BLOKUS            |")
   (format t " ~%|                                     |")
   (format t " ~%|     1 - Humano VS Computador        |")
   (format t " ~%|     2 - Computador VS Computador    |")
   (format t " ~%|     0 - Sair                        |")
   (format t " ~%|_____________________________________|")
   (format t " ~%                                       ")
   (format t " ~%-> Opção: ")))

(defun mostrar-menu-jogador ()
  "Imprime no listener o menu que permite ao jogador escolher qual o jogador que deseja ser"
  (progn
   (format t " ~% _____________________________________")
   (format t " ~%|                                     |")
   (format t " ~%|           JOGO DO BLOKUS            |")
   (format t " ~%|                                     |")
   (format t " ~%|    Qual o jogador que deseja?       |")
   (format t " ~%|                                     |")
   (format t " ~%|             1 - Jogador 1           |")
   (format t " ~%|             2 - Jogador 2           |")
   (format t " ~%|             0 - Voltar              |")
   (format t " ~%|                                     |")
   (format t " ~%|_____________________________________|")
   (format t " ~%                                       ")
   (format t " ~%-> Opção: ")))

(defun mostrar-menu-tempo-limite ()
  "Imprime no listener o menu que permite inserir o limite de tempo"
  (progn
   (format t " ~% _____________________________________")
   (format t " ~%|                                     |")
   (format t " ~%|           JOGO DO BLOKUS            |")
   (format t " ~%|                                     |")
   (format t " ~%|        Qual o tempo limite?         |")
   (format t " ~%|        Entre 1000 a 20000 ms.       |")
   (format t " ~%|                                     |")
   (format t " ~%|       0 - Voltar                    |")
   (format t " ~%|                                     |")
   (format t " ~%|_____________________________________|")
   (format t " ~%                                       ")
   (format t " ~%-> Tempo: ")))

(defun menu-inicial ()
  "Chama a função mostrar-menu-inicial, lê o input do utilizador e redireciona para o menu respetivo"
  (progn
   (mostrar-menu-inicial)
   (let ((option (read)))
     (cond
      ((eq option '1) (menu-escolher-jogador))
      ((eq option '2) (menu-limite-tempo))
      ((eq option '0) (format t "Até à próxima!"))
      (T (progn (format t "Opção inválida!") (menu-inicial)))))))

(defun menu-escolher-jogador ()
  (progn
   (mostrar-menu-jogador)
   (let ((option (read)))
     (cond
      ((eq option '1) (menu-limite-tempo '1))
      ((eq option '2) (menu-limite-tempo '2))
      ((eq option '0) (menu-inicIal))))))

(defun menu-limite-tempo (&optional jogador)
  "Chama a funçãoo mostrar-menu-tempo-limite e lê o input do utilizador"
  (progn (mostrar-menu-tempo-limite)
    (let ((option (read)))
      (cond
      ((or (not (numberp option)) (< option 1000) (> option 20000)) (format t "~% Opção inválida!~%") (menu-limite-tempo jogador))
      ((eq option '0) (menu-escolher-jogador))
      (T (cond 
        ((null jogador) (registar-cabecalho "Computador VS Computador" option ) (jogo-pc-vs-pc option))
        (T (registar-cabecalho "Humano VS Computador" option ) (jogo-humano-vs-pc jogador option))))))))


(defun jogo-humano-vs-pc (jogador-humano tempo-limite)
  (let ((jogador 1)
        (adversario 2)
        (no-atual (no-inicial))
        (profundidade (profundidade-max-para-tempo tempo-limite)))

    (loop do (progn
      (If (= jogador jogador-humano)
        (let ((jogada (jogada-humano no-atual jogador)))
          (if (not (null jogada))
            (setf no-atual jogada)
          )
        )
        (let ((jogada (jogada-computador no-atual jogador adversario profundidade tempo-limite)))
          (if (not (null jogada))
            (setf no-atual jogada)
          )
        ))             
                 (let ((temp jogador))
                   (setf jogador adversario)
                   (setf adversario temp)))
              while (not (and (null (sucessores no-atual 1 (operadores)))
                              (null (sucessores no-atual 2 (operadores))))))
             (mostrar-pontuacoes (no-estado no-atual))
             ))

(defun jogada-computador (no jogador adversario profundidade tempo-limite)
  (progn
    (reiniciar-valores)
    (definir-limite-tempo tempo-limite)
    (let ((valor (alfabeta
                    no
                    (criar-f-sucessores jogador adversario)
                    (criar-f-utilidade jogador adversario) profundidade)))
                (progn
                  (mostrar-turno-jogador jogador)
                (if valor
                  (progn                    
                    (mostrar-no (obter-melhor-jogada))
                      (mostrar-estatisticas valor (obter-nos-analisados) (obter-cortes-alfa) (obter-cortes-beta) (obter-limite-tempo-alcancado)))
                     (mostrar-jogador-passou jogador))))
                     (obter-melhor-jogada))
)             

(defun escolher-peca()
  (progn
      (format t " ~% Escolha uma ação: ")
      (format t " ~% ")
      (format t " ~% 1 - peça A")
      (format t " ~% 2 - peça B")
      (format t " ~% 3 - peça C-H")
      (format t " ~% 4 - peça C-V")
      (format t " ~% 0 - Passar o turno")
      (format t " ~% ")
      (format t " ~% -> Opção: ")
      (let ((option (read)))
        (cond
          ((eq option '1) 'peca-a)
          ((eq option '2) 'peca-b)
          ((eq option '3) 'peca-c-h)
          ((eq option '4) 'peca-c-v)
          ((eq option '0) nil)
          (T (progn (format t "Opção inválida!") (escolher-peca))))
          ))
)

(defun escolher-linha()
  (format t " ~% Escolha uma linha [0,13]: ")
  (let ((option (read)))
    (cond
      ((or (not (numberp option)) (< option 0) (> option 13)) (format t "~% Opção inválida!~%") (escolher-linha))
      (T option)
    )))


(defun escolher-coluna()
  (format t " ~% Escolha uma coluna [0,13]: ")
  (let ((option (read)))
    (cond
      ((or (not (numberp option)) (< option 0) (> option 13)) (format t "~% Opção inválida!~%") (escolher-coluna))
      (T option)
    )))    

(defun jogada-humano (no jogador)
  (let ((peca nil)
      (x nil)
      (y nil))
    (progn
      (setf peca (escolher-peca))
      (if (not (tem-peca peca (estado-pecas-jogador (no-estado no) jogador)))      
        (progn
          (format t " ~% A ~a não está disponivel!" peca)
          (jogada-humano no jogador)
        )
      )
      (setf x (escolher-linha))
      (setf y (escolher-coluna))
      (let ((casas-ocupadas (peca-casas-ocupadas x y (funcall peca))))
        (if (valida-casas (estado-tabuleiro (no-estado no)) casas-ocupadas jogador)
          (criar-no (criar-novo-estado (no-estado no) casas-ocupadas peca jogador) nil)
          (progn
            (format t " ~% Não é possivel jogar a ~a na posição ~a ~a!" peca x y)
            (jogada-humano no jogador)
          )
        )
      )
        )))

(defun jogo-pc-vs-pc (tempo-limite)
  (let ((jogador 1)
        (adversario 2)
        (no-atual (no-inicial))
        (profundidade (profundidade-max-para-tempo tempo-limite)))

    (loop do (progn
              (let ((jogada (jogada-computador no-atual jogador adversario profundidade tempo-limite)))
                (if (not (null jogada))
                  (setf no-atual jogada)
                )
              )               
                 (let ((temp jogador))
                   (setf jogador adversario)
                   (setf adversario temp)))
              while (not (and (null (sucessores no-atual 1 (operadores)))
                              (null (sucessores no-atual 2 (operadores))))))
             (mostrar-pontuacoes (no-estado no-atual))
             ))

(defun mostrar-jogador-passou(jogador)
  (format t "~%Jogador ~a não conseguiu efetuar jogada ~% ------------------- ~% ~%" jogador)
  (registar-jogador-passou jogador)
)

(defun registar-jogador-passou(jogador)
  (with-open-file (file (diretorio-resultados) :direction :output :if-exists :append :if-does-not-exist :create)
  (format file "~%Jogador ~a não conseguiu efetuar jogada ~% ------------------- ~% ~%" jogador))
)

(defun mostrar-estatisticas (melhor-valor nos-analisados cortes-alfa cortes-beta limite-tempo-alcancado)
  (format t "Melhor valor: ~a ~%" melhor-valor)
  (format t "Número nós analisados: ~a ~%" nos-analisados)
  (format t "Número cortes-alfa: ~a ~%" cortes-alfa)
  (format t "Número cortes-beta: ~a ~%" cortes-beta)
  (format t "Limite de tempo alcançado: ~a ~% ~%" (if limite-tempo-alcancado "Sim" "Não"))
  (registar-estatisticas melhor-valor nos-analisados cortes-alfa cortes-beta limite-tempo-alcancado))

(defun mostrar-turno-jogador(jogador)
  (format t "Turno do Jogador ~a ~% ------------------- ~%" jogador)
  (registar-turno-jogador jogador)
)

(defun registar-turno-jogador(jogador)
  (with-open-file (file (diretorio-resultados) :direction :output :if-exists :append :if-does-not-exist :create)
  (format file "Turno do Jogador ~a ~% ------------------- ~%" jogador))
)

(defun registar-estatisticas (melhor-valor nos-analisados cortes-alfa cortes-beta limite-tempo-alcancado)
  (with-open-file (file (diretorio-resultados) :direction :output :if-exists :append :if-does-not-exist :create)
  (format file "Melhor valor: ~a ~%" melhor-valor)
  (format file "Número nós analisados: ~a ~%" nos-analisados)
  (format file "Número cortes-alfa: ~a ~%" cortes-alfa)
  (format file "Número cortes-beta: ~a ~%" cortes-beta)
  (format file "Limite de tempo alcançado: ~a ~% ~%" (if limite-tempo-alcancado "Sim" "Não"))))

(defun mostrar-no (no)
  "Imprime no listener as informações do nó atual"
  (progn
   (format t "~a jogada na posição ~a ~%" (first (no-jogada no)) (second (no-jogada no)))
   (mostrar-tabuleiro (estado-tabuleiro (no-estado no)))
   (format t "Peças disponiveis: ~%")
   (format t "Jogador 1: ~a ~%" (estado-pecas-jogador (no-estado no) 1))
   (format t "Jogador 2: ~a ~% ~% " (estado-pecas-jogador (no-estado no) 2))
   (registar-no no)))

(defun mostrar-pontuacoes (estado)
"Imprime no listener as Pontuações"
    (progn
        (format t "Pontuações: ~%")
        (format t "Jogador 1: ~a pontos ~%" (pontuacao estado 1))
        (format t "Jogador 2: ~a pontos ~% ~%" (pontuacao estado 2))
        (registar-pontuacoes estado)
    )
)

(defun registar-pontuacoes (estado)
    (progn
      (with-open-file (file (diretorio-resultados) :direction :output :if-exists :append :if-does-not-exist :create)
        (format file "Pontuações: ~%")
        (format file "Jogador 1: ~a pontos ~%" (pontuacao estado 1))
        (format file "Jogador 2: ~a pontos ~% ~%" (pontuacao estado 2)))
    )
)

(defun registar-cabecalho (tipo tempo)
    (progn
      (with-open-file (file (diretorio-resultados) :direction :output :if-exists :append :if-does-not-exist :create)
        (format file "~%/////////////////////////////////////////////////////////////////////////~%")
        (format file "Jogo: ~a ~%" tipo)
        (format file "Tempo limite: ~a milisegundos ~%" tempo)
        (format file "/////////////////////////////////////////////////////////////////////////~%")
    ))
)

(defun registar-no (no)
  "Regista no ficheiro log.dat as informações do nó atual"
  (progn
    (with-open-file (file (diretorio-resultados) :direction :output :if-exists :append :if-does-not-exist :create)
      (format file "~a jogada na posição ~a ~%" (first (no-jogada no)) (second (no-jogada no))))
    (registar-tabuleiro (estado-tabuleiro (no-estado no)))
    (with-open-file (file (diretorio-resultados) :direction :output :if-exists :append :if-does-not-exist :create)
      (format file "Peças disponiveis: ~%")
      (format file "Jogador 1: ~a ~%" (estado-pecas-jogador (no-estado no) 1))
      (format file "Jogador 2: ~a ~% ~% " (estado-pecas-jogador (no-estado no) 2)))))


(defun tabuleiro-letras (tabuleiro)
  "Percorre o tabuleiro e troca os números por símbolos"
  (mapcar (lambda (row)
            (mapcar (lambda (cel)
                      (cond
                       ((= cel 2) "O")
                       ((= cel 1) "X")
                       (t "_"))) row)) tabuleiro))

(defun mostrar-tabuleiro (tabuleiro)
  "Imprime no listener o estado do tabuleiro"
  (format t "~{~{~a~^ ~}~%~}" (tabuleiro-letras tabuleiro)))

(defun registar-tabuleiro (tabuleiro)
  "Regista no ficheiro log.dat o estado do tabuleiro"
  (with-open-file (file (diretorio-resultados) :direction :output :if-exists :append :if-does-not-exist :create)
    (format file "~{~{~a~^ ~}~%~}" (tabuleiro-letras tabuleiro))))

(defun diretorio-resultados ()
  ";Devolve o path para o ficheiro resultados.dat"
  (concatenate 'string *path* "log.dat"))

(defun profundidade-max-para-tempo (milisegundos)
  (cond 
    ((< milisegundos 1500) 3)
    ((< milisegundos 2500) 4)
    ((< milisegundos 18000) 5)
    (t 6)
  )
)
