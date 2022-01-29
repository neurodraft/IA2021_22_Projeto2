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

(defun mostrar-menu-jogador()
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
   (format t " ~%|                                     |")
   (format t " ~%|     > 0 - Tempo (milisegundos)      |")
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

(defun menu-escolher-jogador()
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
      ((or (not (numberp option)) (< option 0)) (format t "Opção inválida!") (menu-limite-tempo jogador))
      ((eq option '0) (menu-escolher-jogador))
      (T (jogo-teste))))))
    
(defun jogo-teste ()
    (let 
        (
            (jogador 1)
            (adversario 2)
            (no-atual (no-inicial))
        )
        (progn 
            (limpar-melhor-jogada)
            (loop do
                (progn
                    (alfabeta 
                        no-atual
                        (criar-f-sucessores jogador adversario)
                        (criar-f-utilidade jogador adversario) 5)
                    (setf no-atual (obter-melhor-jogada))
                    (format t "Turno do Jogador ~a ~% ------------------- ~%" jogador)
                    (mostrar-no no-atual)
                    (registar-no no-atual)
                    (let ((temp jogador))
                        (setf jogador adversario)
                        (setf adversario temp)
                    )
                )
            while (not (null (sucessores no-atual jogador (operadores)))))
            (mostrar-pontuacoes (no-estado no-atual))
            (registar-pontuacoes (no-estado no-atual))
        )
    )
)

(defun mostrar-no (no)
"Imprime no listener as informações do nó atual"
  (progn
    (format t "~a jogada na posição ~a ~%" (first (no-jogada no)) (second (no-jogada no)))
   (mostrar-tabuleiro (estado-tabuleiro (no-estado no)))
   (format t "Peças disponiveis: ~%")
   (format t "Jogador 1: ~a ~%" (estado-pecas-jogador (no-estado no) 1))
   (format t "Jogador 2: ~a ~% ~% " (estado-pecas-jogador (no-estado no) 2))
   ))

(defun mostrar-pontuacoes (estado)
"Imprime no listener as Pontuações"
    (progn
        (format t "Pontuações: ~%")
        (format t "Jogador 1: ~a pontos ~%" (pontuacao estado 1))
        (format t "Jogador 2: ~a pontos ~% ~%" (pontuacao estado 2))
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

