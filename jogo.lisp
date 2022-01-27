((defun iniciar ()
  "Função que inicia o programa"
  (progn
   (definir-pasta)
   (menu-inicial))))

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
   (format t " ~%|     > 0 - tempo (milisegundos)      |")
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
                ((eq option '0) (menu-inical))
            ))))

(defun menu-limite-tempo (&optional jogador)
  "Chama a funçãoo mostrar-menu-tempo-limite e lê o input do utilizador"
  (progn (mostrar-menu-tempo-limite)
         (let ((option (read)))
           (cond
            ((or (not (numberp option)) (< option 0)) (format t "Opção inválida!") (menu-limite-tempo jogador))
            ((eq option '0) (menu-escolher-jogador))
            (T T)))))
    