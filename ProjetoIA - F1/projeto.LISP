;===========================================================================
;     CARREGA FICHEIROS E TRATA DA INTERAÇÃO COM UTILIZADOR
;===========================================================================
;;;; projeto.lisp
;;;; Disciplina de IA - 2020/ 2021
;===========================================================================
;       FUNÇÕES PARA MOSTRAR TABULEIROS E SEUS MOVIMENTOS
;===========================================================================
(defun mostrar-tabuleiro (tabuleiro &optional(tipo T))
  "Mostra o tabuleiro"
  (format tipo "~%")
  (mapcar (lambda (x) (format tipo "~@T~@T~@T ~A ~%" x)) tabuleiro)
  (format NIL ""))

(defun imprimir-tabuleiro-escolhido (tabuleiro)
  (format T "~%")
  (format T "~@T~@T~@T~@T~@T~@T~@T +=======================+~%")
  (format T "~@T~@T~@T~@T~@T~@T~@T |   TABULEIRO INICIAL   |~%")
  (format T "~@T~@T~@T~@T~@T~@T~@T +=======================+~%")
  (mostrar-tabuleiro (first (first tabuleiro)))
  (mostrar-tabuleiro (second (first tabuleiro))))

(defun imprimir-tabuleiro-solucao (tabuleiro &optional(tipo T))
  (mostrar-tabuleiro (first tabuleiro) tipo)
  (mostrar-tabuleiro (second tabuleiro) tipo))

(defun mostrar-resultados (resultado nomeAlgoritmo noInicial tipo path &optional (h 1) (profundidade 1))
  "Função que mostra os resultados da procura de estado"
  (progn
    (format T "~%~@T~@T~@T~@T --- TABULEIRO INICIAL ---~%")
    (imprimir-tabuleiro-solucao (first noInicial))
    (format T "~%~@T~@T~@T~@T --- TABULEIRO SOLUÇÃO ---~%")
    (imprimir-tabuleiro-solucao (first (first resultado)))
    (format T "~%~@T~@T~@T~@T --- RESULTADOS ---~%")
    (format T "~@T~@T~@T~@T   Algoritmo: ~A ~%" nomeAlgoritmo)
    (format T "~@T~@T~@T~@T   Profundidade do Nó: ~A ~%" (get-profundidade-no (first resultado)))
    (cond
     ((= tipo 2) (format T "~@T~@T~@T~@T   Profundidade Maxima: ~A~%" profundidade))
     ((= tipo 3) (format T "~@T~@T~@T~@T   Heuristica: ~A~%" h)
      (format T "~@T~@T~@T~@T   Custo Heuristica: ~A~%" (get-heuristica (first resultado) h))))
    (format T "~@T~@T~@T~@T   Nos Gerados: ~A~%"  (first (rest resultado)))     
    (format T "~@T~@T~@T~@T   Fator de Ramificacao: ~A~%" (bissecao (get-profundidade-no (first resultado)) (first(rest resultado))))
    (format T "~@T~@T~@T~@T   Penetrância: ~A~%"  (/(get-profundidade-no (first resultado))(bissecao (get-profundidade-no (first resultado)) (first(rest resultado)))))
    (format T "~@T~@T~@T~@T   Tempo em Segundos: ~A~%" (- (get-universal-time) (third resultado)))
    (format T "~%~@T~@T~@T~@T +===========================+~%")
    (format T "~@T~@T~@T~@T |     FIM DA ESTATISTICA    |~%")
    (format T "~@T~@T~@T~@T +===========================+~%~%"))
  ;;imprime para ficheiro resultado.dat
  (mostrar-resultado-ficheiro resultado nomeAlgoritmo noInicial tipo path h profundidade))

(defun current-date-string () 
  "Retorna a data no formato de string"
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (get-decoded-time)
    (declare (ignore dow dst-p tz))	
    (format nil "~2,'0D/~2,'0D/~A ~2,'0D:~2,'0D:~2,'0D" day mon yr hr min sec)))

(defun mostrar-resultado-ficheiro (resultado nomeAlgoritmo noInicial tipo path &optional (h 1) (profundidade 1))
  "Função que cria o ficheiro e faz a escrita, mostrando o resultado"
  (with-open-file (file (concatenate 'string path "resultados.dat")
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
    (progn
      (format file "~%---------------------------------- Gerado em ~s ------------------------------------------------~%~%" (current-date-string))
      (format file "~@T~@T~@T~@T +===========================+~%")
      (format file "~@T~@T~@T~@T |    ESTATISTICAS GERAIS    |~%")
      (format file "~@T~@T~@T~@T +===========================+~%")
      (format file "~%~@T~@T~@T~@T --- TABULEIRO INICIAL ---~%")
      (imprimir-tabuleiro-solucao (first noInicial) file)
      (format file "~%~@T~@T~@T~@T --- TABULEIRO SOLUÇÃO ---~%")
      (imprimir-tabuleiro-solucao (first (first resultado)) file)
      (format file "~%~@T~@T~@T~@T --- RESULTADOS ---~%")
      (format file "~@T~@T~@T~@T   Algoritmo: ~A ~%" nomeAlgoritmo)
      (format file "~@T~@T~@T~@T   Profundidade do Nó: ~A ~%" (get-profundidade-no (first resultado)))
      (cond
       ((= tipo 2) (format file "~@T~@T~@T~@T   Profundidade Maxima: ~A~%" profundidade))
       ((= tipo 3) (format file "~@T~@T~@T~@T   Heuristica: ~A~%" h)
        (format file "~@T~@T~@T~@T   Custo Heuristica: ~A~%" (get-heuristica (first resultado) h))))
      (format file "~@T~@T~@T~@T   Custo Uniforme: ~A~%" (get-heuristica (first resultado) h))
      (format file "~@T~@T~@T~@T   Nos Gerados: ~A~%"  (first (rest resultado)))     
      (format file "~@T~@T~@T~@T   Fator de Ramificacao: ~A~%" (bissecao (get-profundidade-no (first resultado)) (first(rest resultado))))
      (format file "~@T~@T~@T~@T   Penetrância: ~A~%"  (/(get-profundidade-no (first resultado))(bissecao (get-profundidade-no (first resultado)) (first(rest resultado)))))
      (format file "~@T~@T~@T~@T   Tempo em Segundos: ~A~%" (- (get-universal-time) (third resultado)))
      (format file "~%~@T~@T~@T~@T +===========================+~%")
      (format file "~@T~@T~@T~@T |     FIM DA ESTATISTICA    |~%")
      (format file "~@T~@T~@T~@T +===========================+~%"))))

;===========================================================================
;       FUNÇÕES PARA INTER-ACÇÃO DO UTILIZADOR
;===========================================================================
(defun iniciar (&optional (path nil))
  "Permite para iniciar o programa"
  (if (y-or-n-p "Quer iniciar a partida?")
      (iniciar-uma-partida path)
    (terminar-jogo)))

(defun iniciar-uma-partida (path)
  "Permite para iniciar uma partida de jogo"
  (cond 
   ((null path) (carregar-ficheiros (ler-path path)))
   (T (carregar-ficheiros path))))

(defun ler-path (path) 
  "Função para ler o diretorio onde se encontra os ficheiros do projeto" 
  (cond 
   ((null path) (format T "~%Introduza o caminho ate ao diretorio do projeto:~%")     
    (ler-path (concatenate 'string (read-line) "\\")))
   (T path)))

(defun carregar-ficheiros (path)
  "Permite que compila e carregar os ficheiros do projeto para ser executados"
  (progn 
    (load (concatenate 'string path "puzzle.lisp"))
    (load (concatenate 'string path "procura.lisp"))
    (load (concatenate 'string path "problemas.lisp"))
    (menu-principal path)))
  

;;; Função menu principal
(defun menu-principal (path)
  "Apresenta o menu jogar com as opções"
  (progn
    (format T "~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T |         PROJECTO IA         |~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T |     1 - Resolver Puzzle     |~%")   
    (format T "~@T~@T~@T~@T |     2 - Sair                |~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T Escolha: ")
    (let ((opcao (read)))
      (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
      (cond
       ((not (numberp opcao)) (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [1, 2] ~%") (menu-principal path))
       ((= opcao 1) (resolver-puzzle path))
       ((= opcao 2) (terminar-jogo) NIL)
       (T (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [1, 2] ~%") (menu-principal path)))))) 

(defun resolver-puzzle (path)
  "Funcao que ira jogar a aplicacao, pedindo ao utilizador para que escolha o tabuleiro que pretenda que seja o no inicial, o algoritmo que pretende utilizar e a profundidade maxima que pretende para o algoritmo dfs"
  (let* ((tabuleiro (ler-tabuleiro path))
         (algoritmo (ler-algoritmo tabuleiro path))
         (profundidade (cond
                        ((equal algoritmo 'dfs) (ler-profundidade))
                        (T NIL)))
         (heuristica (cond
                      ((or (equal algoritmo 'A*) (equal algoritmo 'IDA*)) (ler-heuristica path))
                      (T 0)))
         (abertos (list tabuleiro)))
    (case algoritmo
      ('bfs (resolver-problema abertos 'bfs path))
      ('dfs (resolver-problema abertos 'dfs path profundidade))
      (A* (resolver-problema abertos 'A* path heuristica)))
    (iniciar path)))

(defun tipo-algoritmo (algoritmo)
  "Função para mostrar o tipo de algoritmo da procura"
  (let ((tipo (cond 
               ((equal algoritmo 'bfs) "PROCURA EM LARGURA (BFS)")
               ((equal algoritmo 'dfs) "PROCURA EM PROFUNDIDADE (DFS)")
               ((equal algoritmo 'A*) "PROCURA A*")
               ((equal algoritmo 'IDA*) "PROCURA IDA*")))) tipo))

(defun procura-algoritmo (algoritmo) 
  "Função para mostrar o algoritmo da procura"
  (let ((tipo (tipo-algoritmo algoritmo)))
    (cond 
     ((equal algoritmo 'bfs)
      (format T "~%")
      (format T "~@T~@T~@T~@T +==============================+~%")
      (format T "~@T~@T~@T~@T |   ~A   |~%" tipo)
      (format T "~@T~@T~@T~@T +==============================+~%"))
     ((equal algoritmo 'dfs)
      (format T "~%")
      (format T "~@T~@T~@T~@T +===================================+~%")
      (format T "~@T~@T~@T~@T |   ~A   |~%" tipo)
      (format T "~@T~@T~@T~@T +===================================+~%"))
     ((equal algoritmo 'a*)
      (format T "~%")
      (format T "~@T~@T~@T~@T +================+~%")
      (format T "~@T~@T~@T~@T |   ~A   |~%" tipo)
      (format T "~@T~@T~@T~@T +================+~%"))
     ((equal algoritmo 'ida*)
      (format T "~%")
      (format T "~@T~@T~@T~@T +==================+~%")
      (format T "~@T~@T~@T~@T |   ~A   |~%" tipo)
      (format T "~@T~@T~@T~@T +==================+~%")))))

(defun resolver-problema (abertos algoritmo path &optional(valor));;; Executa o algoritmo com o tabuleiro enviado
  "Executa o algoritmo escolhido pelo utilizador com o tabuleiro que foi escolhido e outras informações escolhidas"
  (procura-algoritmo algoritmo)
  (cond
   ((equal algoritmo 'bfs)  (mostrar-resultados (bfs abertos) 'BFS (first abertos) 1 path))
   ((equal algoritmo 'dfs)
    (cond
     ((and (>= valor 1) (<= valor 4)) (mostrar-resultados (dfs abertos nil 0 valor) 'DFS (first abertos) 2 path))))
   ((equal algoritmo 'A*) 
    (cond
     ((= valor 1) (mostrar-resultados (A* abertos nil valor) 'A* (first abertos) 3 path valor))
     ((= valor 2) (mostrar-resultados (A* abertos nil valor) 'A* (first abertos) 3 path valor))))
   ((equal algoritmo 'IDA*)
    (cond
     ((= valor 1) (mostrar-resultados (IDA* abertos nil valor) 'IDA* (first abertos) 3 path valor))
     ((= valor 2) (mostrar-resultados (IDA* abertos nil valor) 'IDA* (first abertos) 3 path valor))))))

(defun ler-tabuleiro (path)
  "Permite a escolha dos problemas a resolver"
  (progn
    (format T "~%")
    (format T "~@T~@T~@T~@T +===================================+~%")
    (format T "~@T~@T~@T~@T |          Lista de Problemas       |~%")
    (format T "~@T~@T~@T~@T +===================================+~%")
    (format T "~@T~@T~@T~@T |         1 - Tabuleiro A           |~%")
    (format T "~@T~@T~@T~@T |         2 - Tabuleiro B           |~%")
    (format T "~@T~@T~@T~@T |         3 - Tabuleiro C           |~%")
    (format T "~@T~@T~@T~@T |         4 - Tabuleiro D           |~%")
    (format T "~@T~@T~@T~@T |         5 - Tabuleiro E           |~%")
    (format T "~@T~@T~@T~@T |         6 - Tabuleiro F           |~%")
    (format T "~@T~@T~@T~@T |         7 - Tabuleiro G (ORAL)    |~%")
    (format T "~@T~@T~@T~@T +===================================+~%")
    (format T "~@T~@T~@T~@T Escolha: ")
    (let ((opcao (read)))
      (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
      (cond
       ((not (numberp opcao)) (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [1, 2, 3, 4, 5, 6, 7] ~%") 
        (ler-tabuleiro path))
       ((= opcao 1) *tabuleiroA*)
       ((= opcao 2) *tabuleiroB*)
       ((= opcao 3) *tabuleiroC*)
       ((= opcao 4) *tabuleiroD*)
       ((= opcao 5) *tabuleiroE*)
       ((= opcao 6) *tabuleiroF*)
       ((= opcao 7) *tabuleiroG*)
       (T (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [1, 2, 3, 4, 5, 6, 7] ~%") (ler-tabuleiro path))))))

;;; Função que apresenta a escolha do algoritimo a usar
(defun ler-algoritmo (tabuleiro path)
  "Permite escolher o algoritimo para resolver o problema"
  (progn
    (imprimir-tabuleiro-escolhido tabuleiro)
    (format T "~%")
    (format T "~@T~@T~@T~@T +=========================================+~%")
    (format T "~@T~@T~@T~@T |            Lista de Algoritmos          |~%")
    (format T "~@T~@T~@T~@T +=========================================+~%")
    (format T "~@T~@T~@T~@T |     1 - Busca em Largura (bfs)          |~%")
    (format T "~@T~@T~@T~@T |     2 - Busca em Profundidade (dfs)     |~%")
    (format T "~@T~@T~@T~@T |     3 - A*                              |~%")
    (format T "~@T~@T~@T~@T |     4 - IDA*                            |~%")
    (format T "~@T~@T~@T~@T +=========================================+~%")
    (format T "~@T~@T~@T~@T Escolha: ")
    (let ((opcao (read)))
      (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
      (cond
       ((not (numberp opcao)) (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [1, 2, 3, 4] ~%") 
        (ler-algoritmo tabuleiro path))
       ((= opcao 1) 'bfs)
       ((= opcao 2) 'dfs)
       ((= opcao 3) 'A*)
       ((= opcao 4) 'IDA*)
       (T (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [1, 2, 3, 4] ~%") 
          (ler-algoritmo tabuleiro path))))))

;; ler-profundidade
(defun ler-profundidade (&optional (resposta NIL))
  "Funcao que ira pedir ao utilizador a inserir a profundidade maxima que pretende para o algortimo Busca em profundidade (dfs)"
  (cond
   ((null resposta) 
    (format T "~%~@T~@T~@T~@T Insira a profundidade maxima que deseja para o algoritmo [1 <= x <= 4]: ") 
    (ler-profundidade (read)))
   ((not (numberp resposta)) 
    (format T "~%~@T~@T~@T~@T Por favor insira um numero. ~%") (ler-profundidade))
   ((or (< resposta 1) (> resposta 4))
    (format T "~%~@T~@T~@T~@T ERRO, profundidade entre 1 e 4. ~%") (ler-profundidade))
   (T resposta)))

;;; Função para escolher heuristica
(defun ler-heuristica (path)
  "Permite escolher a heuristica"
  (progn
    (format T "~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T |     Lista de Heuristica     |~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T |      1 - Heurística1        |~%")
    (format T "~@T~@T~@T~@T |      2 - Heurística2        |~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T Escolha: ")
    (let ((opcao (read)))
      (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
      (cond
       ((not (numberp opcao)) (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0, 1, 2] ~%") (ler-heuristica path))
       ((= opcao 1) 1)
       ((= opcao 2) 2)
       (T (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0,1, 2] ~%") 
          (ler-heuristica path))))))


;; Função para terminar o programa
(defun terminar-jogo ()
  "Permite mostrar mensagem quando o programa termina"
  (format t "~%")	
  (format t "~@T~@T~@T~@T+============================+~%")
  (format t "~@T~@T~@T~@T|     PROGRAMA TERMINADO     |~%")
  (format t "~@T~@T~@T~@T+============================+~%"))




