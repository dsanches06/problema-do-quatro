;===========================================================================
;     CARREGA FIHCEIROS E TRATA DA INTERAÇÃO COM UTILIZADOR
;===========================================================================
;;;; projeto.lisp
;;;; Disciplina de IA - 2019 / 2020
;===========================================================================
;       FUNÇÕES PARA MOSTRAR TABULEIROS E SEUS MOVIMENTOS
;===========================================================================
;;; Função para mostrar o tabuleiro
(defun mostrar-tabuleiro (tabuleiro &optional(tipo T))
  "Mostra o tabuleiro"
  (format tipo "~%")
  (mapcar (lambda (x) (format tipo "~@T~@T~@T~@T~@T ~A ~%" x)) tabuleiro)
  (format NIL ""))

(defun mostrar-movimento (no &optional(tipo T))
  "Função que mostra os movimentos da peça no tabuleiro"
  (let* ((resultado (apply #'append (obter-movimento no)))
         (movimentos (reverse (obter-listas resultado))))
    (mapcar (lambda (x) (mostrar-tabuleiro x tipo)) (rest movimentos))))

(defun obter-movimento (no)
  "Função para obter movimentos de um no"
  (cond 
   ((null (second no)) (first no))
   (T (let* ((movimento (no-movimento no)))
        (cons (first movimento) (obter-movimento (second movimento)))))))

(defun no-movimento (no)
  "Função que recebe um nó e retorna a lista de movimentos"
  (loop for i in no
      if (and (not (numberp i)) (not (equal (first i) 'BRANCA)))  
      collect i))

(defun obter-listas (no)
  "Função para recebe um no e retorna a lista de listas"
  (loop for i in no
      if (listp i) collect i))

;; current-date-string [Data actual] - obtem a data do sistema
(defun current-date-string () 
  "Retorna a data no formato de string"
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (get-decoded-time)
    (declare (ignore dow dst-p tz))	
    (format nil "~2,'0D/~2,'0D/~A ~2,'0D:~2,'0D:~2,'0D" day mon yr hr min sec)))

;===========================================================================
;       FUNÇÕES DE LEITURA E ESCRITA
;===========================================================================

(defun escrever-resultados (resultado path)
  "Funcao que ira imprimir o resultado para o ecra de uma forma mais clara bem como escrever as estatisticas num ficheiro estatisticas.dat"
  (cond
   ((not (null resultado))
    (let* ((no-objetivo (first resultado))
           (estado-tabuleiro (first (first no-objetivo)))
           (pecas (no-peca no-objetivo))
           (profundidade (no-profundidade no-objetivo))
           (heuristica (no-heuristica no-objetivo))
           (custo-uniforme (no-f no-objetivo))
           (pai (first (no-raiz no-objetivo)))
           (problema (second pai))
           (objetivo (third pai))
           (algoritmo (second resultado))
           (tempo (third resultado))
           (nos-expandidos (fourth resultado))
           (nos-gerados (fifth resultado)))
      (progn
        (format T "~%~@T~@T~@T~@T   +===========================+~%")
        (format T "~@T~@T~@T~@T   |    ESTATISTICAS GERAIS    |~%")
        (format T "~@T~@T~@T~@T   +===========================+~%")
        (format T "~%~@T~@T~@T~@T --- NÓ INICIAL ---~%")
        (mostrar-tabuleiro (first pai))
        (format T "~%~@T~@T~@T~@T --- MOVIMENTOS ---~%")
        (mostrar-movimento no-objetivo)
        (format T "~%~@T~@T~@T~@T --- NÓ OBJETIVO ---~%")
        (mostrar-tabuleiro estado-tabuleiro)
        (format T "~%~@T~@T~@T~@T --- RESULTADOS ---~%")
        (format T "~@T~@T~@T~@T   Algoritmo: ~A ~%" algoritmo)
        (format T "~@T~@T~@T~@T   Problema: ~A ~%" problema) 
        (format T "~@T~@T~@T~@T   Objetivo: ~A ~%" objetivo) 
        (format T "~@T~@T~@T~@T   Peça: ~A ~%" (first pecas))
        (format T "~@T~@T~@T~@T   Pontuação: ~A ~%" (second pecas))
        (format T "~@T~@T~@T~@T   Profundidade: ~A ~%" profundidade)
        (format T "~@T~@T~@T~@T   Heuristica: ~A ~%" heuristica)
        (format T "~@T~@T~@T~@T   Custo Uniforme: ~A ~%" custo-uniforme)
        (format T "~@T~@T~@T~@T   Nós Gerados: ~A ~%" nos-gerados)    
        (format T "~@T~@T~@T~@T   Nós Expandidos: ~A ~%" nos-expandidos)  
        (format T "~@T~@T~@T~@T   Penetrancia: ~A ~%" (penetrancia profundidade nos-gerados))
        (format T "~@T~@T~@T~@T   Fator de Ramificação: ~A ~%" (fator-ramificacao profundidade nos-gerados))
        (format T "~@T~@T~@T~@T   Tempo em Segundos: ~A ~%" (- (get-universal-time) tempo))
        (format T "~@T~@T~@T~@T   No Pai:")
        (format T "~@T~@T~@T~@T   ~% ~A~%" no-objetivo)
        (format T "~%~@T~@T~@T~@T   +===========================+~%")
        (format T "~@T~@T~@T~@T   |     FIM DA ESTATISTICA    |~%")
        (format T "~@T~@T~@T~@T   +===========================+~%~%")
        ;;escrever resultados no ficheiro
        (escrever-resultados-ficheiro resultado path))))
   (T NIL)))

(defun escrever-resultados-ficheiro (resultado path)
  "Funcao que permite a escrita das estatisticas para um ficheiro resultados.dat"
  (with-open-file (file (concatenate 'string path "resultados.dat")
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
    (cond
     ((not (null resultado))
      (let* ((no-objetivo (first resultado))
             (estado-tabuleiro (first (first no-objetivo)))
             (pecas (no-peca no-objetivo))
             (profundidade (no-profundidade no-objetivo))
             (heuristica (no-heuristica no-objetivo))
             (custo-uniforme (no-f no-objetivo))
             (pai (first (no-raiz no-objetivo)))
             (problema (second pai))
             (objetivo (third pai))
             (algoritmo (second resultado))
             (tempo (third resultado))
             (nos-expandidos (fourth resultado))
             (nos-gerados (fifth resultado)))
        (progn
          (format file "~%---------------------------------- Gerado em ~s ------------------------------------------------~%~%" (current-date-string))
          (format file "~@T~@T~@T~@T +===========================+~%")
          (format file "~@T~@T~@T~@T |    ESTATISTICAS GERAIS    |~%")
          (format file "~@T~@T~@T~@T +===========================+~%")
          (format file "~%~@T~@T~@T~@T --- NO INICIAL ---~%")
          (mostrar-tabuleiro (first pai) file)
          (format file "~%~@T~@T~@T~@T --- MOVIMENTOS ---~%")
          (mostrar-movimento no-objetivo file)
          (format file "~%~@T~@T~@T~@T --- NO OBJETIVO ---~%")
          (mostrar-tabuleiro estado-tabuleiro file)
          (format file "~%~@T~@T~@T~@T --- RESULTADOS ---~%")
          (format file "~@T~@T~@T~@T   Algoritmo: ~A ~%" algoritmo)
          (format file "~@T~@T~@T~@T   Problema: ~A ~%" problema) 
          (format file "~@T~@T~@T~@T   Objetivo: ~A ~%" objetivo) 
          (format file "~@T~@T~@T~@T   Peca: ~A ~%" (first pecas))
          (format file "~@T~@T~@T~@T   Pontuacao: ~A ~%" (second pecas))
          (format file "~@T~@T~@T~@T   Profundidade: ~A ~%" profundidade)
          (format file "~@T~@T~@T~@T   Heuristica: ~A ~%" heuristica)
          (format file "~@T~@T~@T~@T   Custo Uniforme: ~A ~%" custo-uniforme)
          (format file "~@T~@T~@T~@T   Nos Gerados: ~A ~%" nos-gerados)    
          (format file "~@T~@T~@T~@T   Nos Expandidos: ~A ~%" nos-expandidos)  
          (format file "~@T~@T~@T~@T   Penetrancia: ~A ~%" (penetrancia profundidade nos-gerados))
          (format file "~@T~@T~@T~@T   Fator de Ramificacao: ~A ~%" (fator-ramificacao profundidade nos-gerados))
          (format file "~@T~@T~@T~@T   Tempo em Segundos: ~A ~%" (- (get-universal-time) tempo))
          (format file "~@T~@T~@T~@T   No Pai: ~%~A"  no-objetivo)
          (format file "~%~@T~@T~@T~@T +===========================+~%")
          (format file "~@T~@T~@T~@T |     FIM DA ESTATISTICA    |~%")
          (format file "~@T~@T~@T~@T +===========================+~%"))))
     (T NIL))))

;===========================================================================
;       FUNÇÕES PARA INTER-ACÇÃO DO UTILIZADOR
;===========================================================================
(defun jogar (&optional (caminho nil))
  "Permite para jogar o programa"
  (let ((path (ler-path caminho)))
    (carregar-ficheiros path)))

(defun ler-path (path) 
  "Função para ler o diretorio onde se encontra os ficheiros do projeto" 
  (cond 
   ((null path) (format T "~%Introduza o caminho ate ao diretorio do projeto:~%")     
    (ler-path (read-line)))
   (T path)))

(defun carregar-ficheiros (path)
  "Permite que compila e carregar os ficheiros do projeto para ser executados"
  (progn 
    (compile-file (concatenate 'string path "procura.lisp"))
    (compile-file (concatenate 'string path "puzzle.lisp"))
    (load (concatenate 'string path "puzzle.ofasl"))
    (load (concatenate 'string path "procura.ofasl"))
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
       ((= opcao 2) (menu-sair) NIL)
       (T (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [1, 2] ~%") (menu-principal path)))))) 


(defun resolver-puzzle (path)
  "Funcao que ira jogar a aplicacao, pedindo ao utilizador para que escolha o tabuleiro que pretenda que seja o no inicial, o algoritmo que pretende utilizar e a profundidade maxima que pretende para o algoritmo dfs"
  (let* ((tabuleiro (ler-tabuleiro path))
         (algoritmo (ler-algoritmo (first tabuleiro) path (second tabuleiro) (third tabuleiro)))
         (profundidade (cond
                        ((equal algoritmo 'dfs) (ler-profundidade))
                        (T NIL)))
         (heuristica (cond
                      ((or (equal algoritmo 'A*) (equal algoritmo 'IDA*)) (ler-heuristica path))
                      (T 'sem-heuristica)))
         (pecas-no-raiz (list 'BRANCA 0))
         (heuristica-no-raiz (funcall heuristica tabuleiro pecas-no-raiz))
         (no-inicial (criar-no tabuleiro pecas-no-raiz 0 heuristica-no-raiz heuristica-no-raiz NIL)))
    (cond
     ((equal algoritmo 'dfs)
      (escrever-resultados (dfs no-inicial algoritmo 'no-solucao-p 'gerar-sucessores (operadores) profundidade heuristica) path) (jogar path))
     (T (escrever-resultados (funcall algoritmo no-inicial algoritmo 'no-solucao-p 'gerar-sucessores (operadores) heuristica) path)
        (jogar path)))))


;; Função que apresenta a escolha do problema a resolver
(defun ler-tabuleiro (path)
  "Permite a escolha dos problemas a resolver"
  (progn
    (format T "~%")
    (format T "~@T~@T~@T~@T +===================================+~%")
    (format T "~@T~@T~@T~@T |          Lista de Problemas       |~%")
    (format T "~@T~@T~@T~@T +===================================+~%")
    (format T "~@T~@T~@T~@T |         1 - Tabuleiro A [100]     |~%")
    (format T "~@T~@T~@T~@T |         2 - Tabuleiro B [200]     |~%")
    (format T "~@T~@T~@T~@T |         3 - Tabuleiro C [300]     |~%")
    (format T "~@T~@T~@T~@T |         4 - Tabuleiro D [425]     |~%")
    (format T "~@T~@T~@T~@T |         5 - Tabuleiro E [525]     |~%")
    (format T "~@T~@T~@T~@T |         6 - Tabuleiro F [1000]    |~%")
    (format T "~@T~@T~@T~@T |         7 - Tabuleiro G [ORAL]    |~%")
    (format T "~@T~@T~@T~@T +===================================+~%")
    (format T "~@T~@T~@T~@T Escolha: ")
    (let ((opcao (read)))
      (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
      (with-open-file (file (concatenate 'string path "problemas.dat") :direction :input)
        (let ((tabuleiroA (list (ler-tabuleiro-ficheiro file) 'A 100))
              (tabuleiroB (list (ler-tabuleiro-ficheiro file) 'B 200))
              (tabuleiroC (list (ler-tabuleiro-ficheiro file) 'C 300))
              (tabuleiroD (list (ler-tabuleiro-ficheiro file) 'D 425))
              (tabuleiroE (list (ler-tabuleiro-ficheiro file) 'E 525))
              (tabuleiroF (list (ler-tabuleiro-ficheiro file) 'F 1000)))
          (cond
           ((not (numberp opcao)) (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [1, 2, 3, 4, 5, 6, 7] ~%") 
            (ler-tabuleiro path))
           
           ((= opcao 1) tabuleiroA)
           ((= opcao 2) tabuleiroB)
           ((= opcao 3) tabuleiroC)
           ((= opcao 4) tabuleiroD)
           ((= opcao 5) tabuleiroE)
           ((= opcao 6) tabuleiroF)
           ((= opcao 7) (list (ler-tabuleiroG) 'G (ler-objetivo-tabuleiroG))) ;Problema G da avaliacao oral
           (T (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [1, 2, 3, 4, 5, 6, 7] ~%") (ler-tabuleiro path))))))))

;; ler-tabuleiro-ficheiro
(defun ler-tabuleiro-ficheiro (file)
  "Funcao que permite ler um tabuleiro de um ficheiro"
  (let ((linha (read file nil :fim)))
    (cond
     ((equal linha 'end) (close file) NIL)
     ((not (equal linha 'break)) (cons linha (ler-tabuleiro-ficheiro file)))
     ((equal linha 'break) NIL))))		


(defun ler-tabuleiroG (&optional (tabuleiro nil)) 
  "Função para ler o tabuleiro G na discussao oral" 
  (cond 
   ((null tabuleiro) (format T "~%Insira o tabuleiro: ")     
    (ler-tabuleiroG (read)))
   (T tabuleiro)))

(defun ler-objetivo-tabuleiroG (&optional (pontos nil)) 
  "Função para ler o numero de pontos a atingir" 
  (cond 
   ((null pontos) (format T "~%~%Insira o numero de pontos: ")
    (ler-objetivo-tabuleiroG (read)))
   (T pontos)))
	
;;; Função que apresenta o objetivo do problema escolhido
(defun mostrar-objetivo (problema objetivo) 
  "Permite mostrar o objetivo do problema para escrita em consola e file"
  (cond
   ((equal problema 'A) (format t "~%      --- PROBLEMA ~A: Objectivo ~A pontos ---~%" problema objetivo))
   ((equal problema 'B) (format t "~%      --- PROBLEMA ~A: Objectivo ~A pontos ---~%" problema objetivo))
   ((equal problema 'C) (format t "~%      --- PROBLEMA ~A: Objectivo ~A pontos ---~%" problema objetivo))
   ((equal problema 'D) (format t "~%      --- PROBLEMA ~A: Objectivo ~A pontos ---~%" problema objetivo))
   ((equal problema 'E) (format t "~%      --- PROBLEMA ~A: Objectivo ~A pontos ---~%" problema objetivo))
   ((equal problema 'F) (format t "~%      --- PROBLEMA ~A: Objectivo ~A pontos ---~%" problema objetivo))
   ((equal problema 'G) (format t "~%      --- PROBLEMA ~A: Objectivo ~A pontos ---~%" problema objetivo))))		

;;; Função que apresenta a escolha do algoritimo a usar
(defun ler-algoritmo (tabuleiro path problema objetivo)
  "Permite escolher o algoritimo para resolver o problema"
  (progn
    (mostrar-objetivo problema objetivo)
    (mostrar-tabuleiro tabuleiro)
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
       ((not (numberp opcao)) (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0, 1, 2, 3, 4] ~%") 
        (ler-algoritmo tabuleiro path problema objetivo))
       ((= opcao 1) 'bfs)
       ((= opcao 2) 'dfs)
       ((= opcao 3) 'A*)
       ((= opcao 4) 'IDA*)
       (T (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0, 1, 2, 3, 4] ~%") 
          (ler-algoritmo tabuleiro path problema objetivo))))))

;; ler-profundidade
(defun ler-profundidade (&optional (resposta NIL))
  "Funcao que ira questionar o utilizador sobre qual a profundidade maxima que pretende para o algortimo Busca em profundidade (dfs)"
  (cond
   ((null resposta) 
    (format T "~%~@T~@T~@T~@T Insira a profundidade maxima que deseja para o algoritmo: ") 
    (ler-profundidade (read)))
   ((not (numberp resposta)) 
    (format T "~%~@T~@T~@T~@T Por favor insira um numero maior do que 1. ~%") (ler-profundidade))
   ((<= resposta 1) 
    (format T "~%~@T~@T~@T~@T Por favor insira um numero maior do que 1. ~%") (ler-profundidade))
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
       ((= opcao 1) 'heuristica1)
       ((= opcao 2) 'heuristica2)
       (T (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0,1, 2] ~%") 
          (ler-heuristica path))))))

;; Função para terminar o programa
(defun menu-sair ()
  "Permite mostrar mensagem quando o programa termina"
  (format t "~%")	
  (format t "~@T~@T~@T~@T+============================+~%")
  (format t "~@T~@T~@T~@T|     PROGRAMA TERMINADO     |~%")
  (format t "~@T~@T~@T~@T+============================+~%"))




