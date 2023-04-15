;=======================================================================
;     CARREGA FICHEIROS E TRATA DA INTERAÇÃO COM UTILIZADOR
;=======================================================================
;;;; interact.lisp
;;;; Disciplina de IA - 2019 / 2020
;;;; Autor:
;;----------------------------------------------------------------------------
;;  FUNÇÕES UTILITÁRIOS
;;---------------------------------------------------------------------------
(defun ler-jogada (&optional(jogada NIL)) 
  "Função para ler o diretorio onde se encontra os ficheiros do projeto" 
  (cond 
   ((null jogada) (format T "~%Introduza a jogada [Coluna => A -> G][linha => 1 -> 7]: ")     
    (ler-jogada (read-line)))
   (T (string-capitalize jogada))))

(defun validar-jogada ()       
  "Função para ler o diretorio onde se encontra os ficheiros do projeto"
  (let* ((jogada (ler-jogada))) jogada))

(defun converter-jogada-destino (jogada)       
  "Função para ler o diretorio onde se encontra os ficheiros do projeto"
  (let* ((linha (get-index-linha (subseq jogada 0 1)));;primeiro caracter
         (coluna (get-index-coluna (subseq jogada 1))));;segundo caracter
     (list linha coluna)))

(defun get-index-linha (linha)
  (cond
   ((string-equal linha "A") 0)
   ((string-equal linha "B") 1)
   ((string-equal linha "C") 2)
   ((string-equal linha "D") 3)
   ((string-equal linha "E") 4)
   ((string-equal linha "F") 5)
   ((string-equal linha "G") 6)
   (T NIL)))

(defun get-index-coluna (coluna)
  (cond
   ((string-equal coluna "1") 0)
   ((string-equal coluna "2") 1)
   ((string-equal coluna "3") 2)
   ((string-equal coluna "4") 3)
   ((string-equal coluna "5") 4)
   ((string-equal coluna "6") 5)
   ((string-equal coluna "7") 6)
   (T NIL)))

(defun get-converter-linha (linha)
  (cond
   ((zerop linha) 'A)
   ((= linha 1) 'B)
   ((= linha 2) 'C)
   ((= linha 3) 'D)
   ((= linha 4) 'E)
   ((= linha 5) 'F)
   ((= linha 6) 'G)
   (T NIL)))

(defun converter-jogadas-em-linha-coluna (no jogador)
  (let* ((jogadas-possiveis (calcular-posicao-destino no jogador)))
    (cond 
     ((zerop (length jogadas-possiveis)) NIL)
     (T (mapcar #'(lambda (jogadas) (list (converter-jogadas-aux jogadas))) jogadas-possiveis)))))


(defun converter-jogadas-aux (jogada)
         (let* ((linha (get-converter-linha (first jogada)))
                           (coluna (1+ (second jogada)))
                           (linha-aux (write-to-string linha))
                           (coluna-aux (write-to-string coluna))
                           (result (concatenate 'string linha-aux coluna-aux))) result))

(defun calcular-movimento (no jogador destino)
  (let* ((posicao-peca (converter-jogadas-aux (obter-posicao (get-no-estado no) jogador)))
         (jogada-inicial (converter-jogada-destino posicao-peca))
         (posicao-destino (converter-jogadas-aux destino))
         (jogada-destino (converter-jogada-destino posicao-destino)))
    (cond 
     ((and (zerop (- (first jogada-destino) (first jogada-inicial))) ;; se x1 = x2 = 0
           (> (second jogada-destino) (second jogada-inicial))) ;; e se y2 > y1 
      (list (- (second jogada-destino) (second jogada-inicial)) 'BAIXO)) ;; irá mover para baixo
     ((and (zerop (- (first jogada-destino) (first jogada-inicial))) ;; se x1 = x2 = 0
           (< (second jogada-destino) (second jogada-inicial))) ;; e se y2 < y1 
      (list (- (second jogada-inicial) (second jogada-destino)) 'CIMA)) ;; irá mover para cima
     ((and (zerop (- (second jogada-destino) (second jogada-inicial))) ;; se y1 = y2 = 0
           (> (first jogada-inicial) (first jogada-destino))) ;; e se x1 > x2 
      (list (- (first jogada-inicial) (first jogada-destino)) 'ESQUERDA))
     ((and (zerop (- (second jogada-destino) (second jogada-inicial))) ;; se y1 = y2 = 0
           (< (first jogada-inicial) (first jogada-destino))) ;; e se x2 > x1 
      (list (- (first jogada-destino) (first jogada-inicial)) 'DIREITA)))))

;===========================================================================
;       FUNÇÕES PARA MOSTRAR TABULEIROS E SEUS MOVIMENTOS
;===========================================================================
;;; Função para mostrar o tabuleiro
(defun mostrar-tabuleiro (tabuleiro &optional(tipo T))
  "Mostra o tabuleiro"
  (format tipo "~%")
  (mapcar (lambda (x) (format tipo "~@T~@T~@T~@T~@T ~A~%" x)) tabuleiro)
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
      if (and (not (numberp i)) (not (equal (first i) 'BRANCA)) )  
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
;       FUNÇÕES PARA INTER-ACÇÃO DO UTILIZADOR
;===========================================================================
(defun jogar (&optional (caminho nil))
  "Permite para iniciar o programa"
  (let ((path (ler-path  caminho)))
    (carregar-ficheiros path)))

(defun ler-path (path) 
  "Função para ler o diretorio onde se encontra os ficheiros do projeto" 
  (cond 
   ((null path) (format T "~%~%Introduza o caminho ate ao diretorio do projeto:~%")     
    (ler-path (read-line)))
   (T path)))

(defun carregar-ficheiros (path)
  "Permite que compila e carregar os ficheiros do projeto para ser executados"
  (progn 
    (compile-file (concatenate 'string path "jogo.lisp"))
    (compile-file (concatenate 'string path "algoritmo.lisp"))
    (load (concatenate 'string path "jogo.ofasl"))
    (load (concatenate 'string path "algoritmo.ofasl"))
    (menu-principal path)))

;;; Função menu principal
(defun menu-principal (path)
  "Apresenta o menu iniciar com as opções"
  (progn
    (format T "~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T |         PROJECTO IA         |~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T |     1 - Resolver Puzzle     |~%")  
    (format T "~@T~@T~@T~@T |     0 - Sair                |~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T Escolha: ")
    (let ((opcao (read)))
      (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
      (cond
       ((not (numberp opcao)) (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0, 1] ~%") (menu-principal path))
       ((= opcao 1) (resolver-puzzle path))
       ((= opcao 0) (menu-sair) NIL)
       (T (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [0, 1] ~%") (menu-principal path)))))) 


(defun resolver-puzzle (path)
  "Funcao que ira jogar a aplicacao, pedindo ao utilizador para que escolha o tabuleiro que pretenda que seja o no inicial, o algoritmo que pretende utilizar e a profundidade maxima que pretende para o algoritmo dfs"
  (let* ((tabuleiro (tabuleiro-inicial))
         (tipo-jogo (ler-tipo-jogo tabuleiro))
         (jogador-escolhido (cond
                             ((equal tipo-jogo 'humano) (ler-escolher-jogador))
                             (T 1)))
         (tempo-limite (ler-tempo-limite))
         (profundidade (ler-profundidade))
         (peca-jogador1 (list -1 0))
         (peca-jogador2 (list -2 0))
         (no-inicial (criar-no tabuleiro peca-jogador1 peca-jogador2 jogador-escolhido)))
    (cond
     ((equal tipo-jogo 'humano) (iniciar-jogo-humano no-inicial tempo-limite profundidade jogador-escolhido path) (jogar))
     (T (iniciar-jogo-computador no-inicial tempo-limite profundidade path)
        (jogar path)))))
	

(defun ler-tipo-jogo (tabuleiro)
  "Funcao que ira questionar o utilizar sobre se pretende jogar contra o computador ou jogar Computador vs Computador"
  (progn
    (format T "~%")
    (format T "~@T~@T~@T~@T +======================================+~%")
    (format T "~@T~@T~@T~@T |              Tipo de Jogos           |~%")
    (format T "~@T~@T~@T~@T +======================================+~%")
    (format T "~@T~@T~@T~@T |                                      |~%")
    (format T "~@T~@T~@T~@T |     1 - Humano VS Computador         |~%")
    (format T "~@T~@T~@T~@T |     2 - Computador vs Computador     |~%")
    (format T "~@T~@T~@T~@T |                                      |~%")
    (format T "~@T~@T~@T~@T +======================================+~%")
    (format T "~@T~@T~@T~@T Escolha: ")
    (let((resposta (read)))
      (format T "~@T~@T~@T~@T       ESCOLHEU Jogo ~A ~%" resposta)
      (cond
       ((not (numberp resposta)) (format T "~%~@T~@T~@T~@T Por favor insira o numero 1 ou 2~%") (ler-tipo-jogo tabuleiro))
       ((= resposta 1) 'humano)
       ((= resposta 2) 'computador)
       (T (format T "~%~@T~@T~@T~@T Por favor insira o numero 1 ou 2~%") (ler-tipo-jogo tabuleiro))))))


;; ler-escolha-jogador
(defun ler-escolher-jogador ()
  "Funcao que ira questionar o utilizador sobre com que qual peca deseja jogar"
  (progn
    (format T "~%")
    (format T "~@T~@T~@T~@T +================================+~%")
    (format T "~@T~@T~@T~@T |         Primeiro a jogar       |~%")
    (format T "~@T~@T~@T~@T +================================+~%")
    (format T "~@T~@T~@T~@T |                                |~%")
    (format T "~@T~@T~@T~@T |          1 - Jogador1          |~%")
    (format T "~@T~@T~@T~@T |          2 - Jogador2          |~%")
    (format T "~@T~@T~@T~@T |                                |~%")
    (format T "~@T~@T~@T~@T +================================+~%")
    (format T "~@T~@T~@T~@T Escolha: ")
    (let((resposta (read)))
      (format T "~@T~@T~@T~@T       ESCOLHEU Jogador ~A ~%" resposta)
      (cond
       ((not (numberp resposta)) (format T "~%~@T~@T~@T~@T Por favor insira o numero 1 ou 2~%") (ler-escolher-jogador))
       ((= resposta 1) 1)
       ((= resposta 2) 2)
       (T (format T "~%~@T~@T~@T~@T Por favor insira o numero 1 ou 2~%") (ler-escolher-jogador))))))

;; ler-tempo-limite
(defun ler-tempo-limite (&optional (resposta nil))
  "Funcao que ira questionar o utilizador sobre o tempo maximo que o computador tem para decidir sobre uma jogada"
  (cond
   ((null resposta) 
    (format T "~%~@T~@T~@T~@T Insira o tempo maximo que o computador tem para decidir sobre uma jogada: ") (ler-tempo-limite (read)))
   ((not (numberp resposta)) 
    (format T "~%~@T~@T~@T~@T Por favor insira um numero entre 1 e 10~%") (ler-tempo-limite))
   ((or (<= resposta 0) (>= resposta 11)) 
    (format T "~%~@T~@T~@T~@T Por favor insira um numero entre 1 e 10~%") (ler-tempo-limite))
   (T resposta)))

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
   ((<= resposta 3) 4)
   (T resposta)))

(defun iniciar-jogo-humano (no tempo profundidade jogador path)
 (mostrar-tabuleiro (get-no-estado no))
 (obter-jogador-atual no jogador)
 (let ((jogadas-possiveis (obter-jogadas (get-no-estado no) jogador)))
    (cond
     ((zerop (length jogadas-possiveis)) NIL);; se não houver jogada, sai
     (T (let* ((jogada (validar-jogada))
               (jogada-destino (converter-jogada-destino jogada))
               (movimento (calcular-movimento no jogador jogada-destino)))
        (iniciar-jogo-humano (aplicar-operador-no no movimento) tempo profundidade (trocar-jogador no) path))))))

(defun obter-jogador-atual (no jogador)
 (cond ((= jogador 1) (mostrar-info-jogador no (get-peca-jogador1 no) jogador))
       (T (= jogador 2) (mostrar-info-jogador no (get-peca-jogador2 no) jogador))))

(defun mostrar-info-jogador (no peca jogador)
  (format t "~%Jogador atual: ~A" jogador)
  (format t "~%Peça jogador: ~A" (first peca))
  (format t "~%Posição peça: ~A"  (converter-jogadas-aux (obter-posicao (get-no-estado no) jogador)))
  (format t "~%Posição peça: ~A"  (obter-posicao (get-no-estado no) jogador))
  (format t "~%Pontos acumulados: ~A" (second peca))
  (format t "~%Jogadas possiveis:~A~%" (converter-jogadas-em-linha-coluna no jogador)))

(defun iniciar-jogo-computador (no tempo profundidade path)
)


;; Função para terminar o programa
(defun menu-sair ()
  "Permite mostrar mensagem quando o programa termina"
  (format t "~%")	
  (format t "~@T~@T~@T~@T+============================+~%")
  (format t "~@T~@T~@T~@T|     PROGRAMA TERMINADO     |~%")
  (format t "~@T~@T~@T~@T+============================+~%"))


