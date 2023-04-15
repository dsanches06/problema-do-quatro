;===========================================================================
;     CARREGA FICHEIROS E TRATA DA INTERAÇÃO COM UTILIZADOR
;===========================================================================
;;;; interact.lisp
;;;; Disciplina de IA - 2020/ 2021
;===========================================================================
;      CONSTANTES
;===========================================================================
(defvar *jogador1* 1)
(defvar *jogador2* 2)
(defvar *tempo-jogada* 5);;tempo maximo da jogada
(defparameter *tabuleiro* '(((0 0 0 0) 
                             (0 0 0 0) 
                             (0 0 0 0) 
                             (0 0 0 0))
                            ((branca redonda alta oca)
                             (preta redonda alta oca)
                             (branca redonda baixa oca)
                             (preta redonda baixa oca)
                             (branca quadrada alta oca)
                             (preta quadrada alta oca)
                             (branca quadrada baixa oca)
                             (preta quadrada baixa oca)
                             (branca redonda alta cheia)
                             (preta redonda alta cheia)
                             (branca redonda baixa cheia)
                             (preta redonda baixa cheia)
                             (branca quadrada alta cheia)
                             (preta quadrada alta cheia)
                             (branca quadrada baixa cheia)
                             (preta quadrada baixa cheia))))

;===========================================================================
;     FUNÇÕES PARA INICIAR A PARTIDA DE JOGO
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
    (load (concatenate 'string path "jogo.lisp"));; :load T)
    (load (concatenate 'string path "algoritmo.lisp"));; :load T)
    (menu-principal path)))

;;; Função menu principal
(defun menu-principal (path)
  "Apresenta o menu jogar com as opções"
  (progn
    (format T "~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T |         PROJECTO IA         |~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T |     1 - Iniciar jogo        |~%")   
    (format T "~@T~@T~@T~@T |     2 - Sair                |~%")
    (format T "~@T~@T~@T~@T +=============================+~%")
    (format T "~@T~@T~@T~@T Escolha: ")
    (let ((opcao (read)))
      (format T "~@T~@T~@T~@T       ESCOLHEU OPÇÃO ~A ~%" opcao)
      (cond
       ((not (numberp opcao)) (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [1, 2] ~%") (menu-principal path))
       ((= opcao 1) (iniciar-jogo path))
       ((= opcao 2) (terminar-jogo) NIL)
       (T (format T "~%~@T~@T~@T~@T ERRO: Introduza um numéro válido [1, 2] ~%") (menu-principal path)))))) 

(defun iniciar-jogo (path)
  "Função para iniciar o jogo"
  (let* ((tabuleiro (first *tabuleiro*))
         (reserva (second *tabuleiro*))
         (jogo (escolher-jogo path))
         (jogador (cond
                   ((= jogo 1) (escolher-jogador-inicia-jogo jogo))
                   (T *jogador1*)))
         (tempo-limite (ler-tempo-limite))
         (no-inicial (criar-no tabuleiro reserva jogador)))
    (cond
     ((= jogo 1) (humano-vs-computador jogo no-inicial jogador tempo-limite path))
     (T (computador-vs-computador jogo no-inicial jogador tempo-limite path)))))

(defun escolher-jogo (path)
  "Funcao que ira questionar o utilizar sobre se pretende iniciar contra o computador ou iniciar Computador vs Computador"
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
      (format T "~@T~@T~@T~@T       Escolheu jogo ~A ~%" resposta)
      (cond
       ((not (numberp resposta)) (format T "~%~@T~@T~@T~@T Por favor insira o numero 1 ou 2~%") (escolher-jogo path))
       ((= resposta 1) 1)
       ((= resposta 2) 2)
       (T (format T "~%~@T~@T~@T~@T Por favor insira o numero 1 ou 2~%") (escolher-jogo path))))))

(defun escolher-jogador-inicia-jogo (jogo)
  "Função para escolher que jogador irá iniciar o jogo"
  (progn
    (format T "~%")
    (format T "~@T~@T~@T~@T +=========================+~%")
    (format T "~@T~@T~@T~@T |     ESOLHER JOGADOR     |~%")
    (format T "~@T~@T~@T~@T +=========================+~%")
    (format T "~@T~@T~@T~@T |                         |~%")
    (format T "~@T~@T~@T~@T |     1 - Humano          |~%")
    (format T "~@T~@T~@T~@T |     2 - Computador      |~%")
    (format T "~@T~@T~@T~@T |                         |~%")
    (format T "~@T~@T~@T~@T +=========================+~%")
    (format T "~@T~@T~@T~@T Escolha: ")
    (let* ((resposta (read)))
      (format T "~@T~@T~@T~@T       ESCOLHEU Jogador ~A ~%" (mostrar-jogador jogo resposta))
      (cond
       ((not (numberp resposta)) 
        (format T "~%~@T~@T~@T~@T Por favor insira o numero 1 ou 2~%") (escolher-jogador-inicia-jogo jogo))
       ((= resposta 1) *jogador1*)
       ((= resposta 2) *jogador2*)
       (T (format T "~%~@T~@T~@T~@T Por favor insira o numero 1 ou 2~%") (escolher-jogador-inicia-jogo jogo))))))

(defun ler-tempo-limite (&optional (resposta nil))
  "Funcao que ira questionar o utilizador sobre o tempo maximo que o computador tem para decidir sobre uma jogada"
  (cond
   ((null resposta) 
    (format T "~%~@T~@T~@T~@T Insira o tempo limite do computador[1 <= x <= 5]: ") (ler-tempo-limite (read)))
   ((not (numberp resposta)) 
    (format T "~%~@T~@T~@T~@T Por favor insira um numero entre 1 e 5~%") (ler-tempo-limite))
   ((or (< resposta 1) (> resposta 5)) 
    (format T "~%~@T~@T~@T~@T Por favor insira um numero entre 1 e 5~%") (ler-tempo-limite))
   (T resposta)))

(defun humano-vs-computador (jogo tabuleiro jogador tempo path)
  "Função que realiza o jogo entre humano e computador"
  (imprime-tabuleiro tabuleiro)
  (cond  
   ((null tabuleiro)(format t "~%ERRO!! O nó inicial não foi criado com sucesso.") (iniciar path))
   (T (let* ((peca (first (second tabuleiro))))
        (cond 
         ((null peca) (format t "TABULEIRO CHEIO!! Não tem mais peça na reserva.~%") (format t "~%~%NÃO HOUVE VENCEDOR, TEMOS EMPATE!!!~%") (iniciar path))
         ((= jogador 1) ;; jogador 1
          (let* ((jogada-humano (concretizar-jogada-humano 'humano-faz-jogada jogo tabuleiro jogador peca tempo path)))
            (cond  
             ;; se for solução ou empate, mostra as informações e retorna ao inicio)
             ((equal (definir-vencedor jogada-humano jogo jogador) 'T) (iniciar path))
             ;; se não, passa o jogo para o proximo jogador
             (T (humano-vs-computador jogo jogada-humano (trocar-jogador jogada-humano) tempo path)))))
         
         (T (= jogador 2) ;; jogador 2
            (let* ((jogada-PC (computador-faz-jogada jogo tabuleiro jogador peca tempo path)))
              (cond  
               ;; se for solução ou empate, mostra as informações e retorna ao inicio)
               ((equal (definir-vencedor jogada-PC jogo jogador) 'T) (iniciar path))
               ;; se não, passa o jogo para o proximo jogador
               (T (humano-vs-computador jogo jogada-PC (trocar-jogador jogada-PC) tempo path))))))))))

(defun concretizar-jogada-humano (funcao jogo no jogador peca tempo path)
  "Função que irá concretizar a jogada tanto do humano como computador"
  (let* ((jogada (cond 
                  ((and (= jogo 1) (= jogador 1)) (funcall funcao jogo no jogador peca) ) ;;humano
                  ((and (= jogo 1) (= jogador 2))  (funcall funcao jogo no jogador peca tempo path)) ;;pc
                  (T (and (= jogo 2) (or (= jogador 1) (= jogador 2))) (funcall funcao jogo no jogador peca tempo path))))
         (novo-no (colocar-peca-no-tabuleiro (first jogada) (second jogada) peca no jogador)))  novo-no))

(defun definir-vencedor (no jogo jogador)
  "Função que irá definir se o nó atual é a solução e determina o vencedor, senão retorna nil"
  (cond
   ((solucaop (get-tabuleiro-de-jogo no)) (imprime-tabuleiro no) (format t "~%PARABÉNS!!! GANHOU O JOGADOR ~A.~%" (mostrar-jogador jogo jogador)) T)
   ((not (tabuleiro-preenchido-p no)) (imprime-tabuleiro no) (format t "~%EMPATE!!! NÃO HÁ VENCEDOR.~%") T)
   (T NIL)))

(defun tabuleiro-preenchido-p (tabuleiro &optional(elem 0))
  "Verifica se o tabuleiro ja esta completamente preenchido ou  seja sem casa vaziap"
  (cond
   ((null tabuleiro) nil)
   ((equal elem (car tabuleiro)) t)
   ((consp (car tabuleiro)) (or (tabuleiro-preenchido-p (car tabuleiro) elem)
                                (tabuleiro-preenchido-p (cdr tabuleiro) elem)))
   (t (tabuleiro-preenchido-p (cdr tabuleiro) elem))))

(defun computador-vs-computador (jogo tabuleiro jogador tempo path)
  "Função que realiza o jogo entre computador e computador"
  (imprime-tabuleiro tabuleiro)
  (cond  
   ((null tabuleiro) (format t "ERRO!! O nó inicial não foi criado com sucesso.~%") (iniciar path))
   (T (let* ((peca (first (second tabuleiro))))
        (cond 
         ((null peca) (format t "TABULEIRO CHEIO!! Não tem mais peça na reserva.~%") (format t "~%~%NÃO HOUVE VENCEDOR, TEMOS EMPATE!!!~%") (iniciar path))
         ((= jogador 1) ;; jogador 1
          (let* ((jogada-PC-1 (computador-faz-jogada jogo tabuleiro jogador peca tempo path)))
            (cond  
             ;; se for solução ou empate, mostra as informações e retorna ao inicio)
             ((equal (definir-vencedor jogada-PC-1 jogo jogador) 'T) (iniciar path))
             ;; se não, passa o jogo para o proximo jogador
             (T (computador-vs-computador jogo jogada-PC-1 (trocar-jogador jogada-PC-1) tempo path)))))
         
         (T (= jogador 2) ;; jogador 2
            (let* ((jogada-PC-2 (computador-faz-jogada jogo tabuleiro jogador peca tempo path)))
              (cond  
               ;; se for solução ou empate, mostra as informações e retorna ao inicio)
               ((equal (definir-vencedor jogada-PC-2 jogo jogador) 'T) (iniciar path))
               ;; se não, passa o jogo para o proximo jogador
               (T (computador-vs-computador jogo jogada-PC-2 (trocar-jogador jogada-PC-2) tempo path))))))))))

(defun terminar-jogo ()
  "Permite mostrar mensagem quando o programa termina"
  (format t "~%")	
  (format t "~@T~@T~@T~@T+============================+~%")
  (format t "~@T~@T~@T~@T|     PROGRAMA TERMINADO     |~%")
  (format t "~@T~@T~@T~@T+============================+~%"))

;===========================================================================
;      FUNÇÕES PARA MOSTRAR TABULEIRO
;===========================================================================
(defun mostrar-tabuleiro (tabuleiro tipo)
  "Imprime o tabuleiro, linha a linha"
  (format tipo "~%")
  (mapcar (lambda (x) (format tipo "~@T~@T~@T ~A ~%" x)) tabuleiro)
  (format NIL ""))

(defun imprime-tabuleiro(tabuleiro &optional(tipo T))
  (format T "~%")   
  (format T "~@T~@T~@T~@T~@T~@T~@T +=======================+~%")
  (format T "~@T~@T~@T~@T~@T~@T~@T |   TABULEIRO DO JOGO   |~%")
  (format T "~@T~@T~@T~@T~@T~@T~@T +=======================+~%")
  (mostrar-tabuleiro (get-tabuleiro-de-jogo tabuleiro) tipo)
  (mostrar-tabuleiro (get-pecas-de-reserva tabuleiro) tipo))

;===========================================================================
;     FUNÇÕES PARA OBTER JOGADA DO JOGADOR HUMANO
;===========================================================================
(defun mostrar-jogador (jogo jogador)
  "função para mostrar o tipo de jogador humano ou computador"
  (cond
   ((and (= jogo 1) (= jogador 1)) 'HUMANO)
   ((and (= jogo 1) (= jogador 2)) 'COMPUTADOR)
   (T (and (= jogo 2) (or (= jogador 1) (= jogador 2))) (concatenate 'string "COMPUTADOR-" (write-to-string jogador)))))

(defun jogador-info (jogador jogo peca)
  (format T "~%")   
  (format T "~@T~@T~@T~@T~@T~@T~@T +============================+~%")
  (format T "~@T~@T~@T~@T~@T~@T~@T      JOGADOR ~A    ~%" (mostrar-jogador jogo jogador))
  (format T "~@T~@T~@T~@T~@T~@T~@T +============================+~%")
  (format T "~%")
  (format T "~@T~@T~@T~@T~@T~@T~@T Proxima peça: ~A~%" peca))

(defun humano-faz-jogada (jogo tabuleiro jogador peca)       
  "Função para ler o diretorio onde se encontra os ficheiros do projeto"
  (jogador-info jogador jogo peca)
  (let* ((jogada (ler-jogada jogador)))         
    (cond 
     ((equal "" jogada)
      (format T "~%ERRO: Jogada ~A é inválida, tente [A <= coluna <= D][linha => 1 <= linha <= 4]!!! ~%" jogada) 
      (humano-faz-jogada jogo tabuleiro jogador peca))
     (T (let* ((coluna (get-index-coluna (subseq jogada 0 1)));;primeiro caracter
               (linha (get-index-linha (subseq jogada 1))));;segundo caracter
          (cond      
           ((or (equal coluna 'NIL)(equal linha 'NIL)) 
            (format T "~%ERRO: Jogada ~A é inválida, tente [A <= coluna <= D][linha => 1 <= linha <= 4]!!!~%" jogada) 
            (humano-faz-jogada jogo tabuleiro jogador peca))
           ((not (casa-vaziap linha coluna tabuleiro));;posicao não disponivel, casa com peça
            (format T "~%ERRO: Jogada ~A é inválida, porque nesta posição já existe peça!!! ~%"  jogada)
            (humano-faz-jogada jogo tabuleiro jogador peca))
           (T (list linha coluna))))))))

(defun ler-jogada (jogador &optional(jogada NIL)) 
  "Função para ler o diretorio onde se encontra os ficheiros do projeto" 
  (cond 
   ((null jogada) (format T "~%Introduza a jogada destino [A <= coluna <= D][linha => 1 <= linha <= 4]: ")  
    (ler-jogada jogador (read-line)))
   (T (string-capitalize jogada))))

(defun get-index-coluna (coluna)
  (cond
   ((string-equal coluna "A") 1)
   ((string-equal coluna "B") 2)
   ((string-equal coluna "C") 3)
   ((string-equal coluna "D") 4)
   (T NIL)))

(defun get-index-linha (linha)
  (cond
   ((string-equal linha "1") 1)
   ((string-equal linha "2") 2)
   ((string-equal linha "3") 3)
   ((string-equal linha "4") 4)
   (T NIL)))

(defun get-coluna-linha (coluna linha)
  (cond
   ((= coluna 1) (concatenate 'string "A" (write-to-string linha)))
   ((= coluna 2) (concatenate 'string "B" (write-to-string linha)))
   ((= coluna 3) (concatenate 'string "C" (write-to-string linha)))
   ((= coluna 4) (concatenate 'string "D" (write-to-string linha)))
   (T NIL)))

;===========================================================================
;     FUNÇÕES PARA OBTER JOGADA DO JOGADOR HUMANO
;===========================================================================
(defun computador-faz-jogada (jogo tabuleiro jogador peca tempo path)
  "Retorna a jogada do PC sob a forma de um novo tabuleiro."
  (jogador-info jogador jogo peca)
  (let* ((tempo-inicial (get-universal-time)) ;vai buscar o tempo atual
         (valor-alfabeta (alfabeta-memo (criar-no (first tabuleiro) (second tabuleiro) jogador) 1 -10e11 10e11 tempo-inicial tempo))
         (novo-tabuleiro *jogada*))
    (escrever-log valor-alfabeta path) novo-tabuleiro))

;===========================================================================
;     ESTATISTICAS
;===========================================================================
(defun escrever-log (valor-alfabeta path)
  "Função que escreve as estatisticas num ficheiro"
  (progn
    (with-open-file (file (concatenate 'string path "log.dat")  
                          :direction :output 
                          :if-exists :append 
                          :if-does-not-exist :create) 
      (escrever-ecra-ficheiro valor-alfabeta file)) ;;escrever no ficheiro
    (resetar-estatistica)));;resetar estatistica

(defun escrever-ecra-ficheiro (valor-alfabeta &optional(tipo 'T))
  (format t "~%")  
  (format tipo "~@T~@T~@T~@T~@T~@T~@T +======================+~%")
  (format tipo "~@T~@T~@T~@T~@T~@T~@T |     ESTATISTICA      |~%")
  (format tipo "~@T~@T~@T~@T~@T~@T~@T +======================+~%")
  (format tipo "Jogada: ~a ~%" *jogada*)
  (format tipo "Valor de Alfa-Beta: ~a ~%"valor-alfabeta)
  (format tipo "Cortes Alfa: ~a~%" *cortes-alfa*)
  (format tipo "Cortes Beta: ~a~%" *cortes-beta*)
  (format tipo "Nos Analisados: ~a~%" *nos-analisados*)
  (format tipo "Tempo Gasto: ~a~%~%" *tempo-gasto*))

(defun resetar-estatistica ()
  "Função para resetar estatistica"
  (setf *cortes-alfa* 0)
  (setf *cortes-beta* 0)
  (setf *nos-analisados* 0)
  (setf *tempo-gasto* 0))



