;===========================================================================
;       FUN��ES PARA RESOLVER OS PROBLEMAS DEFINIDOS
;===========================================================================
;;;; jogo.lisp
;;;; Disciplina de IA - 2020 / 2021
;;------------------------------------------------------------------------------------------------------
;;  FUN�OES PARA RETIRAR PE�AS DE RESERVAS E COLOCAR NO TABULEIRO
;;------------------------------------------------------------------------------------------------------
(defun inserir-peca-no-tabuleiro (x y peca no)
  "Fun��o que vai procurar a linha onde inserir a pe�a atrav�s da coordenada de X"
  (cond
   ;;Quando encontra a linha da coordenada X, vai inserir a pe�a atrav�s da fun��o 'insere-na-linha'
   ((zerop (- x 1)) (cons (insere-na-linha peca y (get-tabuleiro-de-jogo no)) (rest no))) 
   ;;Caso n�o encontre, repete a fun��o, decrementando a coordenada y de x a ser encontrada
   (T (cons (get-tabuleiro-de-jogo no) (inserir-peca-no-tabuleiro (- x 1) y peca (rest no))))))

(defun insere-na-linha (peca y tabuleiro)
  "Fun��o que insere uma pe�a na posi��o de coordenada Y ap�s saber qual a linha da coordenada X"
  (cond
   ((zerop (- y 1)) (cons peca (rest tabuleiro))) ;;Quando encontrar a casa na coordenada y, insere a pe�a na zona de jogo
   (T (cons (first tabuleiro) (insere-na-linha peca (- y 1) (rest tabuleiro)))))) ;;Caso ainda n�o encontre a coordenada y, repete a funcao

(defun existe-peca-no-tabuleiro (peca reserva)
  "Fun��o que verifica a exist�ncia da pe�a na reserva"
  (cond
   ((null reserva) nil) ;;Se a reserva for nula, devolve nil, ou seja, a pe�a n�o existe na reserva.
   ((equal (first reserva) peca) T) ;;Compara a pe�a com a primeira pe�a da reserva. Se forem iguais, devole
   (T (existe-peca-no-tabuleiro peca (rest reserva))))) ;;Caso ainda n�o tenha encontrado, repete a funcao com o resto da reserva

(defun remover-peca-da-reserva (peca reserva)
  "Fun��o que remove a pe�a da reserva"
  (cond
   ((null reserva) nil) ;; Se a reserva for nula, devolve nil.
   ((equal (first reserva) peca) (rest reserva)) ;; Compara a pe�a com a primeira pe�a da reserva. Se forem iguais, remove a pe�a da reserva.
   (T (cons (first reserva) (remover-peca-da-reserva peca (rest reserva)))))) ;;Ainda nao encontrou a pe�a na reserva e por isso repete a funcao.

;;------------------------------------------------------------------------------------------------------
;;   CONSTRUTOR PRINCIPAL
;;------------------------------------------------------------------------------------------------------
(defun criar-no (tabuleiro reserva jogador &optional(profundidade 0) (custo 0) (pai NIL))
  "Fun��o que cria um no"
  (list tabuleiro reserva jogador profundidade custo pai))

;;------------------------------------------------------------------------------------------------------
;;   METODOS SELECTORES
;;------------------------------------------------------------------------------------------------------
(defun get-no-tabuleiro (no)
  "Fun��o que devolve o no completo,ideal para sucessores"
  (first no))

(defun get-tabuleiro-de-jogo (no)
  "Fun��o que devolve o tabuleiro do jogo"
  (first no))

(defun get-pecas-de-reserva (no)
  "Fun��o que devolve as pe�as de reserva de um tabuleiro"
  (second no))

(defun get-jogador (no)
  "Fun��o qe devolve o jogador que fez a jogada"
  (third no))

(defun get-profundidade (no)
  "Fun��o qe devolve a profundidade do n�"
  (fourth no))

(defun get-custo (no)
  "Fun��o que devolve o custo heuristico de um n�"
  (fifth no))

(defun get-no-pai (no)
  "Fun��o qe devolve o(s) pai(s) do n�"
  (sixth no))

;;------------------------------------------------------------------------------------------------------
;;   FUN��ES AUXILIARES
;;------------------------------------------------------------------------------------------------------
(defun trocar-jogador (no)
  "Funcao que recebe um no avalia qual o tipo de jogador desse no e altera para o seguinte jogador"
  (cond
   ((= (get-jogador no) 1) 2)
   (T 1)))

;;------------------------------------------------------------------------------------------------------
;;  OPERADOR PARA COLOCAR PE�A NO TABULEIRO E RETORNA UM N�
;;------------------------------------------------------------------------------------------------------
(defun colocar-peca-no-tabuleiro (x y peca no jogador)
  "Fun��o que recebe dois �ndices (linha e coluna), uma lista que representar� uma pe�a e o tabuleiro 
   com reservas de peca e movimenta a pe�a para a c�lula correspondente, removendo-a da reserva de pe�as. 
   De salientar que o colocar-peca-no-tabuleiro deve contemplar a verifica��o da validade do movimento, ou seja, se a casa 
   que se pretende colocar a pe�a se encontra vazia."
  (cond
   ((not (existe-peca-no-tabuleiro peca (get-pecas-de-reserva no))) nil) 
   (T (let* ((novo-tabuleiro (inserir-peca-no-tabuleiro x y peca (get-tabuleiro-de-jogo no)))
             (pecas-reserva (remover-peca-da-reserva peca (get-pecas-de-reserva no)))
             (profundidade (1+ (get-profundidade no)))
             (custo (1+ (get-custo no))))
        (criar-no novo-tabuleiro pecas-reserva jogador profundidade custo no)))))

;;------------------------------------------------------------------------------------------------------
;;  SUCESSORES
;;------------------------------------------------------------------------------------------------------
(defun no-sucessores (no)
  "Fun��o que recebe um no e devolve uma lista com todos os seus nos sucessores"
  (no-sucessores-aux no (get-pecas-de-reserva no) (get-jogador no)))

(defun no-sucessores-aux (no reserva jogador &optional(x 1) (y 1))
  "Fun��o que cria os nos sucessores de um no"
  (cond
   ((or (null reserva) (null jogador)) ())
   (T (percorrer-posicoes x y (first reserva) no reserva jogador))));;;Envia os dados para serem avaliados pela fun��o que percorre as linhas do tabuleiro

(defun percorrer-posicoes (x y peca no reserva jogador)
  "Fun��o que percorrer as linhas do tabuleiro"
  (cond
   ((= x 5) ());;;Pegou numa pe�a e avaliou-a, enviando a pr�xima para ser avaliada
   ((and (> x 0) (< x 5)) (percorrer-linhaX x y peca no reserva jogador))));;;Se a pe�a ainda n�o tiver sido completamente avaliada, manda-a para ser avaliada pela fun��o percorrer-x

(defun percorrer-linhaX (x y peca no reserva jogador)
  "Fun��o que percorre a linha do no e cria o no no caso da x ser 0"
  (cond
   ((= y 5) (percorrer-posicoes (+ x 1) (- y 4) peca no reserva jogador));;;Pegou numa pe�a e avaliou-a, enviando a pr�xima para ser avaliada
   ((and (> y 0) (< y 5) (casa-vaziap x y no))
    ;;;Caso aind n�o tenha chegado ao fim da pe�a e a pe�a esteja vazia, constr�i a lista com um sucessor poss�vel
    (cons (colocar-peca-no-tabuleiro x y peca no jogador) (percorrer-linhaX x (+ y 1) peca no reserva jogador)))
   (t (percorrer-linhaX x (+ y 1) peca no reserva jogador))));;;Envia a pr�xima pe�a para ser avaliada

(defun casa-vaziap (x y no)
  "Fun��o que recebe dois �ndices (linha e coluna) e o tabuleiro e devolve T se a casa estiver vazia e NIL 
  caso contr�rio. O valor de uma casa vazia no Problema do Quatro � o valor 0"
  (let* ((casa (nth (- y 1) (nth (- x 1) (get-tabuleiro-de-jogo no)))))
    (cond
     ((or (numberp no) (listp casa)) NIL)
     (T (zerop casa) 't))));;;Verifica se as coordenadas x e y s�o 0, se forem retorna T

;;------------------------------------------------------------------------------------------------------
;;   SOLU�AO
;;------------------------------------------------------------------------------------------------------
(defun solucaop (no &optional(x 1))
  "Fun��o que verifica se o tabuleiro � solu��o final"
  (cond
   ((or (null no) (equal 5 x)) nil) ;;Se a zona de jogo for nula ou j� estiver verificado todas as caracter�sticas no tabuleiro, devolve nil.
   ((obter-caracteristica-peca no x) T) ;;Verifica se a caracter�stica � comum numa linha de 4 pe�as. Se encontrar, devolve T.
   (T (solucaop no (+ x 1))))) ;;

(defun obter-caracteristica-peca (no x)
  "Fun��o que verifica se existe uma linha com pe�as com uma caracter�stica em comum"
  (cond
   ((or (null no) (numberp no)) nil)
   ((or (verificar-igualdade (diagonal-1 no x)) 
        (verificar-igualdade (diagonal-2 no x)) 
        (peca-existe-linhaX no x) (peca-existe-linhaY no x)) T);;;compara as caracteristicas das pe�as na vertical/horizontal e diagonal
   ((zerop 0) nil)))

(defun peca-existe-linhaX (no x)
  (cond
   ((or (null no) (numberp no)) nil)
   ((verificar-igualdade(obter-peca-linhaX (get-tabuleiro-de-jogo no) x)) T);;;Verifica as caracteristicas das coordenadas x atrav�s do auxilio de outra fun��o
   (T (peca-existe-linhaX (rest no) x))));;; Recursividade para verificar o resto das coordenadas x

(defun peca-existe-linhaY (no x &optional(y 1))
  (cond
   ((or (null no) (numberp no)) nil)
   ((equal 5 y) nil)
   ((verificar-igualdade (obter-peca-linhaY no x y)) T);;;Verifica as caracteristicas das coordenadas y atrav�s do auxilio de outra fun��o
   (T (peca-existe-linhaY no x (+ y 1)))));;; Recursividade para verificar o resto das coordenadas y

(defun obter-peca-linhaX (tabuleiro x)
  (cond
   ((or (null tabuleiro) (numberp tabuleiro)) nil)
   ;;; Se o car da lista n�o for uma lista (n�o tem uma pe�a) p�ra tamb�m utiliza a recursividade de modo a construir uma lista com os valores da 
   ;; coordenada x, de modo a que os seus valores sejam comparados
   ((not (listp (first tabuleiro))) (cons 0 (obter-peca-linhaX (rest tabuleiro) x)))
   (T (cons (nth (- x 1) (first tabuleiro)) (obter-peca-linhaX (rest tabuleiro) x)))))

(defun obter-peca-linhaY (tabuleiro x y)
  (cond
   ((or (null tabuleiro) (numberp tabuleiro)) nil)
   ;;; Recursividade de modo a construir uma lista com os valores de y, de modo a que os seus valores sejam comparados.
   (T (cons (obter-peca-posY (first tabuleiro) x y) (obter-peca-linhaY (rest tabuleiro) x y)))))

(defun obter-peca-posY (tabuleiro x y)
  (cond   
   ((or (null tabuleiro) (numberp tabuleiro)) nil)
   ((zerop (- y 1));;;se a coordenada y for 0
    (cond
     ((zerop 0) 0)
     ;;;Se o car da lista da coordenada x f�r uma lista (tiver uma pe�a), retorna-se o valor da posi��o anterior da lista das coordenadas x
     ((listp (first tabuleiro)) (nth (- x 1) (first tabuleiro)))))
   ;;;Recursividade, de modo a que todos os valores de y sejam estudados e adicionados � lista, caso existam
   (T (obter-peca-posY (rest tabuleiro) x (- y 1)))))

(defun diagonal-1 (tabuleiro x &optional(y 1))
  "Fun��o que recebe um tabuleiro e retorna uma lista que representa uma diagonal desse tabuleiro. 
   Considere a diagonal-1 como a diagonal a come�ar pela c�lula na 1� linha e 1� coluna."
  (cond
   ((or (null tabuleiro) (numberp tabuleiro)) nil)
   ;; Constr�i uma lista com os valores da diagonal principal de modo a serem comparados entre eles.
   (T (cons (obter-peca-posY (first tabuleiro) x y) (diagonal-1 (rest tabuleiro) x (+ y 1))))))

(defun diagonal-2 (tabuleiro x &optional(y 4))
  "Fun��o que recebe um tabuleiro e retorna uma lista que representa uma diagonal desse tabuleiro. 
   Considere a diagonal-2 como a diagonal a come�ar pela c�lula na �ltima linha e 1� coluna."
  (cond
   ((or (null tabuleiro) (numberp tabuleiro)) nil)
   ;;; Constr�i uma lista com valores da diagonal secund�ria, o valor de y vai decrementando. de modo a serem comparados entre eles.
   (T (cons (obter-peca-posY (first tabuleiro) x y) (diagonal-2 (rest tabuleiro) x (- y 1))))))

(defun verificar-igualdade (lista)
  "Verifica a igualdade de entre as pe�as da lista enviada"
  (cond
   ((OR (null lista) (equal 0 (first lista))) nil);;;se o membro f�r 0 ou se a lista estiver nula, p�ra
   ((equal (list-length lista) 1) T);;;se a lista s� tiver 1 membro retorna true
   (T (cond
       ((equal (first lista) (first (rest lista))) (verificar-igualdade (rest lista)));;;verifica a igualdade entre os membros das listas
       ((zerop 0) nil)))));;;condi��o de paragem

;;;; Fun��o para o campeonato
(defun jogar (estado tempo path)
  "Fun��o que servir� para jogar no campeonato.
   Recebe um tabuleiro, a propria pe�a e o tempo maximo de execu��o"
  (let* ((tempo-inicial (get-universal-time))
         (valor-alfabeta (alfa-beta (criar-no (first estado) (second estado) *jogador1*) 1 -10e11 10e11 tempo-inicial tempo))
         (novo-estado *jogada*)) 
    (escrever-log valor-alfabeta path) novo-estado))