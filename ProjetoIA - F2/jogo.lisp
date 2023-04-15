;===========================================================================
;       FUNÇÕES PARA RESOLVER OS PROBLEMAS DEFINIDOS
;===========================================================================
;;;; jogo.lisp
;;;; Disciplina de IA - 2020 / 2021
;;------------------------------------------------------------------------------------------------------
;;  FUNÇOES PARA RETIRAR PEÇAS DE RESERVAS E COLOCAR NO TABULEIRO
;;------------------------------------------------------------------------------------------------------
(defun inserir-peca-no-tabuleiro (x y peca no)
  "Função que vai procurar a linha onde inserir a peça através da coordenada de X"
  (cond
   ;;Quando encontra a linha da coordenada X, vai inserir a peça através da função 'insere-na-linha'
   ((zerop (- x 1)) (cons (insere-na-linha peca y (get-tabuleiro-de-jogo no)) (rest no))) 
   ;;Caso não encontre, repete a função, decrementando a coordenada y de x a ser encontrada
   (T (cons (get-tabuleiro-de-jogo no) (inserir-peca-no-tabuleiro (- x 1) y peca (rest no))))))

(defun insere-na-linha (peca y tabuleiro)
  "Função que insere uma peça na posição de coordenada Y após saber qual a linha da coordenada X"
  (cond
   ((zerop (- y 1)) (cons peca (rest tabuleiro))) ;;Quando encontrar a casa na coordenada y, insere a peça na zona de jogo
   (T (cons (first tabuleiro) (insere-na-linha peca (- y 1) (rest tabuleiro)))))) ;;Caso ainda não encontre a coordenada y, repete a funcao

(defun existe-peca-no-tabuleiro (peca reserva)
  "Função que verifica a existência da peça na reserva"
  (cond
   ((null reserva) nil) ;;Se a reserva for nula, devolve nil, ou seja, a peça não existe na reserva.
   ((equal (first reserva) peca) T) ;;Compara a peça com a primeira peça da reserva. Se forem iguais, devole
   (T (existe-peca-no-tabuleiro peca (rest reserva))))) ;;Caso ainda não tenha encontrado, repete a funcao com o resto da reserva

(defun remover-peca-da-reserva (peca reserva)
  "Função que remove a peça da reserva"
  (cond
   ((null reserva) nil) ;; Se a reserva for nula, devolve nil.
   ((equal (first reserva) peca) (rest reserva)) ;; Compara a peça com a primeira peça da reserva. Se forem iguais, remove a peça da reserva.
   (T (cons (first reserva) (remover-peca-da-reserva peca (rest reserva)))))) ;;Ainda nao encontrou a peça na reserva e por isso repete a funcao.

;;------------------------------------------------------------------------------------------------------
;;   CONSTRUTOR PRINCIPAL
;;------------------------------------------------------------------------------------------------------
(defun criar-no (tabuleiro reserva jogador &optional(profundidade 0) (custo 0) (pai NIL))
  "Função que cria um no"
  (list tabuleiro reserva jogador profundidade custo pai))

;;------------------------------------------------------------------------------------------------------
;;   METODOS SELECTORES
;;------------------------------------------------------------------------------------------------------
(defun get-no-tabuleiro (no)
  "Função que devolve o no completo,ideal para sucessores"
  (first no))

(defun get-tabuleiro-de-jogo (no)
  "Função que devolve o tabuleiro do jogo"
  (first no))

(defun get-pecas-de-reserva (no)
  "Função que devolve as peças de reserva de um tabuleiro"
  (second no))

(defun get-jogador (no)
  "Função qe devolve o jogador que fez a jogada"
  (third no))

(defun get-profundidade (no)
  "Função qe devolve a profundidade do nó"
  (fourth no))

(defun get-custo (no)
  "Função que devolve o custo heuristico de um nó"
  (fifth no))

(defun get-no-pai (no)
  "Função qe devolve o(s) pai(s) do nó"
  (sixth no))

;;------------------------------------------------------------------------------------------------------
;;   FUNÇÕES AUXILIARES
;;------------------------------------------------------------------------------------------------------
(defun trocar-jogador (no)
  "Funcao que recebe um no avalia qual o tipo de jogador desse no e altera para o seguinte jogador"
  (cond
   ((= (get-jogador no) 1) 2)
   (T 1)))

;;------------------------------------------------------------------------------------------------------
;;  OPERADOR PARA COLOCAR PEÇA NO TABULEIRO E RETORNA UM NÓ
;;------------------------------------------------------------------------------------------------------
(defun colocar-peca-no-tabuleiro (x y peca no jogador)
  "Função que recebe dois índices (linha e coluna), uma lista que representará uma peça e o tabuleiro 
   com reservas de peca e movimenta a peça para a célula correspondente, removendo-a da reserva de peças. 
   De salientar que o colocar-peca-no-tabuleiro deve contemplar a verificação da validade do movimento, ou seja, se a casa 
   que se pretende colocar a peça se encontra vazia."
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
  "Função que recebe um no e devolve uma lista com todos os seus nos sucessores"
  (no-sucessores-aux no (get-pecas-de-reserva no) (get-jogador no)))

(defun no-sucessores-aux (no reserva jogador &optional(x 1) (y 1))
  "Função que cria os nos sucessores de um no"
  (cond
   ((or (null reserva) (null jogador)) ())
   (T (percorrer-posicoes x y (first reserva) no reserva jogador))));;;Envia os dados para serem avaliados pela função que percorre as linhas do tabuleiro

(defun percorrer-posicoes (x y peca no reserva jogador)
  "Função que percorrer as linhas do tabuleiro"
  (cond
   ((= x 5) ());;;Pegou numa peça e avaliou-a, enviando a próxima para ser avaliada
   ((and (> x 0) (< x 5)) (percorrer-linhaX x y peca no reserva jogador))));;;Se a peça ainda não tiver sido completamente avaliada, manda-a para ser avaliada pela função percorrer-x

(defun percorrer-linhaX (x y peca no reserva jogador)
  "Função que percorre a linha do no e cria o no no caso da x ser 0"
  (cond
   ((= y 5) (percorrer-posicoes (+ x 1) (- y 4) peca no reserva jogador));;;Pegou numa peça e avaliou-a, enviando a próxima para ser avaliada
   ((and (> y 0) (< y 5) (casa-vaziap x y no))
    ;;;Caso aind não tenha chegado ao fim da peça e a peça esteja vazia, constrói a lista com um sucessor possível
    (cons (colocar-peca-no-tabuleiro x y peca no jogador) (percorrer-linhaX x (+ y 1) peca no reserva jogador)))
   (t (percorrer-linhaX x (+ y 1) peca no reserva jogador))));;;Envia a próxima peça para ser avaliada

(defun casa-vaziap (x y no)
  "Função que recebe dois índices (linha e coluna) e o tabuleiro e devolve T se a casa estiver vazia e NIL 
  caso contrário. O valor de uma casa vazia no Problema do Quatro é o valor 0"
  (let* ((casa (nth (- y 1) (nth (- x 1) (get-tabuleiro-de-jogo no)))))
    (cond
     ((or (numberp no) (listp casa)) NIL)
     (T (zerop casa) 't))));;;Verifica se as coordenadas x e y são 0, se forem retorna T

;;------------------------------------------------------------------------------------------------------
;;   SOLUÇAO
;;------------------------------------------------------------------------------------------------------
(defun solucaop (no &optional(x 1))
  "Função que verifica se o tabuleiro é solução final"
  (cond
   ((or (null no) (equal 5 x)) nil) ;;Se a zona de jogo for nula ou já estiver verificado todas as características no tabuleiro, devolve nil.
   ((obter-caracteristica-peca no x) T) ;;Verifica se a característica é comum numa linha de 4 peças. Se encontrar, devolve T.
   (T (solucaop no (+ x 1))))) ;;

(defun obter-caracteristica-peca (no x)
  "Função que verifica se existe uma linha com peças com uma característica em comum"
  (cond
   ((or (null no) (numberp no)) nil)
   ((or (verificar-igualdade (diagonal-1 no x)) 
        (verificar-igualdade (diagonal-2 no x)) 
        (peca-existe-linhaX no x) (peca-existe-linhaY no x)) T);;;compara as caracteristicas das peças na vertical/horizontal e diagonal
   ((zerop 0) nil)))

(defun peca-existe-linhaX (no x)
  (cond
   ((or (null no) (numberp no)) nil)
   ((verificar-igualdade(obter-peca-linhaX (get-tabuleiro-de-jogo no) x)) T);;;Verifica as caracteristicas das coordenadas x através do auxilio de outra função
   (T (peca-existe-linhaX (rest no) x))));;; Recursividade para verificar o resto das coordenadas x

(defun peca-existe-linhaY (no x &optional(y 1))
  (cond
   ((or (null no) (numberp no)) nil)
   ((equal 5 y) nil)
   ((verificar-igualdade (obter-peca-linhaY no x y)) T);;;Verifica as caracteristicas das coordenadas y através do auxilio de outra função
   (T (peca-existe-linhaY no x (+ y 1)))));;; Recursividade para verificar o resto das coordenadas y

(defun obter-peca-linhaX (tabuleiro x)
  (cond
   ((or (null tabuleiro) (numberp tabuleiro)) nil)
   ;;; Se o car da lista não for uma lista (não tem uma peça) pára também utiliza a recursividade de modo a construir uma lista com os valores da 
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
     ;;;Se o car da lista da coordenada x fôr uma lista (tiver uma peça), retorna-se o valor da posição anterior da lista das coordenadas x
     ((listp (first tabuleiro)) (nth (- x 1) (first tabuleiro)))))
   ;;;Recursividade, de modo a que todos os valores de y sejam estudados e adicionados á lista, caso existam
   (T (obter-peca-posY (rest tabuleiro) x (- y 1)))))

(defun diagonal-1 (tabuleiro x &optional(y 1))
  "Função que recebe um tabuleiro e retorna uma lista que representa uma diagonal desse tabuleiro. 
   Considere a diagonal-1 como a diagonal a começar pela célula na 1ª linha e 1ª coluna."
  (cond
   ((or (null tabuleiro) (numberp tabuleiro)) nil)
   ;; Constrói uma lista com os valores da diagonal principal de modo a serem comparados entre eles.
   (T (cons (obter-peca-posY (first tabuleiro) x y) (diagonal-1 (rest tabuleiro) x (+ y 1))))))

(defun diagonal-2 (tabuleiro x &optional(y 4))
  "Função que recebe um tabuleiro e retorna uma lista que representa uma diagonal desse tabuleiro. 
   Considere a diagonal-2 como a diagonal a começar pela célula na última linha e 1ª coluna."
  (cond
   ((or (null tabuleiro) (numberp tabuleiro)) nil)
   ;;; Constrói uma lista com valores da diagonal secundária, o valor de y vai decrementando. de modo a serem comparados entre eles.
   (T (cons (obter-peca-posY (first tabuleiro) x y) (diagonal-2 (rest tabuleiro) x (- y 1))))))

(defun verificar-igualdade (lista)
  "Verifica a igualdade de entre as peças da lista enviada"
  (cond
   ((OR (null lista) (equal 0 (first lista))) nil);;;se o membro fôr 0 ou se a lista estiver nula, pára
   ((equal (list-length lista) 1) T);;;se a lista só tiver 1 membro retorna true
   (T (cond
       ((equal (first lista) (first (rest lista))) (verificar-igualdade (rest lista)));;;verifica a igualdade entre os membros das listas
       ((zerop 0) nil)))));;;condição de paragem

;;;; Função para o campeonato
(defun jogar (estado tempo path)
  "Função que servirá para jogar no campeonato.
   Recebe um tabuleiro, a propria peça e o tempo maximo de execução"
  (let* ((tempo-inicial (get-universal-time))
         (valor-alfabeta (alfa-beta (criar-no (first estado) (second estado) *jogador1*) 1 -10e11 10e11 tempo-inicial tempo))
         (novo-estado *jogada*)) 
    (escrever-log valor-alfabeta path) novo-estado))