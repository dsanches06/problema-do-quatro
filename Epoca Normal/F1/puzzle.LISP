;===========================================================================
;       FUN��ES PARA RESOLVER OS PROBLEMAS DEFINIDOS
;===========================================================================
;;;; puzzle.lisp
;;;; Disciplina de IA - 2020 / 2021
;;------------------------------------------------------------------------------------------------------
;;  FUN�OES PARA COLOCAR RETIRAR PE�AS DE RESERVAS E COLOCAR NO TABULEIRO
;;------------------------------------------------------------------------------------------------------
(defun inserir-peca-no-tabuleiro (x y peca zonadejogo)
  "Fun��o que vai procurar a linha onde inserir a pe�a atrav�s da coordenada de X"
  (cond
   ((zerop (- x 1 )) (cons (insere-na-linha peca y (first zonadejogo)) (rest zonadejogo))) ;;Quando encontra a linha da coordenada X, vai inserir a pe�a atrav�s da fun��o 'insere-na-linha'
   (T (cons (first zonadejogo) (inserir-peca-no-tabuleiro (- x 1) y peca (rest zonadejogo)))))) ;;Caso n�o encontre, repete a fun��o, decrementando a coordenada y de x a ser encontrada

(defun insere-na-linha (peca y linha)
  "Fun��o que insere uma pe�a na posi��o de coordenada Y ap�s saber qual a linha da coordenada X"
  (cond
   ((zerop (- y 1)) (cons peca (rest linha))) ;;Quando encontrar a casa na coordenada y, insere a pe�a na zona de jogo
   (T (cons (first linha) (insere-na-linha peca (- y 1) (rest linha)))))) ;;Caso ainda n�o encontre a coordenada y, repete a funcao

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
(defun criar-no (no reserva &optional(x 1) (y 1))
  "Fun��o que cria os nos sucessores de um no"
  (cond
   ((null reserva) ())
   (T (percorrer-posicoes x y (first reserva) no reserva))));;;Envia os dados para serem avaliados pela fun��o que percorre as linhas do tabuleiro

(defun percorrer-posicoes (x y peca no reserva)
  "Fun��o que percorrer as linhas do tabuleiro"
  (cond
   ((= x 5) (criar-no no (rest reserva) (- x 4) y));;;Pegou numa pe�a e avaliou-a, enviando a pr�xima para ser avaliada
   ((and (> x 0) (< x 5)) (percorrer-linha x y peca no reserva))));;;Se a pe�a ainda n�o tiver sido completamente avaliada, manda-a para ser avaliada pela func��o percorrer-x

(defun percorrer-linha (x y peca no reserva)
  "Fun��o que percorre a linha do no e cria o no no caso da posicao ser 0"
  (cond
   ((= y 5) (percorrer-posicoes (+ x 1) (- y 4) peca no reserva));;;Pegou numa pe�a e avaliou-a, enviando a pr�xima para ser avaliada
   ((and (> y 0) (< y 5) (posicao-disponivel x y (first no))) 
    (cons (list (colocar-peca-no-tabuleiro x y peca (first no)) no) (percorrer-linha x (+ y 1) peca no reserva)));;;Caso aind n�o tenha chegado ao fim da pe�a e a pe�a esteja vazia, constr�i a lista com um sucessor poss�vel
   (t (percorrer-linha x (+ y 1) peca no reserva))));;;Envia a pr�xima pe�a para ser avaliada

(defun posicao-disponivel (x y no)
  "Fun��o que verifica se a posi��o de coordenada x y � 0"
  (cond
   ((equal (nth (- y 1) (nth (- x 1) (first no))) 0) 't)));;;Verifica se as coordenadas x e y s�o 0, se forem retorna T

;;------------------------------------------------------------------------------------------------------
;;   METODOS SELECTORES
;;------------------------------------------------------------------------------------------------------
(defun get-no-tabuleiro (no) 
  "Fun��o que devolve a zona de jogo e a reserva"
  (first no))

(defun get-zona-de-jogo (no)
  "Fun��o que devolve a zona de jogo"
  (first (first no)))

(defun get-no-pai (no)
  "Fun��o qe devolve o(s) pai(s) do n�"
  (first (rest no)))

(defun get-no-reserva (no)
  "Fun��o que devolve o n� reserva"
  (first (rest (first no))))

(defun get-tabuleiro-zona-de-jogo (tabuleiro)
  "Fun��o que devolve o tabuleiro em si"
  (first tabuleiro))

(defun get-tabuleiro-de-reserva (tabuleiro)
  "Fun��o que devolve a reserva de umtabuleiro"
  (rest tabuleiro))

(defun get-custo-heuristica (no) ;;; Retorna o custo heuristico de um n�
  "Fun��o que devolve o custo heuristico de um n�"
  (third no))

(defun get-profundidade (no)
  "Fun��o que devolve a profundidade de um no"
  (fourth no))

(defun get-no-sem-custo-heuristica (no)
  "Fun��o que devolve o tabuleiro sem custo heuristico"
  (cons (first (first no)) (list(get-no-pai no))))

(defun get-heuristica (no &optional (h 1))
  "Fun��o que devolve o custo heuristico de um n�"
  (cond
   ((null no) nil)
   ((= h 1) (heuristica1 (get-zona-de-jogo no)))
   ((= h 2) (heuristica2 (get-no-tabuleiro no)))))

(defun get-profundidade-no (no) 
  "Fun��o que devolve o numero de pais de um certo n�, sendo este a profundidade"
  (cond
   ((eq (first (get-no-pai no)) nil) 0)
   (T(comprimento (get-no-pai no)))))

;;------------------------------------------------------------------------------------------------------
;;   OPERADOR
;;------------------------------------------------------------------------------------------------------
(defun colocar-peca-no-tabuleiro (x y peca tabuleiro)
  "Fun��o a ser chamada para colocar uma peca no tabuleiro"
  (cond
   ((not (existe-peca-no-tabuleiro peca (first (get-tabuleiro-de-reserva tabuleiro)))) nil) 
   (T (list (inserir-peca-no-tabuleiro x y peca (get-tabuleiro-zona-de-jogo tabuleiro)) 
            (remover-peca-da-reserva peca (first (get-tabuleiro-de-reserva tabuleiro)))))))

;;------------------------------------------------------------------------------------------------------
;;  SUCESSORES
;;------------------------------------------------------------------------------------------------------
(defun gerar-sucessores (no)
  "Fun��o que recebe um no e devolve uma lista com todos os seus nos sucessores"
  (criar-no no (first (get-tabuleiro-de-reserva (first no)))))

;;------------------------------------------------------------------------------------------------------
;;   SOLU�AO
;;------------------------------------------------------------------------------------------------------
(defun solucaop (zonadejogo &optional(peca 1))
  "Fun��o que verifica se o tabuleiro � solu��o final"
  (cond
   ((OR (null zonadejogo) (equal 5 peca)) nil) ;;Se a zona de jogo for nula ou j� estiver verificado todas as caracter�sticas no tabuleiro, devolve nil.
   ((obter-caracteristica-peca zonadejogo peca) T) ;;Verifica se a caracter�stica � comum numa linha de 4 pe�as. Se encontrar, devolve T.
   (T (solucaop zonadejogo (+ peca 1))))) ;;

(defun obter-caracteristica-peca (zonadejogo peca)
  "Fun��o que verifica se existe uma linha com pe�as com uma caracter�stica em comum"
  (cond
   ((null zonadejogo) nil)
   ((OR (verificar-igualdade (obter-peca-na-diagonal zonadejogo peca)) 
        (verificar-igualdade (obter-peca-igual-na-diagonal zonadejogo peca)) 
        (peca-existe-linhaX zonadejogo peca) (peca-existe-linhaY zonadejogo peca)) T);;;compara as caracteristicas das pe�as na vertical/horizontal e diagonal
   ((zerop 0) nil)))

(defun peca-existe-linhaX (zonadejogo peca)
  (cond
   ((null zonadejogo) nil)
   ((verificar-igualdade(obter-peca-linhaX (first zonadejogo) peca)) T);;;Verifica as caracteristicas das coordenadas x atrav�s do auxilio de outra fun��o
   (T (peca-existe-linhaX (rest zonadejogo) peca))));;; Recursividade para verificar o resto das coordenadas x

(defun peca-existe-linhaY (zonadejogo peca &optional(y 1))
  (cond
   ((null zonadejogo) nil)
   ((equal 5 y) nil)
   ((verificar-igualdade (obter-peca-linhaY zonadejogo peca y)) T);;;Verifica as caracteristicas das coordenadas y atrav�s do auxilio de outra fun��o
   (T (peca-existe-linhaY zonadejogo peca (+ y 1)))));;; Recursividade para verificar o resto das coordenadas y

(defun obter-peca-linhaX (x pos)
  (cond
   ((null x) nil)
   ((not (listp (first x))) (cons 0 (obter-peca-linhaX (rest x) pos)));;;se o car da lista n�o for uma lista (n�o tem uma pe�a) p�ra tamb�m utiliza a recursividade de modo a construir uma lista com os valores da coordenada x, de modo a que os seus valores sejam comparados
   (T (cons (nth (- pos 1) (first x)) (obter-peca-linhaX (rest x) pos)))))

(defun obter-peca-linhaY (zonadejogo peca y)
  (cond
   ((null zonadejogo) nil)
   (T (cons (obter-peca-posY (first zonadejogo) peca y) (obter-peca-linhaY (rest zonadejogo) peca y)))));;;Recursividade de modo a construir uma lista com os valores de y, de modo a que os seus valores sejam comparados.

(defun obter-peca-posY (x peca y)
  (cond
   ((null x) nil)
   ((zerop (- y 1));;;se a coordenada y for 0
    (cond
     ((listp (first x)) (nth (- peca 1) (first x)));;;Se o car da lista da coordenada x f�r uma lista (tiver uma pe�a), retorna-se o valor da posi��o anterior da lista das coordenadas x
     ((zerop 0) 0)))
   (T (obter-peca-posY (rest x) peca (- y 1)))));;;Recursividade, de modo a que todos os valores de y sejam estudados e adicionados � lista, caso existam

(defun obter-peca-na-diagonal (zonadejogo peca &optional(y 1))
  (cond
   ((null zonadejogo) nil)
   (T (cons (obter-peca-posY (first zonadejogo) peca y) (obter-peca-na-diagonal (rest zonadejogo) peca (+ y 1))))));;; Constr�i uma lista com os valores da diagonal principal de modo a serem comparados entre eles.


(defun obter-peca-igual-na-diagonal (zonadejogo peca &optional(y 4))
  (cond
   ((null zonadejogo) nil)
   (T (cons (obter-peca-posY (first zonadejogo) peca y) 
            (obter-peca-igual-na-diagonal (rest zonadejogo) peca (- y 1))))));;; Constr�i uma lista com valores da diagonal secund�ria, o valor de y vai decrementando. de modo a serem comparados entre eles.

(defun verificar-igualdade (lista)
  "Verifica a igualdade de entre as pe�as da lista enviada"
  (cond
   ((OR (null lista) (equal 0 (first lista))) nil);;;se o membro f�r 0 ou se a lista estiver nula, p�ra
   ((equal (list-length lista) 1) T);;;se a lista s� tiver 1 membro retorna true
   (T (cond
       ((equal (first lista) (first(rest lista))) (verificar-igualdade (rest lista)));;;verifica a igualdade entre os membros das listas
       ((zerop 0) nil)))));;;condi��o de paragem


(defun solucao-nao-encontrada ()
  "Fun��o que mostra uma mensagem de solu��o n�o encontrada"
  (format T "~%")
  (format T "~@T~@T~@T~@T +=========================================+~%")
  (format T "~@T~@T~@T~@T |         SOLU��O N�O ENCONTRADA!!!       |~%")
  (format T "~@T~@T~@T~@T +=========================================+~%~%"))	

;;------------------------------------------------------------------------------------------------------
;;   FUN��ES DE C�LCULO DE HEURISTICAS
;;------------------------------------------------------------------------------------------------------
(defun heuristica1 (no)
  "h(x)=4-p(x) sendo p(x) o nr de pe�as max ja alinhadas com a mesma caracteristica"
  (- 4 (p no)))

(defun heuristica2 (no)
  "h(x)=(4-p(x))+q(x) sendo p(x) o nr de pe�as max ja alinhadas com a mesma caracteristica e q(x) � o nr de pe�as na reserva"
  (+ (- 4 (p (first no))) (comprimento(first (rest no)))))

(defun comprimento (lista)
  "devolve o comprimento de uma lista"
  (cond
   ((null lista) 0)
   (T (1+ (comprimento (rest lista))))))

(defun p (no)
  "devolve o valor de p(x), ou seja, o nr de pe�as max ja alinhadas com a mesma caracteristica"
  (numero-maximo 
   (numero-maximo (caracteristica-comum-peca (obter-peca-na-diagonal no 1)) 
                  (caracteristica-comum-peca (obter-peca-igual-na-diagonal no 1)) 
                  (maior-numero-linha 'obter-linhaX no 1) 
                  (maior-numero-linha 'obter-linhaY no 1))
   (numero-maximo (caracteristica-comum-peca (obter-peca-na-diagonal no 2)) 
                  (caracteristica-comum-peca (obter-peca-igual-na-diagonal no 2)) 
                  (maior-numero-linha 'obter-linhaX no 2) 
                  (maior-numero-linha 'obter-linhaY no 2)) 
   (numero-maximo (caracteristica-comum-peca (obter-peca-na-diagonal no 3)) 
                  (caracteristica-comum-peca ( obter-peca-igual-na-diagonal no 3)) 
                  (maior-numero-linha 'obter-linhaX no 3) 
                  (maior-numero-linha 'obter-linhaY no 3))
   (numero-maximo (caracteristica-comum-peca (obter-peca-na-diagonal no 4))
                  (caracteristica-comum-peca (obter-peca-igual-na-diagonal no 4))
                  (maior-numero-linha 'obter-linhaX no 4) 
                  (maior-numero-linha 'obter-linhaY no 4))))

(defun maior-numero-linha (funcao no caract)
  "Devolve o maior numero de caracteristicas em comum"
  (numero-maximo (funcall funcao no caract 1)
                 (funcall funcao no caract 2)
                 (funcall funcao no caract 3)
                 (funcall funcao no caract 4)))

(defun numero-maximo (a b c d)
  "devolve o maior de 4 valores"
  (cond
   ((and (>= a b) (>= a c)) (maior-numero-aux a d))
   ((and (>= b a) (>= b c)) (maior-numero-aux b d))
   ((and (>= c a) (>= c b)) (maior-numero-aux c d))))

(defun maior-numero-aux (a b)
  "Devolve o maior de 2 valores"
  (cond
   ((>= a b) a)
   ((>= b a) b)))

(defun obter-linhaX (no caract linha)
  "Verifica as caracteristicas em comum de uma linha"
  (caracteristica-comum-peca (obter-peca-linhaX (nth (1- linha) no) caract)))

(defun obter-linhaY (no caract linhay)
  "Verifica as caracteristicas em comum de uma coluna"
  (caracteristica-comum-peca ( obter-peca-linhaY no caract linhay)))

(defun caracteristica-comum-peca (lista &optional(pos 0) (maximo 0))
  "devolve o nr de caracteristicas em comum de uma linha"
  (cond
   ((= pos 4)(maior-numero-aux maximo (somar-lista (mapcar #'(lambda (n) (binario n (nth pos lista))) lista))))
   (t (caracteristica-comum-peca lista (1+ pos)  (maior-numero-aux maximo (somar-lista (mapcar #'(lambda (n) (binario n (nth pos lista))) lista)))))))

(defun binario (n caract)
  "devolve 0 se um atomo for nil e 1 se o valor for T"
  (cond
   ((null caract) 0) 
   ((equal caract n) 1)
   (T (zerop 0) 0)))

(defun somar-lista (lista)
  "soma os elementos de uma lista"
  (cond
   ((null lista) 0)
   (T (+ (first lista) (somar-lista (rest lista))))))
