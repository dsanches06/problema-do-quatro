;===========================================================================
;       FUNÇÕES PARA RESOLVER OS PROBLEMAS DEFINIDOS
;===========================================================================
;;;; puzzle.lisp
;;;; Disciplina de IA - 2020 / 2021
;;------------------------------------------------------------------------------------------------------
;;  FUNÇOES PARA COLOCAR RETIRAR PEÇAS DE RESERVAS E COLOCAR NO TABULEIRO
;;------------------------------------------------------------------------------------------------------
(defun inserir-peca-no-tabuleiro (x y peca zonadejogo)
  "Função que vai procurar a linha onde inserir a peça através da coordenada de X"
  (cond
   ((zerop (- x 1 )) (cons (insere-na-linha peca y (first zonadejogo)) (rest zonadejogo))) ;;Quando encontra a linha da coordenada X, vai inserir a peça através da função 'insere-na-linha'
   (T (cons (first zonadejogo) (inserir-peca-no-tabuleiro (- x 1) y peca (rest zonadejogo)))))) ;;Caso não encontre, repete a função, decrementando a coordenada y de x a ser encontrada

(defun insere-na-linha (peca y linha)
  "Função que insere uma peça na posição de coordenada Y após saber qual a linha da coordenada X"
  (cond
   ((zerop (- y 1)) (cons peca (rest linha))) ;;Quando encontrar a casa na coordenada y, insere a peça na zona de jogo
   (T (cons (first linha) (insere-na-linha peca (- y 1) (rest linha)))))) ;;Caso ainda não encontre a coordenada y, repete a funcao

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
(defun criar-no (no reserva &optional(x 1) (y 1))
  "Função que cria os nos sucessores de um no"
  (cond
   ((null reserva) ())
   (T (percorrer-posicoes x y (first reserva) no reserva))));;;Envia os dados para serem avaliados pela função que percorre as linhas do tabuleiro

(defun percorrer-posicoes (x y peca no reserva)
  "Função que percorrer as linhas do tabuleiro"
  (cond
   ((= x 5) (criar-no no (rest reserva) (- x 4) y));;;Pegou numa peça e avaliou-a, enviando a próxima para ser avaliada
   ((and (> x 0) (< x 5)) (percorrer-linha x y peca no reserva))));;;Se a peça ainda não tiver sido completamente avaliada, manda-a para ser avaliada pela funcção percorrer-x

(defun percorrer-linha (x y peca no reserva)
  "Função que percorre a linha do no e cria o no no caso da posicao ser 0"
  (cond
   ((= y 5) (percorrer-posicoes (+ x 1) (- y 4) peca no reserva));;;Pegou numa peça e avaliou-a, enviando a próxima para ser avaliada
   ((and (> y 0) (< y 5) (posicao-disponivel x y (first no))) 
    (cons (list (colocar-peca-no-tabuleiro x y peca (first no)) no) (percorrer-linha x (+ y 1) peca no reserva)));;;Caso aind não tenha chegado ao fim da peça e a peça esteja vazia, constrói a lista com um sucessor possível
   (t (percorrer-linha x (+ y 1) peca no reserva))));;;Envia a próxima peça para ser avaliada

(defun posicao-disponivel (x y no)
  "Função que verifica se a posição de coordenada x y é 0"
  (cond
   ((equal (nth (- y 1) (nth (- x 1) (first no))) 0) 't)));;;Verifica se as coordenadas x e y são 0, se forem retorna T

;;------------------------------------------------------------------------------------------------------
;;   METODOS SELECTORES
;;------------------------------------------------------------------------------------------------------
(defun get-no-tabuleiro (no) 
  "Função que devolve a zona de jogo e a reserva"
  (first no))

(defun get-zona-de-jogo (no)
  "Função que devolve a zona de jogo"
  (first (first no)))

(defun get-no-pai (no)
  "Função qe devolve o(s) pai(s) do nó"
  (first (rest no)))

(defun get-no-reserva (no)
  "Função que devolve o nó reserva"
  (first (rest (first no))))

(defun get-tabuleiro-zona-de-jogo (tabuleiro)
  "Função que devolve o tabuleiro em si"
  (first tabuleiro))

(defun get-tabuleiro-de-reserva (tabuleiro)
  "Função que devolve a reserva de umtabuleiro"
  (rest tabuleiro))

(defun get-custo-heuristica (no) ;;; Retorna o custo heuristico de um nó
  "Função que devolve o custo heuristico de um nó"
  (third no))

(defun get-profundidade (no)
  "Função que devolve a profundidade de um no"
  (fourth no))

(defun get-no-sem-custo-heuristica (no)
  "Função que devolve o tabuleiro sem custo heuristico"
  (cons (first (first no)) (list(get-no-pai no))))

(defun get-heuristica (no &optional (h 1))
  "Função que devolve o custo heuristico de um nó"
  (cond
   ((null no) nil)
   ((= h 1) (heuristica1 (get-zona-de-jogo no)))
   ((= h 2) (heuristica2 (get-no-tabuleiro no)))))

(defun get-profundidade-no (no) 
  "Função que devolve o numero de pais de um certo nó, sendo este a profundidade"
  (cond
   ((eq (first (get-no-pai no)) nil) 0)
   (T(comprimento (get-no-pai no)))))

;;------------------------------------------------------------------------------------------------------
;;   OPERADOR
;;------------------------------------------------------------------------------------------------------
(defun colocar-peca-no-tabuleiro (x y peca tabuleiro)
  "Função a ser chamada para colocar uma peca no tabuleiro"
  (cond
   ((not (existe-peca-no-tabuleiro peca (first (get-tabuleiro-de-reserva tabuleiro)))) nil) 
   (T (list (inserir-peca-no-tabuleiro x y peca (get-tabuleiro-zona-de-jogo tabuleiro)) 
            (remover-peca-da-reserva peca (first (get-tabuleiro-de-reserva tabuleiro)))))))

;;------------------------------------------------------------------------------------------------------
;;  SUCESSORES
;;------------------------------------------------------------------------------------------------------
(defun gerar-sucessores (no)
  "Função que recebe um no e devolve uma lista com todos os seus nos sucessores"
  (criar-no no (first (get-tabuleiro-de-reserva (first no)))))

;;------------------------------------------------------------------------------------------------------
;;   SOLUÇAO
;;------------------------------------------------------------------------------------------------------
(defun solucaop (zonadejogo &optional(peca 1))
  "Função que verifica se o tabuleiro é solução final"
  (cond
   ((OR (null zonadejogo) (equal 5 peca)) nil) ;;Se a zona de jogo for nula ou já estiver verificado todas as características no tabuleiro, devolve nil.
   ((obter-caracteristica-peca zonadejogo peca) T) ;;Verifica se a característica é comum numa linha de 4 peças. Se encontrar, devolve T.
   (T (solucaop zonadejogo (+ peca 1))))) ;;

(defun obter-caracteristica-peca (zonadejogo peca)
  "Função que verifica se existe uma linha com peças com uma característica em comum"
  (cond
   ((null zonadejogo) nil)
   ((OR (verificar-igualdade (obter-peca-na-diagonal zonadejogo peca)) 
        (verificar-igualdade (obter-peca-igual-na-diagonal zonadejogo peca)) 
        (peca-existe-linhaX zonadejogo peca) (peca-existe-linhaY zonadejogo peca)) T);;;compara as caracteristicas das peças na vertical/horizontal e diagonal
   ((zerop 0) nil)))

(defun peca-existe-linhaX (zonadejogo peca)
  (cond
   ((null zonadejogo) nil)
   ((verificar-igualdade(obter-peca-linhaX (first zonadejogo) peca)) T);;;Verifica as caracteristicas das coordenadas x através do auxilio de outra função
   (T (peca-existe-linhaX (rest zonadejogo) peca))));;; Recursividade para verificar o resto das coordenadas x

(defun peca-existe-linhaY (zonadejogo peca &optional(y 1))
  (cond
   ((null zonadejogo) nil)
   ((equal 5 y) nil)
   ((verificar-igualdade (obter-peca-linhaY zonadejogo peca y)) T);;;Verifica as caracteristicas das coordenadas y através do auxilio de outra função
   (T (peca-existe-linhaY zonadejogo peca (+ y 1)))));;; Recursividade para verificar o resto das coordenadas y

(defun obter-peca-linhaX (x pos)
  (cond
   ((null x) nil)
   ((not (listp (first x))) (cons 0 (obter-peca-linhaX (rest x) pos)));;;se o car da lista não for uma lista (não tem uma peça) pára também utiliza a recursividade de modo a construir uma lista com os valores da coordenada x, de modo a que os seus valores sejam comparados
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
     ((listp (first x)) (nth (- peca 1) (first x)));;;Se o car da lista da coordenada x fôr uma lista (tiver uma peça), retorna-se o valor da posição anterior da lista das coordenadas x
     ((zerop 0) 0)))
   (T (obter-peca-posY (rest x) peca (- y 1)))));;;Recursividade, de modo a que todos os valores de y sejam estudados e adicionados á lista, caso existam

(defun obter-peca-na-diagonal (zonadejogo peca &optional(y 1))
  (cond
   ((null zonadejogo) nil)
   (T (cons (obter-peca-posY (first zonadejogo) peca y) (obter-peca-na-diagonal (rest zonadejogo) peca (+ y 1))))));;; Constrói uma lista com os valores da diagonal principal de modo a serem comparados entre eles.


(defun obter-peca-igual-na-diagonal (zonadejogo peca &optional(y 4))
  (cond
   ((null zonadejogo) nil)
   (T (cons (obter-peca-posY (first zonadejogo) peca y) 
            (obter-peca-igual-na-diagonal (rest zonadejogo) peca (- y 1))))));;; Constrói uma lista com valores da diagonal secundária, o valor de y vai decrementando. de modo a serem comparados entre eles.

(defun verificar-igualdade (lista)
  "Verifica a igualdade de entre as peças da lista enviada"
  (cond
   ((OR (null lista) (equal 0 (first lista))) nil);;;se o membro fôr 0 ou se a lista estiver nula, pára
   ((equal (list-length lista) 1) T);;;se a lista só tiver 1 membro retorna true
   (T (cond
       ((equal (first lista) (first(rest lista))) (verificar-igualdade (rest lista)));;;verifica a igualdade entre os membros das listas
       ((zerop 0) nil)))));;;condição de paragem


(defun solucao-nao-encontrada ()
  "Função que mostra uma mensagem de solução não encontrada"
  (format T "~%")
  (format T "~@T~@T~@T~@T +=========================================+~%")
  (format T "~@T~@T~@T~@T |         SOLUÇÃO NÃO ENCONTRADA!!!       |~%")
  (format T "~@T~@T~@T~@T +=========================================+~%~%"))	

;;------------------------------------------------------------------------------------------------------
;;   FUNÇÕES DE CÁLCULO DE HEURISTICAS
;;------------------------------------------------------------------------------------------------------
(defun heuristica1 (no)
  "h(x)=4-p(x) sendo p(x) o nr de peças max ja alinhadas com a mesma caracteristica"
  (- 4 (p no)))

(defun heuristica2 (no)
  "h(x)=(4-p(x))+q(x) sendo p(x) o nr de peças max ja alinhadas com a mesma caracteristica e q(x) é o nr de peças na reserva"
  (+ (- 4 (p (first no))) (comprimento(first (rest no)))))

(defun comprimento (lista)
  "devolve o comprimento de uma lista"
  (cond
   ((null lista) 0)
   (T (1+ (comprimento (rest lista))))))

(defun p (no)
  "devolve o valor de p(x), ou seja, o nr de peças max ja alinhadas com a mesma caracteristica"
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
