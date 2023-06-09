;===========================================================================
;       FUN��ES PARA RESOLVER OS PROBLEMAS DEFINIDOS
;===========================================================================
;;;; puzzle.lisp
;;;; Disciplina de IA - 2019 / 2020
;;------------------------------------------------------------------------------------------------------
;;  VARIAVEIS GLOBAIS
;;------------------------------------------------------------------------------------------------------
;;contador para incrementar ocorrencia = 77
(defparameter *fn* (let ((count 0)) #'(lambda () (incf count))))

;;------------------------------------------------------------------------------------------------------
;;   TABULEIRO 7x7
;;------------------------------------------------------------------------------------------------------
(defun gerar-problema()
  "Fun��o que gera o problema F com pe�a na posicao x:0, y:3"
  (substituir 0 3 (gerar-tabuleiro) 'T))

(defun gerar-tabuleiro ()
  "Fun��o auxiliar para gerar e baralhar o tabuleiro com casas de 0 a 77 sem numero com 0, 8 , 9 e ainda sem pe�a colocada"
  (tabuleiro-aleatorio (baralhar (lista-sem-numero-089))))

(defun linha (x tabuleiro)
  "Fun��o que recebe um �ndice e o tabuleiro e retorna uma lista que representa esta linha"
  (cond ((or (not (numberp x)) (< x 0) (>= x (length tabuleiro))) NIL)
        (T (nth x tabuleiro))))

(defun coluna (y tabuleiro)
  "Fun��o que recebe um �ndice e o tabuleiro e retorna uma lista que representa esta coluna"
  (mapcar #'(lambda(linha)
              (cond ((or (< y 0) (>= y (length linha))) NIL)
                    (T (nth y linha)))) tabuleiro))

(defun celula (x y tabuleiro)
  "Fun��o que recebe dois �ndices e o tabuleiro e retorna o valor presente nessa c�lula do tabuleiro"
  (linha x (coluna y tabuleiro)))

;------------------------------------------------------------------------------------------------------
;;   FUN��ES AUXILIARES
;;------------------------------------------------------------------------------------------------------
(defun matriz-transposta (tabuleiro)
  "Fun��o que faz a transposta do tabuleiro"
  (apply #'mapcar #'list tabuleiro))

(defun tabuleiro-aleatorio (lista &optional(n 7))
  "Fun��o que recebe uma � numa lista e criar sublistas de n elementos recorrendo � fun��o 
   subseq que tem um comportamento semelhante ao substring para strings"
  (cond
   ((null lista) NIL)
   (T (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))))

(defun baralhar (lista)
  "Fun��o que recebe uma lista e ir� mudar aleatoriamente os seus n�meros. 
   Fa�a uma fun��o recursiva e utilize a fun��o nth, a fun��o random e a fun��o 
   remover-se em que a fun��o deve remover o n�mero da lista igual ao encontrado aleatoriamente"
  (let ((n (nth (random (length lista)) lista)))
    (cond 
     ((= (funcall *fn*) (length lista)) lista) ;;condi��o de paragem se for 99
     (T (baralhar (setf lista (cons n (remover-se #'(lambda (x) (= x n)) lista))))))))

(defun primeiro-digito (n)
  "Fun��o que recebe um numero e retorna o primeiro digito"
  (cond
   ((< n 10) n) 
   (T (primeiro-digito (floor (/ n 10))))))

(defun ultimo-digito (n)
  "Fun��o que recebe um numero e retorna o ultimo digito"
  (mod n 10))

(defun digitos-iguais (n)
  "Fun��o que recebe um numero e verifica se os 2 digitos s�o iguais"
  (let* ((x (primeiro-digito n)) 
         (y (ultimo-digito n)))
    (cond
     ((= x y) T)
     (T NIL)))) 

(defun simetrico (n)
  "Fun��o que recebe 2 numero e retorna o seu simetrico"
  (let* ((x (primeiro-digito n)) 
         (y (ultimo-digito n))
         (w (write-to-string  x)) 
         (z (write-to-string y)))
    (cond
     ((let ((str (concatenate 'string z w))) (parse-integer str)))
     (T NIL))))

(defun maior-numero-lista (tabuleiro)
  "Fun��o que retorna o numero maior numa lista"
  (reduce #'max tabuleiro))

(defun remover-se (pred lista)
  "Fun��o que recebe uma fun��o e uma lista para remover os numeros duplicados"
  (cond ((null lista) NIL) 
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (T (cons (car lista) (remover-se pred (cdr lista))))))

(defun posicao-aux (casa tabuleiro &optional (inicio 0))     
  (cond ((null tabuleiro) nil)
        ((atom (first tabuleiro))
         (if (eql casa (first tabuleiro))
             (list inicio)
           (posicao-aux casa (rest tabuleiro) (1+ inicio))))
        (T (let ((posicao (posicao-aux casa (first tabuleiro) 0)))
             (if posicao
                 (cons inicio posicao)
               (posicao-aux casa (rest tabuleiro) (1+ inicio)))))))

;;------------------------------------------------------------------------------------------------------
;;   GERAR LISTA SEM NUMEROS COM  0, 8  e 9
;;------------------------------------------------------------------------------------------------------
(defun numero-com-089 (n)
  "Fun��o que recebe um numero e verifica se existe numero com 0, 8 e 9 e retorna t ou nil"
  (let ((x (ultimo-digito n)))
    (cond
     ((or (zerop x) (= x 8) (= x 9)) T)
     (T NIL))))

(defun lista-sem-numero-089 ()
  "Fun��o que gerar e cria uma lista de numeros entre 11 at� 77 com remo��o os numeros que terminam com 0, 8 e 9"
  (loop for n from 11 to 77
      if (equal (numero-com-089 n) 'NIL)
      collect n))

;;------------------------------------------------------------------------------------------------------
;;   REGRAS DE NUMEROS-DUPLOS
;;------------------------------------------------------------------------------------------------------
(defun regra-numero-duplo (pontos tabuleiro)
  "Se uma Torre for colocado numa casa com um n�mero duplo, por exemplo 66, ent�o deve ser removido o n�mero duplo de maior valor"
  (cond 
   ((zerop pontos) tabuleiro)
   (T (let* ((lista-nova (apply #'append tabuleiro));;converter o tabuleiro 7x7 em uma �nica lista
             (lista-max (loop for n in lista-nova ;;percorrer a nova lista
                            if (and (numberp n)(equal (digitos-iguais n) 'T))
                            collect n)))
        (cond 
         ((equal lista-max 'NIL) tabuleiro)
         (T (let* ((duplo-max (maior-numero-lista lista-max))
                   (posicao-duplo-max (obter-posicao tabuleiro duplo-max)))
              (substituir (first posicao-duplo-max) (second posicao-duplo-max) tabuleiro 'NIL))))))))

;;------------------------------------------------------------------------------------------------------
;;   REGRAS DE SIMETRICOS
;;------------------------------------------------------------------------------------------------------
(defun regra-simetrico (pontos tabuleiro) 
  "Se a casa escolhida tiver um n�mero com dois d�gitos diferentes,
por exemplo 57, ent�o, em consequ�ncia, o n�mero sim�trico 75 � apagado do tabuleiro"
  (cond 
   ((zerop pontos) tabuleiro)
   (T (let* ((simetrico-pontos-destino (simetrico pontos));;numero simetricos de pontos
             (posicao-simetrico (obter-posicao tabuleiro simetrico-pontos-destino)));;obter a posicao do numero simetrico
        (cond
         ((or (equal simetrico-pontos-destino 'NIL) (equal posicao-simetrico 'NIL)) tabuleiro);; for nula, retorna o tabuleiro
         (T (substituir (first posicao-simetrico) (second posicao-simetrico) tabuleiro 'NIL)))))))

;;------------------------------------------------------------------------------------------------------
;;   CONSTRUTOR PRINCIPAL
;;------------------------------------------------------------------------------------------------------
(defun criar-no (estado pecas &optional (profundidade 0) (heuristica 0) (f 0) (pai NIL))
  "Cria uma lista que representa um no que tem um estado. Este no pode tambem ter uma profundidade, heuristica, f e um no que o gerou"
  (list estado pecas profundidade heuristica f pai))

;;------------------------------------------------------------------------------------------------------
;;   METODOS SELECTORES (GETTERS)
;;------------------------------------------------------------------------------------------------------
(defun no-estado (no)
  "Fun��o que retorna o estado de no"
  (first no))

(defun no-peca (no)
  "Fun��o que retorna o numero de pontos da pe�a de no"
  (second no))

(defun no-profundidade (no)
  "Funcao que retorna a profundidade de um no"
  (third no))

(defun no-heuristica (no)
  "Funcao que retorna a heuristica de um no"
  (fourth no))

(defun no-f (no)
  "Funcao que retorna o f de um no"
  (fifth no))

(defun no-pai (no)
  "Funcao que retorna o no pai"
  (sixth no))

(defun no-raiz (no)
  "Fun��o que retorna a n� raiz"
  (cond 
      ((null (no-pai no)) no)
   (T (no-raiz (no-pai no)))))

;;------------------------------------------------------------------------------------------------------
;;   CONSTRUTOR NOVA PE�A - METODOS SELECTORES(GETTERS) -  MODIFICADRES (SETTERS)
;;------------------------------------------------------------------------------------------------------
(defun set-pontos-peca (peca ponto)
  "Fun��o que recebe um no e valor de pontos, obtem a peca e retorna nova lista
   com pontos atualizados"
  (list (first peca) (+ (second peca) ponto)))

;;------------------------------------------------------------------------------------------------------
;;   OPERA��O SUBSTITUIR PE�A NO TABULEIRO
;;------------------------------------------------------------------------------------------------------
(defun substituir (x y tabuleiro &optional (valor 'NIL))
  "Fun��o que recebe dois �ndices, o tabuleiro e um valor (por default o valor � NIL).
   A fun��o dever� retornar o tabuleiro com a c�lula substitu�da pelo valor pretendido. 
   Utilize a fun��o substituir-posicao definida anteriormente" 
  (cond   
   ((equal (posicao-validap y x) 'NIL) NIL)
   (T (substituir-posicao-aux x tabuleiro (substituir-posicao-aux y (linha x tabuleiro) valor)))))

(defun substituir-posicao-aux (y tabuleiro &optional (valor 'NIL))
  "Fun��o que recebe um �ndice, uma lista e um valor (por default o valor � NIL) e substitui pelo valor pretendido nessa posi��o."
  (cond
   ((zerop y) (cons valor (cdr tabuleiro)))
   (T (cons (car tabuleiro) (substituir-posicao-aux (- y 1) (cdr tabuleiro) valor)))))

;;------------------------------------------------------------------------------------------------------
;;   PE�A TORRE ("T")
;;------------------------------------------------------------------------------------------------------
(defun obter-posicao (tabuleiro &optional (casa 'NIL))
  "Fun��o que recebe o elemento e tabuleiro devolvendo a posi��o (i j) 
   caso n�o se encontre no tabuleiro dever� ser retornado NIL"
  (cond
   ((null casa) (posicao-aux 'T tabuleiro));;posicao da pe�a 'T
   (T (posicao-aux casa tabuleiro))));;posicao das outras casas

(defun mover-peca (x y dx dy tabuleiro)
  (let* ((novo-tabuleiro (copy-tree tabuleiro))
         (novo-x (+ x dx))
         (novo-y (+ y dy))
         (peca (nth y (nth x tabuleiro)))
         (pontos-destino (nth novo-y (nth novo-x tabuleiro))))
    
    (when (posicao-validap novo-x novo-y)
      (setf (nth y (nth x novo-tabuleiro)) NIL
        (nth novo-y (nth novo-x novo-tabuleiro)) peca)
      (if (equal (digitos-iguais pontos-destino) 'T);;se for digitos iguais
          (regra-numero-duplo pontos-destino novo-tabuleiro);;regra duplo
        (regra-simetrico pontos-destino novo-tabuleiro)))));;se n�o, regra simetrico

;;------------------------------------------------------------------------------------------------------
;;   VALIDA��O DE CASAS E MOVIMENTOS
;;------------------------------------------------------------------------------------------------------
(defun casa-disponivel (x y tabuleiro)
  "Retorna o valor presente nessa c�lula do tabuleiro"	
  (let ((casa (celula x y tabuleiro)))
    (cond 
     ((numberp casa) T)
     (T NIL))))

;;------------------------------------------------------------------------------------------------------
;;   OPERADORES
;;------------------------------------------------------------------------------------------------------
(defun VERTICAL (x y n movimento tabuleiro) 
  "Fun��o que faz o movimento da Torre no sentido horizontal"
  (cond
   ((and (equal movimento 'CIMA) 
         (equal (posicao-validap (- x n) (+ y 0)) 'T)
         (equal (casa-disponivel (- x n) (+ y 0) tabuleiro) 'T))
    (mover-peca x y (- n) 0 tabuleiro))
   
   ((and (equal movimento 'BAIXO) 
         (equal (posicao-validap (+ x n) (+ y 0)) 'T)
         (equal (casa-disponivel (+ x n) (+ y 0) tabuleiro) 'T))
    (mover-peca x y n 0 tabuleiro))
   (T NIL)))

(defun HORIZONTAL (x y n movimento tabuleiro) 
  "Fun��o que faz o movimento da Torre no sentido vertical"
  (cond
   ((and (equal movimento 'ESQUERDA) 
         (equal (posicao-validap (+ x 0) (- y n)) 'T)
         (equal (casa-disponivel (+ x 0) (- y n) tabuleiro) 'T))
    (mover-peca x y 0 (- n) tabuleiro))
   
   ((and (equal movimento 'DIREITA) 
         (equal (posicao-validap (+ x 0) (+ y n)) 'T)
         (equal (casa-disponivel (+ x 0) (+ y n) tabuleiro) 'T))
    (mover-peca x y 0 n tabuleiro))
   (T NIL)))

(defun operadores ()
  "Funcao que lista todos os operadores existentes no dominio do problema"
  (list 'HORIZONTAL 'VERTICAL))

(defun posicao-validap (x y)
  "Fun�ao para verificar se as coordenadas s�o validas"
  (cond
   ((or (null x) (null y)) NIL)
   (T (and (>= x 0) (>= y 0) (< x 7) (< y 7)))))

(defun jogadas-possiveis (x y tabuleiro) 
  "Funcao que determina todas as jogadas possiveis para um tipo de 
  peca num determinado tabuleiro, devolvendo-as numa lista"
  (let (result)
    (dolist (mov '(CIMA BAIXO ESQUERDA DIREITA) result)
      (dotimes (n 7)
        (if (>= n 1)
            (cond 
             ((or (equal mov 'CIMA) (equal mov 'BAIXO)) 
              (when (VERTICAL x y n mov tabuleiro) ;se movimento for valido na vertical
                (push (list n mov) result)))
             (T (or (equal mov 'ESQUERDA) (equal mov 'DIREITA)) 
                (when (HORIZONTAL x y n mov tabuleiro) ;se movimento for valido na horizontal
                  (push (list n mov) result)))))))))
;;------------------------------------------------------------------------------------------------------
;;  SUCESSORES
;;------------------------------------------------------------------------------------------------------
(defun gerar-sucessores (no operadores algoritmo &optional (heuristica NIL) (profundidade NIL))
  "Funcao que devolve a lista de todos os sucessores de um determinado no passado como argumento"
  (cond
   ((equal algoritmo 'dfs)
    (cond
     ((>= (no-profundidade no) profundidade) NIL);So se aplica os sucessores caso a profundidade do no seja inferior a profundidade maxima
     (T (remover-jogadas-invalidas (apply #'append (mapcar #'(lambda (operador)
                                                               (aplicar-operador-no no operador heuristica)) operadores))))))
   (T (remover-jogadas-invalidas (apply #'append (mapcar #'(lambda (operador)
                                                             (aplicar-operador-no no operador heuristica)) operadores))))))
(defun aplicar-operador-no (no operador heuristica)
  "Funcao que aplica apenas um operador a um determinado no. Consoante o operador passado por argumento esta funcao ira 
   determinar as jogadas possiveis para esse operador e ira criar um no (um sucessor) para cada uma dessas jogadas possives" 
  (let* ((tabuleiro (first (no-estado no)))
         (problema (second (no-estado no)))
         (objetivo (third (no-estado no)))
         (jogadas-possiveis (obter-jogadas tabuleiro)))
    (cond
     ((zerop (length jogadas-possiveis)) NIL);; se n�o houver jogada, sai
     (T (mapcar #'(lambda (jogada) 
                    (let* ((posicao-peca (obter-posicao tabuleiro))
                           (posicao-destino (posicao-destino posicao-peca jogada))
                           (pontos-destino (pontos-destino posicao-destino tabuleiro))
                           (estado (list (funcall operador (first posicao-peca) (second posicao-peca) (first jogada) (second jogada) tabuleiro) problema objetivo))
                           (pecas (set-pontos-peca (no-peca no) pontos-destino))
                           (nova-heuristica (funcall heuristica (no-estado no) pecas))
                           (profundidade (1+ (no-profundidade no)))
                           (custo (+ profundidade nova-heuristica)))
                      (criar-no estado pecas profundidade nova-heuristica custo no))) jogadas-possiveis)))))

(defun obter-jogadas (tabuleiro)
  "Fun��o para obter a posicao da pe�a no tabuleiro e retorna as jogadas possiveis"
  (let* ((posicao-peca (obter-posicao tabuleiro))
         (jogadas-possiveis (jogadas-possiveis (first posicao-peca) (second posicao-peca) tabuleiro)))
    jogadas-possiveis))

(defun posicao-destino (peca jogada)
  "Fun��o que recebe uma jogada e retorna a posicao de destino"
  (cond 
   ((equal (second jogada) 'CIMA) (list (- (first peca)  (first jogada)) (+ (second peca) 0)))
   ((equal (second jogada) 'BAIXO) (list (+ (first peca) (first jogada)) (+ (second peca) 0)))
   ((equal (second jogada) 'ESQUERDA) (list (+ (first peca) 0) (- (second peca)  (first jogada))))
   ((equal (second jogada) 'DIREITA) (list (+ (first peca) 0) (+ (second peca)  (first jogada))))
   (T NIL)))

(defun pontos-destino (destino tabuleiro)
  "Fun��o que recebe uma jogada e retorna o numero de pontos da casa destino"
  (cond 
   ((null destino) 0);;jogada destino nula, 0 ponto
   (T (celula (first destino) (second destino) tabuleiro))))

(defun remover-jogadas-invalidas (sucessores)
  "Remove todas as jogadas inv�lidas da lista de sucessores"
  (cond
   ((null sucessores) ())
   ((null (first (first (first sucessores)))) (remover-jogadas-invalidas (cdr sucessores)))
   (T (cons (first sucessores) (remover-jogadas-invalidas (cdr sucessores))))))

;;------------------------------------------------------------------------------------------------------
;;   SOLU�AO, OBJECTIVO e CALCULAR PONTOS
;;------------------------------------------------------------------------------------------------------
(defun no-solucao-p (no)
  "O estado final � atingido quando a Torre chega a uma casa que lhe permite obter uma pontua��o igual
  ou superior ao objetivo definido; Se n�o for poss�vel atingir o objetivo, o programa dever� informar o 
  utilizador de que o problema n�o tem solu��o;"
  (let* ((pecas-no (no-peca no))
         (pai (no-pai no))
	     (tabuleiro (first (no-estado no)))
		 (jogadas-possiveis (obter-jogadas tabuleiro)))
    (cond  
     ((and (not (null pecas-no)) (not (null pai));;se a pe�a n�o for nula
           ;; se n�o houver jogadas
		  (zerop (length jogadas-possiveis))
           ;;numeros de pontos de no for maior que ou igual ao objetivo
           (>= (second pecas-no) (third (first pai)))) T)
     (T NIL))))

(defun solucao-nao-encontrada () ;fun��o que mostra uma mensagem de solu��o n�o encontrada
  (format T "~%")
  (format T "~@T~@T~@T~@T +=========================================+~%")
  (format T "~@T~@T~@T~@T |         SOLU��O N�O ENCONTRADA!!!       |~%")
  (format T "~@T~@T~@T~@T +=========================================+~%~%"))	

;;------------------------------------------------------------------------------------------------------
;;   FUN��ES DE C�LCULO DE HEURISTICAS
;;------------------------------------------------------------------------------------------------------
(defun pontos-atingir-objetivo (objetivo peca)
  "Fun��o que retorna o n�mero de pontos que faltam para atingir o valor definido como objetivo"
  (let ((pontos (- objetivo (second peca)))) pontos))

(defun pontos-medio-casa-tabuleiro (tabuleiro)
  "Fun��o que calcula a m�dia por casa dos pontos que constam no tabuleiro"
  (cond
   ((null tabuleiro) 0);;n�o existe tabuleiro
   ((zerop (length (obter-jogadas tabuleiro))) 0);;n�o existe jogada
   (T (let* ((lista (apply #'append tabuleiro));;converter o tabuleiro 7x7 em uma �nica lista
             (nova-lista (loop for n in lista ;;percorrer a lista
                             if (numberp n) collect n));;colecionar apenas casas que tem numero de pontos
             (total-casas (length nova-lista));;calcula o numero total de casas da nova lista
             (soma (apply #'+ nova-lista));;faz a soma dos elementos na lista
             (media (round (/ soma total-casas))));;calcula a media arredondada
        media))));;retorna a media

(defun sem-heuristica (no &optional(peca NIL))
  "Fun��o com uma heur�stica sem significado para uso nas pesquisas cegas"
  (cond 
   ((or (null no) (null peca)) 0)
   (T 0)))

(defun heuristica1 (no peca)
  "Calcula a heuristica h(x) = o(x)/m(x) em que
  o(x) � o n�mero de pontos que faltam para atingir o valor definido como objetivo
  m(x) � a m�dia por casa dos pontos que constam no tabuleiro x"
  (let* ((tabuleiro (no-estado no))
         (objetivo (third no))
         (pontos-atingir (pontos-atingir-objetivo objetivo peca))
         (pontos-medio (pontos-medio-casa-tabuleiro tabuleiro))
         (heuristica (round (/ pontos-atingir pontos-medio)))) heuristica))

(defun heuristica2 (no peca)
  "Calcula a heuristica2 para trabalhar com numeros de jogadas possiveis da pe�a no tabuleiro"
  (cond 
   ((or (null no) (null peca)) 0)
   (T (let* ((tabuleiro (no-estado no))
             (jogadas (obter-jogadas tabuleiro))
             (heuristica (length jogadas))) heuristica))))

