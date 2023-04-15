;===========================================================================
;       FUNÇÕES PARA RESOLVER OS PROBLEMAS DEFINIDOS
;===========================================================================
;;;; puzzle.lisp
;;;; Disciplina de IA - 2019 / 2020
;;;; Autor:
;;------------------------------------------------------------------------------------------------------
;;  VARIAVEIS GLOBAIS
;;------------------------------------------------------------------------------------------------------
;;contador para incrementar ocorrencia = 77
(defparameter *fn* (let ((count 0)) #'(lambda () (incf count))))

;;------------------------------------------------------------------------------------------------------
;;   TABULEIRO 7x7
;;------------------------------------------------------------------------------------------------------
(defun tabuleiro-inicial ()
  "Função auxiliar para gerar e baralhar o tabuleiro com casas de 0 a 77
 sem numero com 0, 8 , 9 e ainda sem peça colocada"
  (let* ((tabuleiro  (tabuleiro-aleatorio (baralhar (lista-sem-numero-089))))
         ;;inserir peça jogador 1 na posicao A1 - G1
         (tabuleiro-aux (substituir 0 0 tabuleiro -1))
         ;;inserir peça jogador 2 na posicao A7 - G7
         (tabuleiro-final (substituir 6 6 tabuleiro-aux -2))) tabuleiro-final))

(defun linha (x tabuleiro)
  "Função que recebe um índice e o tabuleiro e retorna uma lista que representa esta linha"
  (cond ((or (not (numberp x)) (< x 0) (>= x (length tabuleiro))) NIL)
        (T (nth x tabuleiro))))

(defun coluna (y tabuleiro)
  "Função que recebe um índice e o tabuleiro e retorna uma lista que representa esta coluna"
  (mapcar #'(lambda(linha)
              (cond ((or (< y 0) (>= y (length linha))) NIL)
                    (T (nth y linha)))) tabuleiro))

(defun celula (x y tabuleiro)
  "Função que recebe dois índices e o tabuleiro e retorna o valor presente nessa célula do tabuleiro"
  (linha x (coluna y tabuleiro)))

;------------------------------------------------------------------------------------------------------
;;   FUNÇÕES AUXILIARES
;;------------------------------------------------------------------------------------------------------
(defun matriz-transposta (tabuleiro)
  "Função que faz a transposta do tabuleiro"
  (apply #'mapcar #'list tabuleiro))

(defun tabuleiro-aleatorio (lista &optional(n 7))
  "Função que recebe uma é numa lista e criar sublistas de n elementos recorrendo à função 
   subseq que tem um comportamento semelhante ao substring para strings"
  (cond
   ((null lista) NIL)
   (T (cons (subseq lista 0 n) (tabuleiro-aleatorio (subseq lista n) n)))))

(defun baralhar (lista)
  "Função que recebe uma lista e irá mudar aleatoriamente os seus números. 
   Faça uma função recursiva e utilize a função nth, a função random e a função 
   remover-se em que a função deve remover o número da lista igual ao encontrado aleatoriamente"
  (let ((n (nth (random (length lista)) lista)))
    (cond 
     ((= (funcall *fn*) (length lista)) lista) ;;condição de paragem se for 77
     (T (baralhar (setf lista (cons n (remover-se #'(lambda (x) (= x n)) lista))))))))

(defun primeiro-digito (n)
  "Função que recebe um numero e retorna o primeiro digito"
  (cond
   ((< n 10) n) 
   (T (primeiro-digito (floor (/ n 10))))))

(defun ultimo-digito (n)
  "Função que recebe um numero e retorna o ultimo digito"
  (mod n 10))

(defun digitos-iguais (n)
  "Função que recebe um numero e verifica se os 2 digitos são iguais"
  (let* ((x (primeiro-digito n)) 
         (y (ultimo-digito n)))
    (cond
     ((= x y) T)
     (T NIL)))) 

(defun simetrico (n)
  "Função que recebe 2 numero e retorna o seu simetrico"
  (let* ((x (primeiro-digito n)) 
         (y (ultimo-digito n))
         (w (write-to-string  x)) 
         (z (write-to-string y)))
    (cond
     ((let ((str (concatenate 'string z w))) 
        (parse-integer str)))
     (T NIL))))

(defun maior-numero-lista (tabuleiro)
  "Função que retorna o numero maior numa lista"
  (reduce #'max tabuleiro))

(defun remover-se (pred lista)
  "Função que recebe uma função e uma lista para remover os numeros duplicados"
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
  "Função que recebe um numero e verifica se existe numero com 0, 8 e 9 e retorna t ou nil"
  (let ((x (ultimo-digito n)))
    (cond
     ((or (zerop x) (= x 8) (= x 9)) T)
     (T NIL))))

(defun lista-sem-numero-089 ()
  "Função que gerar e cria uma lista de numeros entre 11 até 77 com remoção os numeros que terminam com 0, 8 e 9"
  (loop for n from 11 to 77
      if (equal (numero-com-089 n) 'NIL)
      collect n))

;;------------------------------------------------------------------------------------------------------
;;   REGRAS DE NUMEROS-DUPLOS
;;------------------------------------------------------------------------------------------------------
(defun regra-numero-duplo (pontos tabuleiro)
  "Se uma Torre for colocado numa casa com um número duplo, por exemplo 66, então deve ser removido o número duplo de maior valor"
  (cond 
   ((zerop pontos) tabuleiro)
   (T (let* ((lista-nova (apply #'append tabuleiro));;converter o tabuleiro 7x7 em uma única lista
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
  "Se a casa escolhida tiver um número com dois dígitos diferentes,
por exemplo 57, então, em consequência, o número simétrico 75 é apagado do tabuleiro"
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
(defun criar-no (estado peca-jogador1 peca-jogador2 tipo-jogador &optional (profundidade 0) (tipo-no 'MAX) (no-pai NIL))
  "Cria uma lista que representa um no que tem um estado."
  (list estado peca-jogador1 peca-jogador2 tipo-jogador profundidade tipo-no no-pai))

;;------------------------------------------------------------------------------------------------------
;;   METODOS SELECTORES (GETTERS)
;;------------------------------------------------------------------------------------------------------
(defun get-no-estado (no)
  "Função que retorna o estado de no"
  (first no))

(defun get-peca-jogador1 (no)
  "Função que retorna o numero de pontos da peça do jogador"
  (second no))

(defun get-peca-jogador2 (no)
  "Função que retorna o numero de pontos da peça do jogador 2"
  (third no))

(defun get-tipo-jogador (no)
  "Função que retorna o tipo de jogador"
  (fourth no))

(defun get-no-profundidade (no)
  "Funcao que retorna a profundidade de um no"
  (fifth no))

(defun get-tipo-no (no)
  "Funcao que retorna a heuristica de um no"
  (sixth no))

(defun get-no-pai (no)
  "Funcao que retorna o no pai"
  (seventh no))

(defun get-no-raiz (no)
  "Função que retorna a nó raiz"
  (cond 
   ((null (get-no-pai no)) no)
   (T (get-no-raiz (get-no-pai no)))))

;;------------------------------------------------------------------------------------------------------
;;   CONSTRUTOR NOVA PEÇA - METODOS SELECTORES(GETTERS) -  MODIFICADRES (SETTERS)
;;------------------------------------------------------------------------------------------------------
(defun set-pontos-jogador1 (no pontos jogador)
  "Função que recebe um no e valor de pontos, obtem a peca e retorna nova lista com pontos atualizados"
  (let ((peca-jogador1 (get-peca-jogador1 no)))
    (if (and (= jogador 1) (not (null (first peca-jogador1))) (not (null (second peca-jogador1))) (not (null pontos)))
        (list (first peca-jogador1) (+ (second peca-jogador1) pontos))
      peca-jogador1)))

(defun set-pontos-jogador2 (no pontos jogador)
  "Função que recebe um no e valor de pontos, obtem a peca e retorna nova lista com pontos atualizados"
  (let ((peca-jogador2 (get-peca-jogador2 no)))
     (if (and (= jogador 2) (not (null (first peca-jogador2))) (not (null (second peca-jogador2))) (not (null pontos))) 
        (list (first peca-jogador2) (+ (second peca-jogador2) pontos))
      peca-jogador2)))

(defun trocar-jogador (no)
  "Função que recebe um no e troca o jogador"
  (cond
   ((= (get-tipo-jogador no) 1) 2)
   (T 1)))

(defun trocar-no-tipo (no)
  "Função que recebe um no e troca o jogador"
  (cond
   ((equal (get-tipo-no no) 'MAX) 'MIN)
   (T 'MAX)))

;;------------------------------------------------------------------------------------------------------
;;   OPERAÇÃO SUBSTITUIR PEÇA NO TABULEIRO
;;------------------------------------------------------------------------------------------------------
(defun substituir (x y tabuleiro &optional (valor 'NIL))
  "Função que recebe dois índices, o tabuleiro e um valor (por default o valor é NIL).
   A função deverá retornar o tabuleiro com a célula substituída pelo valor pretendido. 
   Utilize a função substituir-posicao definida anteriormente" 
  (cond   
   ((equal (posicao-validap y x) 'NIL) NIL)
   (T (substituir-posicao-aux x tabuleiro (substituir-posicao-aux y (linha x tabuleiro) valor)))))

(defun substituir-posicao-aux (y tabuleiro &optional (valor 'NIL))
  "Função que recebe um índice, uma lista e um valor (por default o valor é NIL) e substitui pelo valor pretendido nessa posição."
  (cond
   ((zerop y) (cons valor (cdr tabuleiro)))
   (T (cons (car tabuleiro) (substituir-posicao-aux (- y 1) (cdr tabuleiro) valor)))))

;;------------------------------------------------------------------------------------------------------
;;   PEÇA
;;------------------------------------------------------------------------------------------------------
(defun obter-posicao (tabuleiro jogador)
  "Função que recebe o elemento e tabuleiro devolvendo a posição (i j) 
   caso não se encontre no tabuleiro deverá ser retornado NIL"
  (cond 
   ((= jogador 1) (posicao-aux -1 tabuleiro));;peca -1
   ((= jogador 2) (posicao-aux -2 tabuleiro));; peça -2
   (T (posicao-aux jogador tabuleiro))))

(defun mover-peca (x y dx dy tabuleiro)
  (let* ((novo-tabuleiro (copy-tree tabuleiro))
         (novo-x (+ x dx))
         (novo-y (+ y dy))
         (peca (nth y (nth x tabuleiro)))
         (pontos-destino (nth novo-y (nth novo-x novo-tabuleiro))))
    
    (when (posicao-validap novo-x novo-y)
      (setf (nth y (nth x novo-tabuleiro)) NIL
        (nth novo-y (nth novo-x novo-tabuleiro)) peca)
      (if (equal (digitos-iguais pontos-destino) 'T);;se for digitos iguais
          (regra-numero-duplo pontos-destino novo-tabuleiro);;regra duplo
        (regra-simetrico pontos-destino novo-tabuleiro)))));;se não, regra simetrico

;;------------------------------------------------------------------------------------------------------
;;   VALIDAÇÃO DE CASAS E MOVIMENTOS
;;------------------------------------------------------------------------------------------------------
(defun casa-disponivel (x y tabuleiro)
  "Retorna o valor presente nessa célula do tabuleiro"	
  (let ((casa (celula x y tabuleiro)))
    (cond 
     ((and (numberp casa) (/= casa -1) (/= casa -2)) T)
     (T NIL))))

;;------------------------------------------------------------------------------------------------------
;;   OPERADORES
;;------------------------------------------------------------------------------------------------------
(defun VERTICAL (x y n movimento tabuleiro) 
  "Função que faz o movimento da Torre no sentido horizontal"
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
  "Função que faz o movimento da Torre no sentido vertical"
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
  "Funçao para verificar se as coordenadas são validas"
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

(defun obter-operadores (jogada)
 "Função que retorna uma jogada e retorna o seu operador"
  (cond 
      ((or (equal (second jogada) 'CIMA) (equal (second jogada) 'BAIXO)) 'VERTICAL)
      (T (or (equal (second jogada) 'ESQUERDA) (equal (second jogada) 'DIREITA)) 'HORIZONTAL)))

;;------------------------------------------------------------------------------------------------------
;;  SUCESSORES
;;------------------------------------------------------------------------------------------------------
(defun gerar-sucessores (no operadores)
  "Funcao que devolve a lista de todos os sucessores de um determinado no passado como argumento"
  (apply #'append (mapcar #'(lambda (operador) (aplicar-operador-no no operador)) operadores)))

(defun aplicar-operador-no (no jogada)
 "Função que testa jogada para criação de novo nó"
  (let* ((tabuleiro (get-no-estado no))
         (posicao-peca (obter-posicao tabuleiro (get-tipo-jogador no)))
         (posicao-destino (posicao-destino posicao-peca jogada))
         (pontos-destino (pontos-destino posicao-destino tabuleiro))
         (estado (funcall (obter-operadores jogada) (first posicao-peca) (second posicao-peca) (first jogada) (second jogada) tabuleiro))
         (peca-jogador1 (set-pontos-jogador1 no pontos-destino (get-tipo-jogador no)))
         (peca-jogador2 (set-pontos-jogador2 no pontos-destino (get-tipo-jogador no)))
         (profundidade (1+ (get-no-profundidade no))))
    (criar-no estado peca-jogador1 peca-jogador2 (trocar-jogador no) profundidade (trocar-no-tipo no) no)))

(defun obter-jogadas (tabuleiro jogador)
  "Função para obter a posicao da peça no tabuleiro e retorna as jogadas possiveis"
  (let* ((posicao-peca-jogador (obter-posicao tabuleiro jogador))
         (jogadas-possiveis (jogadas-possiveis (first posicao-peca-jogador) (second posicao-peca-jogador) tabuleiro)))         
    jogadas-possiveis))

(defun posicao-destino (jogador jogada)
  "Função que recebe uma jogada e retorna a posicao de destino"
  (cond 
   ((equal (second jogada) 'CIMA) (list (- (first jogador)  (first jogada)) (+ (second jogador) 0)))
   ((equal (second jogada) 'BAIXO) (list (+ (first jogador) (first jogada)) (+ (second jogador) 0)))
   ((equal (second jogada) 'ESQUERDA) (list (+ (first jogador) 0) (- (second jogador)  (first jogada))))
   ((equal (second jogada) 'DIREITA) (list (+ (first jogador) 0) (+ (second jogador)  (first jogada))))
   (T NIL)))

(defun calcular-posicao-destino (no jogador)
  (let* ((tabuleiro (get-no-estado no))
         (jogadas-possiveis (obter-jogadas tabuleiro jogador)))
    (cond
     ((zerop (length jogadas-possiveis)) NIL)
     (T (mapcar #'(lambda (jogada) 
                    (let* ((posicao-peca (obter-posicao tabuleiro jogador))
                           (posicao-destino (posicao-destino posicao-peca jogada))) posicao-destino)) jogadas-possiveis)))))

(defun pontos-destino (destino tabuleiro)
  "Função que recebe uma jogada e retorna o numero de pontos da casa destino"
  (cond 
   ((null destino) 0);;jogada destino nula, 0 ponto
   (T (celula (first destino) (second destino) tabuleiro))))

(defun remover-jogadas-invalidas (sucessores)
  "Remove todas as jogadas inválidas da lista de sucessores"
  (cond
   ((null sucessores) ())
   ((null (first (first (first sucessores)))) (remover-jogadas-invalidas (cdr sucessores)))
   (T (cons (first sucessores) (remover-jogadas-invalidas (cdr sucessores))))))

;;------------------------------------------------------------------------------------------------------
;;   SOLUÇAO, OBJECTIVO e CALCULAR PONTOS
;;------------------------------------------------------------------------------------------------------

(defun solucao-nao-encontrada () ;função que mostra uma mensagem de solução não encontrada
  (format T "~%")
  (format T "~@T~@T~@T~@T +=========================================+~%")
  (format T "~@T~@T~@T~@T |         SOLUÇÃO NÃO ENCONTRADA!!!       |~%")
  (format T "~@T~@T~@T~@T +=========================================+~%~%"))	
