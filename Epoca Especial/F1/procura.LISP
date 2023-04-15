;===========================================================================
;       FUNÇÕES PARA PROCURA NO ESPAÇO
;===========================================================================
;;;; procura.lisp
;;;; Disciplina de IA - 2019 / 2020
;===========================================================================
;       FUNÇÃO DE PROCURA EM LARGURA - BFS
;===========================================================================
(defun bfs (no-inicial algoritmo funcao-solucao funcao-sucessores operadores heuristica &optional (abertos (list no-inicial)) (fechados nil) (tempo-inicial (get-universal-time)) (nos-expandidos 0)(nos-gerados 0))
  "Funcao que implementa o algoritmo de procura em largura"
  (cond
   ((null abertos) (solucao-nao-encontrada)) ;Se a lista de abertos estiver vazia, falha, soluçao nao encontrada
   (T (let* ((no-atual (first abertos))
             (abertos-sem-no-inicial (rest abertos))
             (fechados-com-no-inicial (cons no-atual fechados))
             (sucessores (remover-nos-repetidos (funcall funcao-sucessores no-atual operadores 'bfs heuristica) abertos-sem-no-inicial fechados-com-no-inicial))
             (abertos-com-sucessores (abertos-bfs abertos-sem-no-inicial sucessores)))
            
     
        (cond
         ;Se existir uma solucao nos sucessores, vamos a procura dessa solucao
         ((existe-solucaop sucessores funcao-solucao) 
          (list (procura-no-objetivo sucessores funcao-solucao) algoritmo tempo-inicial (1+ nos-expandidos) (+ nos-gerados (length sucessores))))
         (T (bfs (first abertos-com-sucessores) algoritmo funcao-solucao funcao-sucessores operadores heuristica abertos-com-sucessores fechados-com-no-inicial tempo-inicial (1+ nos-expandidos) (+ nos-gerados (length sucessores)))))))))

;===========================================================================
;       FUNÇÃO DE PROCURA EM PROFUNDIDADE - DFS
;===========================================================================
(defun dfs (no-inicial algoritmo funcao-solucao funcao-sucessores operadores profundidade heuristica &optional (abertos (list no-inicial)) (fechados nil) (tempo-inicial (get-universal-time)) (nos-expandidos 0) (nos-gerados 0))
  "Funcao que implementa o algoritmo de procura em profundidade"
  (cond
   ((null abertos) (solucao-nao-encontrada));Se abertos vazia falha, soluçao nao encontrada
   (T (let* ((no-atual (first abertos))
             (abertos-sem-no-inicial (rest abertos))
             (fechados-com-no-inicial (cons no-atual fechados)))       
        (cond
         ;Se a profundidade do no atual ultrapassar a profundidade maxima passa-se para o no seguinte
         ((> (no-profundidade no-atual) profundidade) 
          (dfs (first abertos-sem-no-inicial) funcao-solucao funcao-sucessores operadores profundidade heuristica abertos-sem-no-inicial fechados-com-no-inicial tempo-inicial nos-expandidos nos-gerados))
         (T (let* ((sucessores (remover-nos-repetidos (funcall funcao-sucessores no-atual operadores 'dfs heuristica profundidade) abertos-sem-no-inicial fechados-com-no-inicial))
                   (abertos-com-sucessores (abertos-dfs abertos-sem-no-inicial sucessores)))
              (cond
               ;Se existir uma solucao nos sucessores, vamos a procura dessa solucao
               ((existe-solucaop sucessores funcao-solucao) 
                (list (procura-no-objetivo sucessores funcao-solucao) algoritmo tempo-inicial (1+ nos-expandidos) (+ nos-gerados (length sucessores))))
               (T (dfs (first abertos-com-sucessores) algoritmo funcao-solucao funcao-sucessores operadores profundidade heuristica abertos-com-sucessores fechados-com-no-inicial tempo-inicial (1+ nos-expandidos) (+ nos-gerados (length sucessores))))))))))))

;===========================================================================
;       FUNÇÃO DE PROCURA A*
;===========================================================================
(defun A* (no-inicial algoritmo funcao-solucao funcao-sucessores operadores heuristica &optional (abertos (list no-inicial)) (fechados nil) (tempo-inicial (get-universal-time)) (nos-expandidos 0) (nos-gerados 0))
  "Funcao que implementa o algoritmo de procura A*"
  (cond
   ((null abertos) (solucao-nao-encontrada));Se abertos vazia falha, soluçao nao encontrada
   (T (let* ((no-atual (first abertos)) 
             (abertos-sem-no-inicial (rest abertos))  ;Retira o primeiro no de abertos, o que possui o menor f
             (fechados-com-no-inicial (cons no-atual fechados))) ;Insere o primeiro no de abertos em fechados
        (cond
         ;Verifica se o no atual e um no objetivo, se for devolve a solucao
         ((funcall funcao-solucao no-atual) (list no-atual algoritmo tempo-inicial nos-expandidos nos-gerados))
         (T (let* ((sucessores (funcall funcao-sucessores no-atual operadores 'A* heuristica))  ;Expande o no
                   (fechados-atualizados (remover-nos-com-menor-f-fechados sucessores fechados-com-no-inicial)) ;Primeiro remove todos os nos de fechados que possuam um f maior do que um mesmo no existente em sucessores
                   (abertos-com-sucessores (abertos-A* abertos-sem-no-inicial sucessores fechados-atualizados))) ;Insere todos os sucessores nao existentes em abertos nem fechados na lista de abertos, atualiza os nos de abertos que tenham f's maiores do que os nos dos sucessores
              (A* (first abertos-com-sucessores) algoritmo funcao-solucao funcao-sucessores operadores heuristica abertos-com-sucessores fechados-atualizados tempo-inicial (1+ nos-expandidos) (+ nos-gerados (length sucessores))))))))))

;===========================================================================
;       FUNÇÃO DE PROCURA IDA*
;===========================================================================
(defun IDA* (no-inicial algoritmo funcao-solucao funcao-sucessores operadores heuristica &optional (abertos (list no-inicial)) (fechados nil) (limiar (no-f no-inicial)) (tempo-inicial (get-universal-time)) (nos-expandidos 0) (nos-gerados 0) (no-inicial-original no-inicial))
  "Funcao que imlementa o algoritmo IDA*"
  (cond
   ((null abertos) (solucao-nao-encontrada));Se abertos vazia falha, soluçao nao encontrada
   (T (let* ((no-atual (first abertos))
             (abertos-sem-no-inicial (rest abertos))
             (fechados-com-no-inicial (cons no-atual fechados)))
        (cond
         ((> (no-f no-atual) limiar)  ;Caso o no atual possua um f maior do que o limiar
          (cond
           ;Se existir um em abertos cujo seu f seja menor que o limiar, passa a ser esse o no atual
           ((existe-no-menor-limiar-p abertos-sem-no-inicial limiar) 
            (let* ((novo-no-atual (procura-no-menor-limiar abertos-sem-no-inicial limiar))
                   (sucessores (funcall funcao-sucessores novo-no-atual operadores 'IDA* heuristica))
                   (fechados-atualizados (remover-nos-com-menor-f-fechados sucessores fechados-com-no-inicial))
                   (abertos-com-sucessores (abertos-A* abertos-sem-no-inicial sucessores fechados-atualizados)))
              (IDA* (first abertos-com-sucessores) algoritmo funcao-solucao funcao-sucessores operadores heuristica abertos-com-sucessores fechados-atualizados limiar tempo-inicial (1+ nos-expandidos) (+ nos-gerados (length sucessores)) no-inicial-original)))
           ;Se nao existir nenhum no em abertos cujo f seja menor que o limiar, entao muda-se o limiar para o minimo dos f's em abertos e comeca-se tudo outra vez
           (T (IDA* no-inicial-original algoritmo funcao-solucao funcao-sucessores operadores heuristica (list no-inicial-original) nil (novo-limiar abertos) tempo-inicial nos-expandidos nos-gerados no-inicial-original))))
         ;se o f do no-atual for menor ou igual que o limiar e no-atual for um no objetivo entao devolve-se o no atual
         ((funcall funcao-solucao no-atual) (list no-atual algoritmo tempo-inicial nos-expandidos nos-gerados)) 
         (T (let* ((sucessores (funcall funcao-sucessores no-atual operadores 'IDA* heuristica))
                   (fechados-atualizados (remover-nos-com-menor-f-fechados sucessores fechados-com-no-inicial))
                   (abertos-com-sucessores (abertos-A* abertos-sem-no-inicial sucessores fechados-atualizados)))
              (IDA* (first abertos-com-sucessores) algoritmo funcao-solucao funcao-sucessores operadores heuristica abertos-com-sucessores fechados-atualizados limiar tempo-inicial (1+ nos-expandidos)(+ nos-gerados (length sucessores)) no-inicial-original))))))))

;===========================================================================
;       FUNÇÕES AUXILIARES
;===========================================================================
(defun procura-no-objetivo (sucessores funcao-solucao)
  "Funcao que procura o primeiro no objetivo de uma lista de sucessores"
  (cond
   ((null sucessores) nil)
   ((funcall funcao-solucao (first sucessores)) (first sucessores))
   (T (procura-no-objetivo (rest sucessores) funcao-solucao))))

(defun remover-nos-repetidos (sucessores abertos fechados)
  "Funcao que retira sucessores que ja existam em abertos ou fechados"
  (apply #'append (mapcar #'(lambda (no)
                              (cond
                               ((or (existep no abertos) (existep no fechados)) nil)
                               (T (list no)))) sucessores)))

(defun existep (no lista)
  "Funcao que verifica se um determinado no ja existe numa lista ou nao"
  (cond
   ((null lista) NIL)
   ((and 
     ;Se o estado do no passado por argumento for igual ao estado do no que estamos a iterar neste momento
     (equal (no-estado no) (no-estado (first lista)))
     ;Se as pecas do no passado por argumento forem iguais as pecas do no que estamos a iterar neste momento
     (equal (no-peca no) (no-peca (first lista)))) T) 
   (T (existep no (rest lista)))))

(defun existe-solucaop (sucessores funcao-solucao)
  "Funcao que verifica se existe algum no objetivo numa determinada lista de sucessores"
  (eval (cons 'or (mapcar funcao-solucao sucessores))))

(defun abertos-bfs (abertos sucessores)
  "Funcao que constroi a lista de ABERTOS segundo o algoritmo bfs"
  (append abertos sucessores))

(defun abertos-dfs (abertos sucessores)
  "Funcao que constroi a lista de ABERTOS segundo o algoritmo dfs"
  (append sucessores abertos))

(defun abertos-A* (abertos sucessores fechados)
  "Funcao que constroi a lista de ABERTOS segundo o algoritmo A*. 
   Esta funcao ira percorrer toda a lista de sucessores a procura de sucessores que nao existam na lista de ABERTOS
   ou que ja existam mas que tenham um f menor"
  (cond
   ((null sucessores) (sort abertos #'< :key #'no-f)) ;Ordena a lista de abertos por ordem crescente de f's
   ((and (not (existep (first sucessores) abertos))(not (existep (first sucessores) fechados)))
    ;se o no nao existem em abertos nem fechados, insere-o
    (abertos-A* (append abertos (list (first sucessores))) (rest sucessores) fechados))  
   (T (abertos-A* (substituir-abertos-a* (first sucessores) abertos) (rest sucessores) fechados)))) ;Caso contrario, ja existe esse no e pode ou nao ser atualizado

(defun substituir-abertos-a* (no abertos)
  "Funcao que recebe como argumento um no proveniente da lista de sucessores. 
   Com esse no esta funcao vai percorrer a lista de ABERTOS a procura de um no 
   igual ao passado por argumento, se encontrar e se o no passado por argumento 
   possuir um f menor entao substitui-se o que esta na lista de ABERTOS pelo no passado por argumento"
  (cond
   ((null abertos) nil)
   ((and (equal (no-estado no) (no-estado (first abertos))) ;Se o no-atual for igual ao no passado
         (equal (no-peca no) (no-peca (first abertos))));Se o no-atual for igual ao no passado
    (cond
     ;Se o no passado tiver um no inferior ao que ja existe em abertos, substituir o que ja existe em abertos pelo no passado
     ((< (no-f no) (no-f (first abertos))) (cons no (rest abertos))) 
     (T abertos))) ;Caso contrario manter o no ja existente
   (T (cons (first abertos) (substituir-abertos-a* no (rest abertos))))))

(defun remover-nos-com-menor-f-fechados (sucessores fechados)
  "Funcao que percorre a lista de sucessores enquanto verifica se existe algum no 
   identico na lista de FECHADOS, caso existe e caso o f do no-sucessor seja menor 
   do que o no em FECHADOS entao retira-se o no de FECHADOS"
  (cond
   ((null sucessores) fechados)
   ;Se o no nao existe na lista de fechados entao passar ao proximo no
   ((not (existep (first sucessores) fechados)) (remover-nos-com-menor-f-fechados (rest sucessores) fechados)) 
   ;Se o no existe na lista de fechados entao pode ser removido caso o no dos sucessores tenha um f menor
   (T (remover-nos-com-menor-f-fechados (rest sucessores) (atualizar-fechados-a* (first sucessores) fechados)))))

(defun atualizar-fechados-a* (no fechados)
  "Funcao que percorre a lista de FECHADOS verificando para cada no se o seu f 
   e superiror ao f do no passado por argumento, caso seja retira-se o no da lista de FECHADOS"
  (apply #'append (mapcar #'(lambda (no-fechado)
                              (cond
                               ((and
                                 ;Se o no-atual for igual ao no passado
                                 (equal (no-estado no) (no-estado no-fechado)) 
                                 ;Se o no-atual for igual ao no passado
                                 (equal (no-peca no) (no-peca no-fechado)))
                                (cond
                                 ;Se f do no existente em sucessores for menor do que o existente em fechados,
                                 ;entao retira-se o no existente em fechados (para por o no existente nos sucessores em abertos)
                                 ((< (no-f no) (no-f no-fechado)) nil) 
                                 ;Caso contrario mantem-se o no que ja tinhamos em fechados
                                 (T (list no-fechado)) ))
                               (T (list no-fechado)))) fechados)))

(defun existe-no-menor-limiar-p (abertos limiar)
  "Funcao que verifica se existe algum no na lista de abertos cujo o seu f 
   seja menor que o atual limiar utilizado pelo algoritmo ida*"
  (eval (cons 'or (mapcar #'(lambda (no)
                              (cond
                               ((<= (no-f no) limiar) T)
                               (T nil))) abertos))))

(defun procura-no-menor-limiar (abertos limiar)
  "Funcao que obtem o primeiro no, uma vez que a lista de abertos esta ordenada 
   por ordem crescente de f's, cujo seu f seja menor que o atual limiar utilizado pelo algoritmo ida*"
  (cond
   ((null abertos) nil)
   ((<= (no-f (first abertos)) limiar) (first abertos))
   (T (procura-no-menor-limiar (rest abertos) limiar))))

(defun novo-limiar (abertos)
  "Funcao que ira obter o novo limiar a ser utilizado para o algoritmo ida* 
   sendo este o menor dos valores de f dos nos presentes em abertos"
  (apply #'min (mapcar #'no-f abertos)))

(defun remover-no (no lista)
  "Funcao que remove um no de uma determinada lista"
  (cond
   ((null lista) nil)
   ((equal no (first lista)) (remover-no no (rest lista)) )
   (T (cons (first lista) (remover-no no (rest lista))))))

;===========================================================================
;       FUNÇÕES PARA AVALIAÇÃO DE EFICIENCIA
;===========================================================================
(defun fator-ramificacao(L nos-gerados &optional (limite-inferior 1) (limite-superior most-positive-fixnum) (margem-erro 0.00001) (limite-medio (/ (+ limite-inferior limite-superior) 2)))
  "Funcao que permite calcular o fator de ramificacao para um no. Aplica o metodo da bissecao para o calculo desta funcao com uma margem de erro de 0.00001"
  (cond
   ;Se a diferenca entre os limites for inferior a margem de erro entao podemos assumir com precisao que o fator de ramificacao encontra-se entre estes dois limites
   ((< (- limite-superior limite-inferior) margem-erro) (float limite-medio)) 
   ;Caso o valor esteja mais perto do limite-inferior, diminui-se o limite superior
   ((> (- (f-polinomio L limite-medio) nos-gerados) margem-erro) (fator-ramificacao L nos-gerados limite-inferior limite-medio margem-erro))  
   ;Caso o valor esteja mais perto do limite-superior, aumenta-se o limite inferior
   (T (float (fator-ramificacao L nos-gerados limite-medio limite-superior margem-erro))))) 

(defun f-polinomio (grau polinomio)
  "Funcao que implementa o calculo de uma funcao polinomial"
  (cond
   ((= grau 1) polinomio)
   (T (+ (expt polinomio grau) (f-polinomio (1- grau) polinomio)))))

(defun penetrancia (L nos-gerados)
  "Funcao que permite calcular a penetrancia de um algoritmo, 
   passando como argumentos a profundidade da solucao encontrada 
   o numero total de nos gerados"
  (cond
   ((not (zerop L)) (float (/ L nos-gerados)))
   (T 0)))



