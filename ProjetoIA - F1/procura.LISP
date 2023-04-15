;===========================================================================
;       FUNÇÕES PARA PROCURA NO ESPAÇO
;===========================================================================
;;;; procura.lisp
;;;; Disciplina de IA - 2020 / 2021
;===========================================================================
;       FUNÇÃO DE PROCURA EM LARGURA - BFS
;===========================================================================
(defun bfs (abertos &optional(fechados nil) (tempo-inicial (get-universal-time)))
  "Algoritmo de procura em largura primeiro"
  (cond
   ((null abertos) (solucao-nao-encontrada)) ;Se a lista de abertos estiver vazia, falha, soluçao nao encontrada
   ((solucaop (get-zona-de-jogo (first abertos)))
    (list (first (first abertos)) (contar-nos abertos fechados) tempo-inicial))
   (T (bfs (abertos-bfs (rest abertos) (gerar-sucessores (first abertos))) 
           (cons (first abertos) fechados)))))

(defun abertos-bfs (abertos sucessores)
  "Funcao que adiciona os sucessores do nó expandido à lista dos abertos"
  (cons abertos sucessores))

;===========================================================================
;       FUNÇÃO DE PROCURA EM PROFUNDIDADE - DFS
;===========================================================================
(defun dfs (abertos &optional(fechados nil) (profundidade-atual 0) (profundidade-max 4) (tempo-inicial (get-universal-time))) 
  " Algoritmo de procura em profundidade primeiro"
  (cond
   ((null abertos) (solucao-nao-encontrada)) ;Se a lista de abertos estiver vazia, falha, soluçao nao encontrada
   ((solucaop (get-zona-de-jogo (first abertos)))
    (list (first abertos) (contar-nos abertos fechados) tempo-inicial))
   ((eq profundidade-max profundidade-atual) (dfs (rest abertos) fechados profundidade-atual))
   (T (dfs (abertos-dfs (rest abertos) (gerar-sucessores (first abertos))) (cons (first abertos) fechados) (1+ profundidade-atual) tempo-inicial))))

(defun abertos-dfs (abertos sucessores)
  (cond
   ((null sucessores) abertos)
   (T (cons (first sucessores) (abertos-dfs abertos (rest sucessores))))))

;===========================================================================
;       FUNÇÃO DE PROCURA A*
;===========================================================================
(defun A* (abertos &optional(fechados nil) (h 1) (tempo-inicial (get-universal-time)))
  "Algoritmo de procura A*"
  (cond
   ((null abertos) (solucao-nao-encontrada)) ;Se a lista de abertos estiver vazia, falha, soluçao nao encontrada
   ((solucaop (get-zona-de-jogo (first abertos)))
    (list (first abertos) (contar-nos abertos fechados) tempo-inicial))
   (T (A* (ordenar-lista (heuristica-sucessores (gerar-sucessores (first abertos)) h)) (cons fechados (first abertos)) h tempo-inicial))))

;===========================================================================
;       FUNÇÃO DE PROCURA IDA*
;===========================================================================
(defun IDA* (abertos &optional(fechados nil) (h 1))
  "Algoritmo de procura IDA*"
  )
;===========================================================================
;       FUNÇÕES AUXILIARES
;===========================================================================
(defun contar-nos-aux (no)
  "Retorna o numero de nos abertos ou fechados na lista de nos abertos ou fechados"
  (cond
   ((null no) 0)
   (T (1+ (contar-nos-aux (rest no))))))

(defun contar-nos (abertos fechados)
  "Retorna o numero de nos resultado da soma do numero dos nos abertos e fechados"
  (+ (contar-nos-aux abertos) (contar-nos-aux fechados)))

(defun heuristica-sucessores (sucessores &optional (h 1))
  "Função que constrói a lista de sucessores do a* para que depois sejam organizados por custo heuristico"
  (cond
   ((null  sucessores) nil)
   (T (cons (criar-no-heuristica (first sucessores) h) (heuristica-sucessores (rest sucessores))))))

(defun criar-no-heuristica (no &optional (h 1))
  "Função que adiciona o custo heuristico e a profundidade a um nó de modo a auxiliar nos algoritmos"
  (cond
   ((= h 1) (append no (list (heuristica1 (get-no-tabuleiro no)) (get-profundidade-no no))))
   ((= h 2) (append no (list (heuristica2 (get-no-tabuleiro no)) (get-profundidade-no no))))))

(defun ordenar-lista (lista)
  "Função que ordena uma lista de nos"
  (cond
   ((null lista) nil)
   (t (ordenar-no-heuristica (first lista) (ordenar-lista (rest lista))))))

(defun ordenar-no-heuristica (no lista)
  "Compara um no com uma lista de nos de modo a inserir o no numa posição ordenada segundo o custo heuristico"
  (cond
   ((or (null lista) (> (get-custo-heuristica (first lista)) (get-custo-heuristica no))) (cons no lista))
   (t (cons (first lista) (ordenar-no-heuristica no (rest lista))))))

;===========================================================================
;       FUNÇÕES PARA AVALIAÇÃO DE EFICIENCIA
;===========================================================================
(defun bissecao (L nos-gerados &optional(limite-inferior 1) (limite-superior most-positive-fixnum) (margem-erro 0.00001))
"Funcao que permite calcular o fator de ramificacao para um no. Aplica o metodo da bissecao para o calculo desta funcao com uma margem de erro de 0.00001"
 (cond
 ((< (- limite-superior limite-inferior) margem-erro) (/ (+ limite-superior limite-inferior) 2))
 ((= limite-inferior (limite-medio limite-inferior limite-superior L nos-gerados)) (bissecao L nos-gerados limite-inferior (/(+ limite-superior limite-inferior)2) margem-erro))
 (T(bissecao L nos-gerados margem-erro (/(+ limite-superior limite-inferior)2) limite-superior))))

(defun potencia (base expoente)
  "Calculo da potencia"
  (exp (* expoente (log base))))

(defun polinomio (b l g)
  "Funcao que implementa o calculo de uma funcao polinomial"
  (cond
   ((= l 1) (- b g))
   (T (+ (potencia b l) (polinomio b (- l 1) g)))))

(defun limite-medio (limite-inferior limite-superior L nos-gerados)
  (cond
   ((> (polinomio (/ (+ limite-superior limite-inferior) 2) L nos-gerados) 0) limite-inferior)
   (T limite-superior)))
