;===========================================================================
;       FUNÇÕES PARA RESOLVER OS PROBLEMAS DEFINIDOS
;===========================================================================
;;;; algoritmo.lisp
;;;; Disciplina de IA - 2020 / 2021
;;------------------------------------------------------------------------------------------------------
;;  VARIAVEIS GLOBAIS
;;------------------------------------------------------------------------------------------------------
(defparameter *cortes-alfa* 0) ;numero de cortes alfa
(defparameter *cortes-beta* 0) ;numero de corte beta
(defparameter *nos-analisados* 0) ;numero de nos visitados 
(defparameter *tempo-gasto* 0)
(defparameter *jogada* NIL)
(defparameter *tabela* (make-hash-table))

;;------------------------------------------------------------------------------------------------------
;;  FUNÇÃO HASH-NODE
;;------------------------------------------------------------------------------------------------------
(defun hash-node (no)
  "Função hash que converte o estado e as pecas de reservas de um no numa string para que possa ser usada como key na hash table de memoizacao"
  (concatenate 'string (format nil "~S" (get-tabuleiro-de-jogo no)) (format nil "~S" (get-pecas-de-reserva no))))

;;------------------------------------------------------------------------------------------------------
;;  MEMOIZAÇÃO 
;;------------------------------------------------------------------------------------------------------
(let ((tab (make-hash-table :test 'equal)))
  (defun alfabeta-memo (no profMax &optional(alfa -10e11) (beta 10e11) (tempo-inicial (get-universal-time)) (tempo-max *tempo-jogada*))
    "Funcao que verifica se ja existe um resultado alfabeta para o no passado na hash table, 
     caso exista devolve-o, caso nao exista calcula o seu valor, retorna-o e insere-o na hash table"
    (or (gethash (hash-node no) tab)
        (let ((resultado-alfabeta (alfa-beta no profMax alfa beta tempo-inicial tempo-max)))
          (setf (gethash (hash-node no) tab) resultado-alfabeta)
          resultado-alfabeta))))

;;------------------------------------------------------------------------------------------------------
;;  FUNÇOES PARA IMPLEMENTAR ALGORITMO ALFA-BETA
;;------------------------------------------------------------------------------------------------------
(defun alfa-beta (no profMax &optional(alfa -10e11) (beta 10e11) (tempo-inicial (get-universal-time)) (tempo-max *tempo-jogada*))
  "Função alfabeta, com cortes"
  (let* ((jogador (isMaxOrMin no))
         (contar-nos (setf *nos-analisados* (1+ *nos-analisados*)))
         (tempo-atual (get-universal-time))
         (tempo-aux (setf *tempo-gasto* (- tempo-atual tempo-inicial)))
         (profundidade-atual (get-profundidade no)))
    (cond ((>= (- tempo-atual tempo-inicial) tempo-max) (avaliar-folha no)) 
          ((no-terminal-p no) (avaliar-folha no))
          ((and (not (null profundidade-atual)) (= profundidade-atual  0))
           (alfa-value profMax (no-sucessores no) alfa beta tempo-inicial tempo-max)) ;no 1º nivel nao precisa de trocar de peça, mas nos restantes precisa
          ((eq jogador 'MAX)        
           (alfa-value profMax (no-sucessores no) alfa beta tempo-inicial tempo-max)) ;Maximizer sem defparameter
          (T (beta-value profMax (no-sucessores no) alfa beta tempo-inicial tempo-max)))))

;;------------------------------------------------------------------------------------------------------
;;  FUNÇÃO MAXIMIZER
;;------------------------------------------------------------------------------------------------------
;; Função Maximizer
(defun alfa-value (profMax lista-sucessores alfa beta tempo-inicial tempo-max)
  "Função para descobrir o valor mais alto entre o value e a lista de sucessores recebidos"
  (cond ((null lista-sucessores) alfa) ;condição de paragem
        (t (let* ((no-filho (car lista-sucessores))
                  (value-sucessor (alfa-beta no-filho profMax alfa beta tempo-inicial tempo-max)) ;vai invocar o alfabeta para o primeiro sucessor da lista
                  (new-value-alfa (verifica-sucessor-maior alfa value-sucessor no-filho))) ;vai buscar o maximo entre o alfa e o valor anteriormente calculado
             (cond ((<= beta new-value-alfa) (setf *cortes-beta* (1+ *cortes-beta*)) beta)
                   (t (alfa-value profMax (cdr lista-sucessores) new-value-alfa beta tempo-inicial tempo-max))))))) ;Faz set à jogada com o first dos sucessores?

;;------------------------------------------------------------------------------------------------------
;;  FUNÇÃO MINIMIZER
;;------------------------------------------------------------------------------------------------------
;; Função Minimizer
(defun beta-value (profMax lista-sucessores alfa beta tempo-inicial tempo-max)
  "Função para descobrir o valor mais baixo entre o value e os valores dos sucessores"
  (cond ((null lista-sucessores) beta) ;condição de paragem
        (t (let* ((no-filho (car lista-sucessores)) 
                  ;vai invocar o alfabeta para o primeiro sucessor da lista
                  (value-sucessor (alfa-beta no-filho profMax alfa beta tempo-inicial tempo-max))
                  (new-value-beta (verifica-sucessor-menor beta value-sucessor))) ;vai buscar o minimo entre o alfa e o valor anteriormente calculad
             (cond ((<= new-value-beta alfa) (setf *cortes-alfa* (1+ *cortes-alfa*)) alfa)
                   (t (beta-value profMax (cdr lista-sucessores) alfa new-value-beta tempo-inicial tempo-max)))))))

;;------------------------------------------------------------------------------------------------------
;;  FUNÇOES UTEIS
;;------------------------------------------------------------------------------------------------------
(defun verifica-sucessor-maior (alfa value-sucessor sucessor)
  "Função que verifica se o valor devolvido pelo sucessor é maior, se sim, atualiza a jogada"
  (cond ((> value-sucessor alfa) (setf *jogada* sucessor) value-sucessor)
        (t alfa)))

(defun verifica-sucessor-menor (beta value-sucessor)
  "Função que verifica se o valor devolvido pelo sucessor é menos, se sim atualiza a jogada"
  (cond ((< value-sucessor beta) value-sucessor)
        (t beta)))

(defun isMaxOrMin (no)
  "Verificar se um no é MIN ou MAX, faz o mesmo que o isMax, só que retorn MAX ou MIN, em vez de um valor lógico"
  (let ((profundidade (get-profundidade no)))
    (cond ((OR (null profundidade) (zerop profundidade) (evenp profundidade)) 'MAX)
          (t 'MIN))))

(defun avaliar-folha (no)
  "Função que calcula o valor a devolver de acordo com a jogada do jogador"
  (let ((valor-f-avaliacao (f-avaliacao no))) valor-f-avaliacao))

(defun f-avaliacao (no)
  "Função que recebe um no final e devolve a avaliacao do no"
  (let ((vencedor (obter-vencedor no)))
    (cond 
     ((null vencedor) 0)
     ((= vencedor 1) 100) ;;maximiza a jogada
     (T -100)))) ;;minimiza a jogada adversaria

(defun obter-vencedor (no)
  "Função que recebe um no e devolve o vencedor"
  (cond
   ((solucaop (get-tabuleiro-de-jogo no)) (get-jogador no))
   (T NIL)))  

(defun no-terminal-p (no)
  "Função que devolve se o nó é solucao ou o tabuleiro está preenchido"
  (or (solucaop (get-tabuleiro-de-jogo no))
      (not (tabuleiro-preenchido-p no))))