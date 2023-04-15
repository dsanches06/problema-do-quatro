;===========================================================================
;        IMPLEMENTAÇÃO DO ALGORITMO ALFABETA 
;===========================================================================
;;;; algoritmo.lisp
;;;; Disciplina de IA - 2020 / 2021
;;;; Autor:
;===========================================================================
;       VARIÁVEIS GLOBAIS
;===========================================================================
(defvar *humano* 1)
(defvar *computador* 2)
(defvar *melhor-jogada* nil)
(defvar *nos-analisados* 0)
(defvar *cortes-alfa* 0)
(defvar *cortes-beta* 0)
(defvar *tempo-gasto* 0)
(defvar *tabela* (make-hash-table))
;===========================================================================
;       MEMOIZAÇÃO DO ALFABETA
;===========================================================================
(defun alfabeta-memo (no profundidade-limite operadores funcao-utilidade)
 "Função para memoize da função"
)

;===========================================================================
;       FUNÇÃO ALFABETA
;===========================================================================
(defun alfabeta (no profundidade-limite operadores funcao-utilidade)
 ""
)
;===========================================================================
;       PERCORRER SUCESSORES PARA ATUALIZAR O ALFABETA
;===========================================================================
(defun percorrer-sucessores (sucessores alfa beta profundidade operadores funcao-utilidade) 
  "Funcao que percorre cada um dos sucessores e atualiza o alfa ou beta"  
)

;===========================================================================
;     FUNÇÃO VERIFICAR MELHOR JOGADA ALFA
;===========================================================================
(defun verificar-melhor-jogada-alfa (alfa v sucessor)
  "Funcao que ira fazer o max entre o alfa e o valor de utilidade do sucessor, caso o valor de utilidade seja superior ao alfa, atualiza-se a melhor jogada para que seja o sucessor passado por argumento"
   (cond
     ((> v alfa) (setf *melhor-jogada* sucessor) v) ;Se o valor de utilidade do sucessor for maior que o alfa atual atualiza-se a melhor jogada e o alfa passa a ser o v
     (T alfa))) ;Caso contrario mantem-se a melhor jogada e o alfa

;===========================================================================
;     FUNÇÃO VERIFICAR MELHOR MELHOR JOGADA DO BETA
;===========================================================================
(defun verificar-melhor-jogada-beta (beta v sucessor)
  "Funcao que ira fazer o min entre o beta e o valor de utilidade do sucessor, caso o valor de utilidade seja inferior ao beta, atualiza-se a melhor jogada para que seja o sucessor passado por argumento"
  (cond
    ((< v beta) (setf *melhor-jogada* sucessor) v) ;Se o valor de utilidade do sucessor for maior que o alfa atual atualiza-se a melhor jogada e o alfa passa a ser o v
    (T beta))) ;Caso contrario mantem-se a melhor jogada e o alfa

;===========================================================================
;  FUNÇÃO ORDENAR SUCESSORES DE UM NÓ
;===========================================================================
(defun ordenar-sucessores (sucessores tipo-no)
  "Funcao que ordena os sucessores de um no (com base na sua funcao de utilidade) de modo a aumentar a eficiencia dos cortes. Se o no for do tipo MAX, ordenar de forma crescente, caso contrario ordenar de forma decreste"
  (cond
   ((equal tipo-no 'max) (sort sucessores #'< :key #'funcao-utilidade))
   (T (sort sucessores #'> :key #'funcao-utilidade))))

;===========================================================================
;  FUNÇÃO UTILIDADE
;===========================================================================
(defun funcao-utilidade (no) "Função utilidade")
              
        
