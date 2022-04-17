#lang racket

;TDA cardsSet - constructor. Función constructora de conjuntos válidos de cartas para el juego Dobble.


;REPRESENTACION
;una lista con sub-listas las cuales representarian las cartas del mazo
(define lista '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57))

;CONSRUCTOR



; funcion que crear la primera carta del mazo
; dom: list
; rec: list

(define firstcarta (lambda (lista iterador)
                     (cond
                       [(= (simboloCarta(length lista)) iterador)
                        null]
                       [else (cons (list-ref lista iterador) (firstcarta lista (+ iterador 1)))]))) ;se utiliza list-ref para poder jugar con cualquier simbolo

;funcion que crea las primeras n cartas del mazo


;PERTENENCIA


;SELECTOR


;MODIFICADOR


;OTRAS OPERACIONES

; Función que calcula los símbolos de las cartas implementando una ecuacion de segundo grado
(define simboloCarta (lambda (n)
                      (+ (/ (- (sqrt (- (* 4 n) 3)) 1) 2) 1)))

; Función que da la cantidad total de simbolos implemetada por el algoritmo entregado
(define totalSimbolos (lambda (n)
                       (+ (+ (* n n) n) 1)))
