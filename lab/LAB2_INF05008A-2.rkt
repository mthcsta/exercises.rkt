;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname LAB2_INF05008A-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#| >18 de setembro de 2019
 | Controle uma bolinha e a sua velocidade numa tela que tem como limite os lados da screen
 | usando as setas, "A" e "D" como controle
 |#

(require 2htdp/universe)
(require 2htdp/image)

;Struct usada para passagem de dados da posição e velocidade da bolinha para a função big-bang
;Ball:Numero Numero Numero->Struct
(define-struct Ball (x y sp))


;Definimos o LIMITE para o tamanho da tela, TAMANHO para o tamanho da bolinha,
;FONTSIZE para tamanho do texto, INITIAL_XY para o início da bolinha,
;INITIAL_SP para a velocidade inicial, ADD_SP para a quantia que sera acrescentada a velocidade e
;BALL_STRUCT para a struct que sera inserida sempre que houver um quadro no big-bang
(define LIMITE 500)
(define TAMANHO 20)
(define FONTSIZE 10)
(define INITIAL_XY (/ LIMITE 2))
(define INITIAL_SP 1)
(define ADD_SP 1)
(define BALL_STRUCT (make-Ball INITIAL_XY INITIAL_XY INITIAL_SP))

;Character->Image
(define Character (circle TAMANHO "solid" "magenta"))
;BG->Image
(define BG (empty-scene LIMITE LIMITE))


;Objetivo:Auxiliar no controle do movimento da bolinha
;Position:Numero Numero->Numero
(define (Position pos value)
 (cond
   [(<= (+ pos value) TAMANHO) (+ TAMANHO 1)]
   [(>= (+ pos value) (- LIMITE TAMANHO)) (- LIMITE TAMANHO 1)]
   [(< TAMANHO (+ pos value) (- LIMITE TAMANHO)) (+ pos value)]
   [else pos]
 )
)

;View:Ball->Image
(define (View Obj)
 ;Auxiliar para exibição da posição e a velocidade da bolinha
 (place-image (text (string-append "Speed: " (number->string (Ball-sp Obj)) "x | [X: " (number->string (Ball-x Obj)) " Y: " (number->string (Ball-y Obj)) "]") FONTSIZE "darkgray") (/ LIMITE 2) (- LIMITE FONTSIZE)
   (place-image Character (Ball-x Obj) (Ball-y Obj) BG) ;Exibição da Bolinha   
  )
)

;Objetivo: Controlar a bolinha através das setas do teclado; e sua velocidade através das teclas "A" e "D"
;Action:Ball Key->Ball
(define (Action Obj ke)
  (cond
    [(key=? ke "a") (make-Ball (Ball-x Obj) (Ball-y Obj) (+ (Ball-sp Obj) ADD_SP))]
    [(and (key=? ke "d") (> (Ball-sp Obj) 0)) (make-Ball (Ball-x Obj) (Ball-y Obj) (- (Ball-sp Obj) ADD_SP))]
    [(key=? ke "left")  (make-Ball (Position (Ball-x Obj) (- (Ball-sp Obj))) (Ball-y Obj) (Ball-sp Obj))]
    [(key=? ke "right") (make-Ball (Position (Ball-x Obj) (Ball-sp Obj)) (Ball-y Obj) (Ball-sp Obj))]
    [(key=? ke "up")    (make-Ball (Ball-x Obj) (Position (Ball-y Obj) (- (Ball-sp Obj))) (Ball-sp Obj))]
    [(key=? ke "down")  (make-Ball (Ball-x Obj) (Position (Ball-y Obj) (Ball-sp Obj)) (Ball-sp Obj))]
    [else Obj]
  )
)

#| CHECK-EXPECTs |#
;checando se esta tudo funcionando bem nos controles
(check-expect (Ball-sp (Action BALL_STRUCT "a")) (+ INITIAL_SP ADD_SP))
(check-expect (Ball-sp (Action BALL_STRUCT "d")) (- INITIAL_SP ADD_SP))
(check-expect (Ball-x (Action BALL_STRUCT "left")) (- INITIAL_XY INITIAL_SP))
(check-expect (Ball-x (Action BALL_STRUCT "right")) (+ INITIAL_XY INITIAL_SP))
(check-expect (Ball-y (Action BALL_STRUCT "up")) (- INITIAL_XY INITIAL_SP))
(check-expect (Ball-y (Action BALL_STRUCT "down")) (+ INITIAL_XY INITIAL_SP))


;Função big-bang que faz a animação acontecer
(big-bang BALL_STRUCT
  (to-draw View)
  (on-key Action)
)
