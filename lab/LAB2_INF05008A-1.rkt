;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname LAB2_INF05008A-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#| 18 de setembro de 2019
 | Criando Emissora, e Filmes e Series para ela.
 | Relacionando structs Emissora com outras que chamam por Serie e Filme
 | Funções de Calcular a duração total da junção de todos os episodios e temporadas da serie,
 | Acrescentar episodio a serie ja existente, checar se está disponivel para streaming,
 | comparar suas durações e mudar de emissora
 |#

;Emissora:(nome:String cidadesede:String streaming?:Boolean)
(define-struct Emissora(nome cidadesede streaming?))

;Serie:(nome:String emissora:Emissora numero-episodios:Numero duracao-media:Numero numero-temporadas:Numero)
(define-struct Serie (nome emissora numero-episodios duracao-media numero-temporadas))

;Filme:(nome:String genero:String duracao:Numero ator-principal:String ator-coadjuvante:String emissora:Emissora)
(define-struct Filme (nome genero duracao ator-principal ator-coadjuvante emissora))

;Emissoras
(define AMC (make-Emissora "AMC" "Nova Iorque" #true))
(define Netflix (make-Emissora "Netflix" "Los Gatos" #false))
(define SBT (make-Emissora "SBT" "Osasco" #false))

;Séries
(define BB (make-Serie "Breaking Bad" AMC 64 40 5))
(define Dark (make-Serie "Dark" Netflix 18 50 2))
(define Chaves (make-Serie "Chaves" SBT 33 44 8))

;Filmes
(define IceAge (make-Filme "Era do Gelo" "Aventura" 120 "Esquizo" "Mamute" SBT))
(define Teste (make-Filme "Testezika" "Comédia" 120 "Fabiola Porchat" "Mauro" AMC))


;Dada uma série ou um filme
;Retorne a duração total
;DuracaoTotal:FilmeOuSerie->Numero
(define (DuracaoTotal View) 
  (cond
    [(Filme? View) (Filme-duracao View)]
    [(Serie? View) (* (Serie-duracao-media View) (Serie-numero-episodios View) (Serie-numero-temporadas View))]
    [else 0]
   )
)

(check-expect (DuracaoTotal IceAge) 120) ; Filme
(check-expect (DuracaoTotal BB) 12800) ; Serie
(check-expect (DuracaoTotal "nada") 0) ; Duraçao Total de nada


;Dado o tempo de um novo episódio e uma série
;Retorne a serie com a nova média de duração
;NovoEpisodio:Numero Serie->Serie
(define (NovoEpisodio tempo serie)
  (make-Serie (Serie-nome serie) (Serie-emissora serie) (+ (Serie-numero-episodios serie) 1) (round (/ (+ (DuracaoTotal serie) tempo) (* (Serie-numero-episodios serie) (Serie-numero-temporadas serie)))) (Serie-numero-temporadas serie))  
)

(check-expect (Serie-numero-episodios (NovoEpisodio 40 BB)) 65)
(check-expect (Serie-duracao-media (NovoEpisodio 40 BB)) 40)


;podePassarNetflix?:SerieOuFilme->temStreaming?
;Dada uma série
;Retorne se tem na Netflix ou não 
(define (podePassarNetflix? serieOuFilme)
  (cond
   [(Serie? serieOuFilme) (temStreaming? (Serie-emissora serieOuFilme))]
   [(Filme? serieOuFilme) (temStreaming? (Filme-emissora serieOuFilme))]
   [else #false]
  )
)
;temStreaming?:Emissora->Boolean
;Objetivo:Auxiliar para função podePassarNetflix?
;Dada uma Emissora
;Retorna se ela tem na netflix ou seu streaming próprio
(define (temStreaming? emissora)
  (cond
   [(or (not (Emissora-streaming? emissora)) (equal? (Emissora-nome emissora) "NetFlix")) #true]
   [else #false]
  )
)

(check-expect (podePassarNetflix? BB) #false)
(check-expect (podePassarNetflix? IceAge) #true)
(check-expect (podePassarNetflix? Dark) #true)
(check-expect (podePassarNetflix? "nada") #false)


;Dado dois filmes ou series
;Retorne uma função comparativa das durações
;CompararDuracao:SerieOuFilme SerieOuFilme->Boolean
(define (CompararDuracao titulo1 titulo2)
  (cond
    [(and (or (and (Filme? titulo1) (Filme? titulo2)) (and (Serie? titulo1) (Serie? titulo2)) ) (< (DuracaoTotal titulo1) (DuracaoTotal titulo2)) ) #true]
    [else #false]
  )
)

(check-expect (CompararDuracao BB IceAge) #false)
(check-expect (CompararDuracao Teste IceAge) #false)
(check-expect (CompararDuracao Dark BB) #true)


;Dada uma série e uma nova emissora
;Retorne a série atualizada com essa nova emissora
;MudarEmissora:Serie Emissora->Serie
(define (MudarEmissora p novaemissora)
  (cond
    [(and (Serie? p) (Emissora? novaemissora)) (make-Serie (Serie-nome p) novaemissora (Serie-numero-episodios p) (Serie-duracao-media p) (Serie-numero-temporadas p))]
    [(and (Filme? p) (Emissora? novaemissora)) (make-Filme (Filme-nome p) (Filme-genero p) (Filme-duracao p) (Filme-ator-principal p) (Filme-ator-coadjuvante p) novaemissora)]
    [else #false]
  )
)

(check-expect (Serie-emissora (MudarEmissora BB SBT)) SBT)
(check-expect (Filme-emissora (MudarEmissora IceAge Netflix)) Netflix)
(check-expect (MudarEmissora IceAge BB) #false)
(check-expect (MudarEmissora IceAge "nada") #false)
