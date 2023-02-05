{- |
Module      : Tarefa6_2021li1gXXX
Description : Resolução de um puzzle

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.

= Introdução

Na Tarefa 6 foi-nos proposto o desenvolvimento de um algoritmo capaz de calcular a possibilidade de resolver um jogo num máximo de x jogadas e, se sim, indicar qual a sequência de jogadas.

= Estratégia 

A estratégia para esta Tarefa deixou bastante a desejar tendo em conta a pequena quantidade de casos que são tidos em conta.

= Conclusão
 
Foi uma tarefa difícil como é visível. Não fui capaz de a terminar.

-}

module Tarefa6_2021li1gXXX where

import LI12122
import Tarefa1_2021li1g108
import Tarefa2_2021li1g108
import Tarefa3_2021li1g108
import Tarefa4_2021li1g108
import Niveis


{-| A função 'resolveJogo' determina a sequência de jogadas através da qual a resolução de um Jogo é possível.
   
-}
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo x jogo | (x == 0 && acabado jogo)       = Just []
                   | (x == 0 && not (acabado jogo)) = Nothing
                   | (x < minPoss jogo)             = Nothing
                   | (length (aFrente jogo) <= x && length (aFrente jogo) >= minPoss jogo) = Just (aFrente jogo)
                   | otherwise = Just []
 

{-| A função 'coords' determina as coordenadas nas quais a Porta se encontra num determinado Jogo.
   
-}
coords :: Mapa -> Coordenadas 
coords l = snd (head (filter(\(x,y) -> Porta == x) (desconstroiMapa l)))

{-| A função 'esq' indica se a Porta se encontra à esquerda ou direita das coordenadas atuais do Jogador.
   
-}
esq :: Jogo -> Bool 
esq (Jogo l (Jogador (x,y) d b)) | ((x - fst (coords l)) > 0) = True
                                 | otherwise                  = False

{-| A função 'minPoss' determina o número mínimo de jogadas possível para resolver um Jogo.
   
-}
minPoss :: Jogo -> Int 
minPoss (Jogo l (Jogador (x,y) d b)) = abs (x - (fst (coords l))) 


{-| A função 'acabado' averigua se as coordenadas do Jogador são as mesmas da Porta, ou seja, se o Jogo já está resolvido.
   
-}
acabado :: Jogo -> Bool 
acabado (Jogo l (Jogador (x,y) d b)) | (x,y) == coords l = True
                                     | otherwise         = False
                                        

{-| A função 'aFrente' determina os movimentos que se devem realizar para resolver Jogos simples, considerando apenas movimentações normais e Trepar obstáculos de altura 1, não envolvendo Caixas.
   
-}
aFrente :: Jogo -> [Movimento]
aFrente j@(Jogo l (Jogador (x,y) d b))  | finalEsquerda  = [AndarEsquerda]
                                        | finalDireita   = [AndarDireita]
                                        | normalEsquerda = AndarEsquerda : aFrente (Jogo l (Jogador (x-1,y+(vac (x-1, y) l 0)) d b)) 
                                        | normalDireita  = AndarDireita : aFrente (Jogo l (Jogador (x+1,y+(vac (x+1, y) l 0)) d b)) 
                                        | treparNormalE  = Trepar : aFrente (Jogo l (Jogador (x-1,y+(vac (x-1, y) l 0)) d b)) 
                                        | treparNormalD  = Trepar : aFrente (Jogo l (Jogador (x+1,y+(vac (x+1, y) l 0)) d b))
                                        | virarTreparE   = AndarEsquerda : Trepar : aFrente (Jogo l (Jogador (x-1,y+(vac (x-1, y) l 0)) d b)) 
                                        | virarTreparD   = AndarDireita : Trepar : aFrente (Jogo l (Jogador (x+1,y+(vac (x+1, y) l 0)) d b))
                                        | otherwise = []


                            where finalEsquerda  = (esq j && (procura (x-1, y) l) == Porta)
                                  finalDireita   = (not (esq j) && (procura (x-1, y) l) == Porta) 
                                  normalEsquerda = (esq j && coords l /= (x,y) && (procura (x-1, y) l) == Vazio)
                                  normalDireita  = (not (esq j) && coords l /= (x,y) && (procura (x+1, y) l) == Vazio)
                                  treparNormalE  = (esq j && coords l /= (x,y) && (procura (x-1, y) l) /= Vazio && (altura (x-1, y) l 0) == 1 && d == Oeste)
                                  treparNormalD  = (not (esq j) && coords l /= (x,y) && (procura (x-1, y) l) /= Vazio && (altura (x+1, y) l 0) == 1 && d == Este)
                                  virarTreparE   = (esq j && coords l /= (x,y) && (procura (x-1, y) l) /= Vazio && (altura (x-1, y) l 0) == 1 && d == Este)
                                  virarTreparD   = (not (esq j) && coords l /= (x,y) && (procura (x-1, y) l) /= Vazio && (altura (x+1, y) l 0) == 1 && d == Oeste)