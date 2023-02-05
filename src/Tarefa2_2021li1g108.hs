{- |
Module      : Tarefa2_2021li1g108
Description : Construção/Desconstrução do mapa
Copyright   : Pedro Dantas da Cunha Pereira <a97396@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.

= Introdução

Na Tarefa 2 foi-nos proposto o desenvolvimento de duas funções distintas. Uma capaz de criar um Mapa e outra capaz de desfazê-lo. 

= Estratégia 

Na primeira parte desta Tarefa foi criada uma função que cria um Mapa consituido unicamente por Vazios. Seguidamente, cada Vazio é substituido na devida coordenada pela Peca correta, deixando apenas livres os Vazios por omissão.
Quanto à segunda parte, ou seja, a função com o objetivo de descontruir um mapa, comecei por desfazer o mapa linha por linha, ignorando apenas os Vazios, que ficam definidos por omissão. Depois isto é aplicado a todo o mapa e, finalmente, o resultado é ordenado pela coordenada Y.

= Conclusão
 
Esta foi uma tarefa bastante simples e, portanto, rápida de se concluir. Após decidir uma estratégia, o desenvolvimento da Tarefa em si foi rápido e com resultados que me parecem bastante adequados.

-}

module Tarefa2_2021li1g108 where

import LI12122
import Tarefa1_2021li1g108
import Data.List
import Tarefa3_2021li1g108


{-| A função 'constroiMapa' cria um Mapa a partir de uma lista de (Peca, Coordenadas).
   
   == Exemplo

   >>>>> constroiMapa [(Bloco, (1,1)), (Bloco, (1,0))]
   > [[Vazio,Bloco],[Vazio,Bloco]]


-}

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa l  = compileMap l 

{-| A função 'emptyMap' gera uma lista de (Peca, Coordenadas) constituida apenas por Vazios, do tamanho do Mapa que pretendemos criar.
   
   == Exemplo

   >>>>> emptyMap 1 1
   > [(Vazio, (0,0)),(Vazio, (0,1)),(Vazio, (1,0)),(Vazio, (1,1))]

-}

emptyMap x y = [ (Vazio, coords) | coords <- [(x1, y1) | y1 <- [0..y], x1 <- [0..x]]]


{-| A função 'ordList' ordena uma lista de (Peca, Coordenadas) com base na Coordenada Y.
   
   == Exemplo

   >>>>> ordList [(Bloco, (0,0)),(Vazio, (0,1)),(Porta, (1,0)),(Vazio, (1,1))]
   > [(Bloco,(0,0)),(Porta,(1,0)),(Vazio,(0,1)),(Vazio,(1,1))]

-}

ordList :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordList [] = []
ordList l  = sortOn (\(p,(x,y)) -> y ) l

{-| A função 'altera' troca a Peca numa determinada Coordenada.
   
   == Exemplo

   >>>>> altera (Bloco, (0,0)) [(Vazio, (0,0)), (Bloco, (1,0))]
   > [(Bloco,(0,0)),(Bloco,(1,0))]

-}

altera :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
altera _ [] = []
altera (x,xs) (y:ys) | xs == (snd y) = (x,xs) : ys 
                     | otherwise     = y : altera (x,xs) ys

{-| A função 'convert' usa a função altera para alterar uma várias Pecas em diferentes Coordenadas. Será utilizada para substituir na lista de Vazios as Pecas em falta.
   
   == Exemplo

   >>>>> convert [(Vazio, (0,0)),(Vazio, (0,1)),(Vazio, (1,0)),(Vazio, (1,1))] [(Bloco, (1,0)), (Bloco, (1,1))]
   > [(Vazio, (0,0)),(Vazio, (0,1)),(Bloco, (1,0)),(Bloco, (1,1))]

-}

convert :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
convert l [] = l
convert l (x:xs) = convert (altera x l) xs 


{-| A função 'convertMap' será utilizada para remover a componente Coordenadas de um tuplo (Peca, Coordenadas), deixando apenas a devida Peca.
   
   == Exemplo

   >>>>> convertMap [(Vazio, (0,0)),(Vazio, (0,1)),(Bloco, (1,0)),(Bloco, (1,1))]
   > [Vazio, Vazio, Bloco, Bloco]

-}

convertMap l = map convertRow l

             where convertRow l = map fst l


{-| A função 'compileMap' é uma junção de todas as funções auxiliares que dará origem ao Mapa final que se pretende criar. É definida uma função local separate, de modo a dividir a lista por linhas.
   
   == Exemplo

   >>>>> compileMap [(Bloco, (1,1)), (Bloco, (1,0))]
   > [[Vazio,Bloco],[Vazio,Bloco]]

-}
compileMap :: [(Peca, Coordenadas)] -> Mapa 
compileMap [] = []
compileMap l  = convertMap (separate (convert (mapa) (ordList l)))

            where separate l = [filter (\(p,(x,y)) -> y == n ) l   | n <- [0..(linhas l)]]

                  mapa = emptyMap (colunas l) (linhas l)


---------
---------

{-| A função 'desconstroiMapa' dissolve um Mapa, criando uma lista de (Peca, Coordenadas).
   
   == Exemplo

   >>>>> desconstroiMapa [[Vazio,Bloco],[Vazio,Bloco]]
   > [(Bloco, (1,0)), (Bloco, (1,1))]

-}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa [] = []
desconstroiMapa l  = ordList (concat (undoMap l 0))
                  

{-| A função 'undoRow' dissolve uma linha do Mapa dada a Coordenada Y dessa mesma linha formando uma lista (Peca, Coordenadas), mais tarde usada para decompor o Mapa por completo. Os Vazios são ignorados, ficando deinidos por omissão.
   
   == Exemplo

   >>>>> undoRow [Vazio,Bloco] 0
   > [(Bloco, (1,0))]

-}

undoRow :: [Peca] -> Int -> [(Peca, Coordenadas)]
undoRow [] _ = []
undoRow l n | (last l == Vazio)  = undoRow (init l) n
            | (last l == Caixa)  = (Caixa, (length l-1, n)) : undoRow (init l) n 
            | (last l == Bloco)  = (Bloco, (length l-1, n)) : undoRow (init l) n 
            | otherwise          = (Porta, (length l-1, n)) : undoRow (init l) n 

{-| A função 'undoMap' usa a função 'undoRow' para decompor um Mapa por completo, linha a linha, usando um contador para a Coordenada Y. 
   
   == Exemplo

   >>>>> undoMap [[Vazio,Bloco],[Vazio,Bloco]] 0
   > [[(Bloco,(1,0))],[(Bloco,(1,1))]]

-}
undoMap :: Mapa -> Int -> [[(Peca, Coordenadas)]]
undoMap [] n     = []
undoMap (x:xs) n = reverse (undoRow x n) : undoMap xs (n+1)
