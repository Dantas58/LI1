{- |
Module      : Tarefa3_2021li1g108
Description : Representação textual do jogo
Copyright   : Pedro Dantas da Cunha Pereira <a97396@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.


= Introdução

Na Tarefa 3 foi-nos proposta a representação de um Jogo com diferentes caracteres como forma de representação dos diferentes elementos. 

= Estratégia 

Para o desenvolvimento desta Tarefa tive apenas de arranjar uma forma de procurar a linha na qual o Jogador está representado e separar esse caso do resto das linhas.

= Conclusão
 
Foi uma Tarefa que parecia simples inicialmente mas que, no entanto, deu algum trabalho devido a alguns erros que demoraram a ser corrigidos.

-}

module Tarefa3_2021li1g108 where

import LI12122


{-| A função 'aux' serve para representar um Mapa com determinados caracteres definidos mais abaixo. É usado um contador para comparar com a Coordenada Y do Jogador, de modo a distinguir a linha no qual este está localizado do resto.
   
   Após a definição deste função, todos os elementos de um Mapa tomam uma diferente representação gráfica.

   == Exemplo 

mapa =  [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
                 [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
                 [Porta,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco],
                 [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

jogo = Jogo mapa  (Jogador (5,2) Oeste False)

   >>>>> jogo
   >      X
          X
    P   C<X
    XXXXXXX

-}

aux :: Jogo -> Int -> String 
aux (Jogo [] _) _ = ""
aux (Jogo [a] _) _ = concatMap show a
aux arg@(Jogo m@(h:t) j@(Jogador (x,y) _ _)) y1 | y1 == y = concatMap show (take x h) 
                                                 ++ show j 
                                                 ++ concatMap show (drop (x+1) h) 
                                                 ++ "\n" 
                                                 ++ aux (Jogo t j) 100000
                                               
                                                | otherwise = concatMap show  h 
                                                 ++ "\n" 
                                                 ++ aux (Jogo t j) (y1 + 1)


{-| Representação de um Jogo. É utilizada a função aux com o contador iniciado em 0.

-}
instance Show Jogo where
    show jogo = aux jogo 0

{-| Representação de um Jogador.

-}
instance Show Jogador where
    show (Jogador _ Este _) = ">"
    show (Jogador _ Oeste _) = "<"

{-| Representação das diferentes Pecas.

-}
instance Show Peca where
    show Bloco = "X"
    show Porta = "P"
    show Caixa = "C"
    show Vazio = " "



mapa =  [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
         [Porta,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco],
         [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

jogo  = Jogo mapa  (Jogador (5,2) Oeste False)
jogo9 = Jogo mapa9 (Jogador (6,1) Oeste True)

mapa9 =  [[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa]
         ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
         ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]
         ,[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco]
         ,[Porta,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco]
         ,[Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]


valido :: [(Peca, Coordenadas)]
valido = [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3))
         ,(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2))
         ,(Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,0))
         ,(Bloco, (6,1)), (Bloco, (6,2)), (Bloco, (6,3))]


