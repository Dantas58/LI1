{- |
Module      : Tarefa1_2021li1g108
Description : Validação de um potencial mapa
Copyright   : Pedro Dantas da Cunha Pereira <a97396@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.


= Introdução

Na Tarefa 1 foi-nos proposta a testagem da validade de um potencial mapa para o nosso jogo.

= Estratégia 

Foram divididos os diferentes aspetos a ser testados por diferentes fases. Na função final desta Tarefa (validaPotencialMapa) todas as funções de testagem são utilizadas para averiguar a validade do mapa.
Algumas das funções desenvolvidas nesta tarefa serão também utilizadas em tarefas futuras (nomeadamente as funções colunas e linhas).

= Conclusão
 
Foi um tarefa realizada de forma relativamente rápida, com excessão da validade do chão que ficou aquém das minhas espectativas. Foram feitas bastantes alterações ao longo do tempo para tentar melhorar o código. 

-}

module Tarefa1_2021li1g108 where

import LI12122
import Tarefa3_2021li1g108


fazmapablocos x y = [ (Bloco, coords) | coords <- [(x1, y1) | y1 <- [0..y], x1 <- [0..x]]]

------ Peças na mesma Coord

{-| A função 'coordsVal' determina se existe ou não mais do que uma declaração para a mesma coordenada. 
   
   == Exemplos 

   >>>>> coordsVal [(Bloco, (1,1)), (Bloco, (1,0))]
   > False

   >>>>> coordsVal [(Bloco, (1,1)), (Vazio, (1,2))]
   > True

-}

coordsVal :: [(Peca, Coordenadas)] -> Bool
coordsVal [] = True 
coordsVal ((x,y):xs) = aux (x,y) xs && coordsVal xs 

                where aux (x,xs) l =  not (any (\(_,y) -> xs == y) l)

--coordsVal ((x,y):xs) | (aux (x,y) xs > 0) = False 
--                     | otherwise          = coordsVal xs
--                     where aux (x,xs) l = length (filter (\(_,y) -> xs == y) l) 


------ Porta

{-| A função 'portaVal' determina se existe uma e uma só porta num potencial mapa. 
   
   == Exemplos 

   >>>>> portaVal [(Bloco, (1,1)), (Vazio, (1,2))]
   > False

   >>>>> portaVal [(Porta, (1,1)), (Porta, (1,2))]
   > False 

   >>>>> coordsVal [(Bloco, (1,1)), (Porta, (1,2))]
   > True

-}

portaVal :: [(Peca, Coordenadas)] -> Bool
portaVal [] = False 
portaVal l | (length (filtro) /= 1) = False
           | otherwise              = True

     where filtro = filter(\(x,y) -> Porta == x) l


------ Chao

{-| A função 'chaoVal' determina se o chão de um determinado potencial mapa é valido, ou seja, se a última linha é composta totalmente por Blocos. 
   
   == Exemplos 

   >>>>> portaVal [(Bloco, (0,1)), (Vazio, (1,1))]
   > False

   >>>>> portaVal [(Bloco, (0,1)), (Bloco, (1,1))]
   > True 

-}

chaoVal :: [(Peca, Coordenadas)] -> Bool
chaoVal [] = False
chaoVal l  | (chao == (colunas l)) = True
           | otherwise             = False

         where filtro l n = filter(\(x,y) -> n == (snd y) && Bloco == x) l 

               chao = length (filtro l (linhas l)) - 1


{-| A função 'linhas' determina a coordenada Y mais alta de um potencial mapa (ou seja, o número de linhas).
   
   == Exemplo 

   >>>>> portaVal [(Bloco, (0,1)), (Vazio, (1,2))]
   > 2

-}

linhas :: [(Peca, Coordenadas)] -> Int
linhas [] = 0
linhas l = maximum (map(\(x,y) -> (snd y)) l)


{-| A função 'colunas' determina a coordenada X mais alta de um potencial mapa (ou seja, o número de colunas).
   
   == Exemplo

   >>>>> portaVal [(Bloco, (0,1)), (Vazio, (1,2))]
   > 1

-}

colunas :: [(Peca, Coordenadas)] -> Int
colunas [] = 0
colunas l = maximum (map(\(x,y) -> (fst y)) l)


------ Caixas

{-| A função 'caixaVal' determina se todas as Caixas têm um suporte, ou seja, se não existe nenhuma Caixa a flutuar. 
   
   == Exemplos 

   >>>>> caixaVal [(Bloco, (0,3)), (Caixa, (1,1))]
   > False

   >>>>> caixaVal [(Bloco, (1,1)), (Caixa, (1,0))]
   > True 

-}

caixaVal :: [(Peca, Coordenadas)] -> Bool 
caixaVal [] = False
caixaVal l = supCaixa (search (mudaCoords (filtro)) l)

      where filtro = filter(\(x,y) -> Caixa == x) l


{-| A função 'mudaCoords' utiliza as Coordenadas das Caixas para determinar todas as Coordenadas diretamente abaixo destas, de forma a mais tarde averiguar se existem ou não Caixas sem suporte. 
   
   == Exemplo

   >>>>> portaVal [(Bloco, (0,1)), (Vazio, (1,1))]
   > [(0,2), (1,2)]

-}

mudaCoords :: [(Peca, Coordenadas)] -> [Coordenadas]
mudaCoords [] = []
mudaCoords l = map (\(x,(y,ys)) -> (y,ys+1)) l 


{-| A função 'search' utiliza as Coordenadas diretamente abaixo de todas as Caixas para procurar as Pecas nessas mesmas Coordenadas, de modo a determinar se existe alguma Caixa a flutuar. 
   
   == Exemplos 

   >>>>> search [(0,1)] [(Bloco, (0,1)), (Vazio, (1,1))]
   > [Bloco]

   >>>>> search [(0,1), (0,2)]
   > [Bloco, Vazio]

-}

search :: [Coordenadas] -> [(Peca, Coordenadas)] -> [Peca]
search [] _ = []
search l [] = replicate (length l) Vazio 
search (x:xs) (y:ys) | (x == snd y) = (fst y) : search xs ys
                     | otherwise    = search (x:xs) ys

{-| A função 'supCaixa' determina ou não se existe ou não um Vazio numa lista de Pecas (de modo a determinar se existe efetivamente alguma Caixa a flutuar). 
   
   == Exemplos

   >>>>> supCaixa [Bloco, Bloco, Caixa, Bloco]
   > True

   >>>>> supCaixa [Bloco, Caixa, Vazio, Caixa]
   > False

-}

supCaixa :: [Peca] -> Bool
supCaixa [] = True
supCaixa (x:xs) | (x /= Vazio) = supCaixa xs
                | otherwise    = False

------ Vazio

{-| A função 'vazio' determina se existe algum Vazio num potencial mapa. 
   
   == Exemplos

   >>>>> portaVal [(Bloco, (0,1)), (Vazio, (1,1))]
   > True

   >>>>> portaVal [(Bloco, (0,1)), (Bloco, (1,1))]
   > True 

-}

vazio :: [(Peca, Coordenadas)] -> Bool 
vazio [] = True
vazio l | (isVazio l || isNotFull l) = True
        | otherwise                  = False 


{-| A função 'isVazio' determina se existe algum Vazio declarado diretamente num potencial mapa. 
   
   == Exemplos 

   >>>>> isVazio [(Bloco, (0,1)), (Vazio, (1,1))]
   > True

   >>>>> isVazio [(Bloco, (0,1)), (Bloco, (1,1))]
   > False

-}

isVazio :: [(Peca, Coordenadas)] -> Bool
isVazio l | length (filter(\(x,y) -> Vazio == x) l) > 0 = True
          | otherwise                                   = False


{-| A função 'isNotFull' determina se existe algum Vazio declarado por omissão num potencial mapa. 
   
   == Exemplos 

   >>>>> isNotFull [(Bloco, (0,1)), (Bloco, (1,1))]
   > True

   >>>>> isNotFull [(Bloco, (0,0) ,(Bloco, (0,1)), (Bloco, (1,1)), (Bloco, (1,0))]
   > False

-}

isNotFull :: [(Peca, Coordenadas)] -> Bool
isNotFull [] = False
isNotFull l | (length l < (linhas l + 1) * (colunas l + 1)) = True
            | otherwise                                     = False



------ Valida

{-| A função 'validaPotencialMapa' determina a validade de um potencial mapa através da utilização das funções anteriormente definidas. 
   
-}

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa l | (caixaVal l && chaoVal l && portaVal l && coordsVal l && vazio l) = True
                      | otherwise                                                         = False