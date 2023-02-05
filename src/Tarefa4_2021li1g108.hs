{- |
Module      : Tarefa4_2021li1g108
Description : Movimentação do personagem
Copyright   : Pedro Dantas da Cunha Pereira <a97396@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
= Introdução

Na Tarefa 4 foi-nos proposto o desenvolvimento de uma função capaz de ler movimentos e mudar o estado de um Jogo tendo em conta esses mesmos movimentos.

= Estratégia 

Para o desenvolvimento desta Tarefa criei algumas funções auxiliares mais gerais e, seguidamente, dividi os diferentes movimentos por fases, tornando mais fácil a divisão de casos específicos para os vários movimentos distintos.

= Conclusão
 
Esta foi de longe a tarefa mais difícil devido ao vasto número de casos especiais que necessitava de ter em conta. Foi também a Tarefa que sofreu mais alterações ao longo do projeto.

-}

module Tarefa4_2021li1g108 where

import LI12122
import Tarefa1_2021li1g108
import Tarefa2_2021li1g108
import Tarefa3_2021li1g108



------ Gerais

{-| A função 'procura' procura a Peca que se encontra numa determinada coordenada de um Mapa.
   
-}
procura :: Coordenadas -> Mapa -> Peca
procura (x, y) [] = error "non existent"
procura (x,y) l | (y /= length l-1) = procura (x,y) (init l)
                | otherwise         = procAux (x,y) (last l)

                where procAux (x,y) l | (x /= length l-1) = procAux (x,y) (init l)
                                      | otherwise         = (last l)


{-| A função 'swap' substitui a Peca numa determinada coordenada por uma outra Peca.
   
-}
swap :: Coordenadas -> [Peca] -> Peca -> [Peca]
swap (x,y) [] _ = error "non existent"
swap (x,y) l p | (x /= length l-1) = swap (x,y) (init l) p ++ [last l]
               | otherwise         = (init l) ++ [p]


{-| A função 'tornaCaixa' substitui a Peca numa determinada coordenada por uma Caixa.
   
-}
tornaCaixa :: Coordenadas -> Mapa -> Mapa 
tornaCaixa _ [] = []
tornaCaixa (x,y) l | (y == length l-1) = (init l) ++ [(swap (x,y) (last l) Caixa)]
                   | otherwise         = tornaCaixa (x,y) (init l) ++ [last l]
 

{-| A função 'tornaVazio' substitui a Peca numa determinada coordenada por um Vazio.
   
-}
tornaVazio :: Coordenadas -> Mapa -> Mapa 
tornaVazio _ [] = []
tornaVazio (x,y) l | (y == length l-1) = (init l) ++ [(swap (x,y) (last l) Vazio)]
                   | otherwise         = tornaVazio (x,y) (init l) ++ [last l]


{-| A função 'altura' determina a altura a partir de uma determinada coordenada através do uso de um contador.
   
-}
altura :: Coordenadas -> Mapa -> Int -> Int 
altura (x,y) [] n = 0 
altura (x,y) m n | (procura (x,y) m == Caixa || procura (x,y) m == Bloco) = altura (x,y-1) m (n+1)
                 | otherwise                                              = n  


vac :: Coordenadas -> Mapa -> Int -> Int 
vac (x,y) [] n = 0 
vac (x,y) m n | (procura (x,y+1) m == Vazio) = vac (x,y+1) m (n+1)
              | otherwise                    = n  


------ InterageCaixa

{-| A função 'tapado' determina se uma Peca está tapada por uma Caixa ou por um Bloco.
   
-}
tapado :: Coordenadas -> Mapa -> Bool 
tapado _ [] = False 
tapado (x,y) m | (procura (x,y-1) m == Bloco) = True
               | (procura (x,y-1) m == Caixa) = True
               | otherwise                    = False 


------ Carregar a Caixa

{-| A função 'apanhaEste' determinada se é ou não possível apanhar uma Caixa que se encontre a Este do Jogador.
   
-}
apanhaEste :: Coordenadas -> Mapa -> Bool 
apanhaEste _ [] = False 
apanhaEste (x,y) m | (not offLimits && procura (x+1,y) m == Caixa && not (tapado (x+1, y) m) && not (tapado (x, y) m)) = True
                   | otherwise                                                                                         = False 

                where offLimits = (x+1) >= length (head m) 


{-| A função 'apanhaOeste' determinada se é ou não possível apanhar uma Caixa que se encontre a Este do Jogador.
   
-}
apanhaOeste :: Coordenadas -> Mapa -> Bool 
apanhaOeste _ [] = False
apanhaOeste (x,y) m | (not offLimits && procura (x-1,y) m == Caixa && not (tapado (x-1, y) m) && not (tapado (x, y) m)) = True
                    | otherwise                                                                                         = False

                where offLimits = (x-1) < 0 



------- Largar a Caixa


{-| A função 'canDropRight' determinada se é ou não possível largar uma Caixa a Este do Jogador.
   
-}
canDropRight :: Coordenadas -> Mapa -> Bool 
canDropRight _ [] = False
canDropRight (x,y) m | (not offLimits && procura (x+1,y) m == Vazio && not (tapado (x+1, y) m))  = True
                     | otherwise                                                                 = False 

                     where offLimits = (x+1) >= length (head m) 


{-| A função 'rightCase' determinada se é ou não possível largar uma Caixa a Este do Jogador no caso de a Peca diretamente a Este do mesmo não seja um Vazio.
   
-}
rightCase :: Coordenadas -> Mapa -> Bool 
rightCase _ [] = False
rightCase (x,y) m | (not offLimits && procura (x+1,y) m /= Vazio && procura (x+1,y-1) m == Vazio) = True
                  | otherwise                                                                     = False

                  where offLimits = (x+1) >= length (head m) 


{-| A função 'canDropLeft' determinada se é ou não possível largar uma Caixa a Oeste do Jogador.
   
-}
canDropLeft :: Coordenadas -> Mapa -> Bool 
canDropLeft _ [] = False
canDropLeft (x,y) m | (not offLimits && procura (x-1,y) m == Vazio && not (tapado (x-1, y) m))  = True
                    | otherwise                                                                 = False 

                 where offLimits = (x-1) < 0 

{-| A função 'leftCase' determinada se é ou não possível largar uma Caixa a Oeste do Jogador no caso de a Peca diretamente a Oeste do mesmo não seja um Vazio.
   
-}
leftCase :: Coordenadas -> Mapa -> Bool
leftCase _ [] = False
leftCase (x,y) m | (not offLimits && procura (x-1,y) m /= Vazio && procura (x-1,y-1) m == Vazio) = True
                 | otherwise                                                                     = False
 
                 where offLimits = (x-1) < 0 



------- Finais Caixa

{-| A função 'apanha' apanha uma Caixa tendo em conta o estado do Jogo e a própria possibilidade da caixa ser apanhada, utilizando as funções auxiliares definidas anteriormente.
   
-}
apanha :: Jogo -> Movimento -> Jogo 
apanha (Jogo l (Jogador (x,y) d b)) m | normalEste  = (Jogo (tornaVazio (x+1,y) (tornaCaixa (x, y-1) l)) (Jogador (x,y) d True))
                                      | normalOeste = (Jogo (tornaVazio (x-1,y) (tornaCaixa (x, y-1) l)) (Jogador (x,y) d True))
                                      | otherwise   = (Jogo l (Jogador (x,y) d b))

                                    where normalEste  = (m == InterageCaixa && d == Este && apanhaEste (x,y) l && b == False)
                                          normalOeste = (m == InterageCaixa && d == Oeste && apanhaOeste (x,y) l && b == False)



{-| A função 'apanha' larga uma Caixa tendo em conta o estado do Jogo e a própria possibilidade da caixa ser largada, utilizando as funções auxiliares definidas anteriormente.
   
-}
larga :: Jogo -> Movimento -> Jogo 
larga (Jogo l (Jogador (x,y) d b)) m | normalEste  = (Jogo (tornaVazio (x,y-1) (tornaCaixa (x+1, y+(vac (x+1, y) l 0) ) l)) (Jogador (x,y) d False))
                                     | normalOeste = (Jogo (tornaVazio (x,y-1) (tornaCaixa (x-1, y+(vac (x-1, y) l 0) ) l)) (Jogador (x,y) d False))
                                     | casoEste    = (Jogo (tornaVazio (x,y-1) (tornaCaixa (x+1, y-1) l)) (Jogador (x,y) d False))
                                     | casoOeste   = (Jogo (tornaVazio (x,y-1) (tornaCaixa (x-1, y-1) l)) (Jogador (x,y) d False))
                                     | otherwise   = (Jogo l (Jogador (x,y) d b))

                                    where normalEste  = (m == InterageCaixa && d == Este && canDropRight (x,y) l && b == True)
                                          normalOeste = (m == InterageCaixa && d == Oeste && canDropLeft (x,y) l && b == True)
                                          casoEste    = (m == InterageCaixa && d == Este && rightCase (x,y) l && b == True)
                                          casoOeste   = (m == InterageCaixa && d == Oeste && leftCase (x,y) l && b == True)


------ Trepar

{-| A função 'trepaOeste' determina se é ou não possível o Jogador Trepar imediatamente a Oeste do mesmo, no caso de não estar a carregar uma caixa.
   
-}
trepaOeste :: Coordenadas -> Mapa -> Int -> Bool
trepaOeste (x,y) m n | (offLimits || altura (x-1,y) m 0 /= 1) = False
                     | otherwise                              = True  

                    where offLimits = (x-1) < 0


{-| A função 'trepaEste' determina se é ou não possível o Jogador Trepar imediatamente a Este do mesmo, no caso de não estar a carregar uma caixa.
   
-}
trepaEste :: Coordenadas -> Mapa -> Int -> Bool
trepaEste (x,y) m n | (offLimits || altura (x+1,y) m 0 /= 1) = False
                    | otherwise                              = True

                    where offLimits = (x+1) >= length (head m)


{-| A função 'trepaCaixaO' determina se é ou não possível o Jogador Trepar imediatamente a Oeste do mesmo, no caso de estar a carregar uma caixa.
   
-}
trepaCaixaO :: Coordenadas -> Mapa -> Int -> Bool
trepaCaixaO _ [] _ = False 
trepaCaixaO (x,y) m n | (offLimits || altura (x-1,y) m 0 /= 1 || procura (x-1,y-2) m /= Vazio) = False
                      | otherwise                                                              = True  

                    where offLimits = (x-1) < 0 


{-| A função 'trepaCaixaE' determina se é ou não possível o Jogador Trepar imediatamente a Este do mesmo, no caso de estar a carregar uma caixa.
   
-}
trepaCaixaE :: Coordenadas -> Mapa -> Int -> Bool
trepaCaixaE _ [] _    = False
trepaCaixaE (x,y) m n | (offLimits || altura (x+1,y) m 0 /= 1 || procura (x+1,y-2) m /= Vazio) = False
                      | otherwise                                                              = True 

                    where offLimits = (x+1) >= length (head m) 


{-| A função 'trepa' atualiza o estado do Jogo de modo a fazer o Jogador Trepar tanto à esquerda como à direita, dependendo da possibilidade desta ação poder ser realizada.
    Isto é testado através das funções auxiliares anteriormente definidas.
   
-}
trepa :: Jogo -> Movimento -> Jogo 
trepa (Jogo l (Jogador (x,y) d b)) m | normalEste  = (Jogo l (Jogador (x+1,y-1) d b))    
                                     | normalOeste = (Jogo l (Jogador (x-1,y-1) d b))  
                                     | caixaEste   = (Jogo (tornaVazio (x,y-1) (tornaCaixa (x+1, y-2) l)) (Jogador (x+1,y-1) d b))
                                     | caixaOeste  = (Jogo (tornaVazio (x,y-1) (tornaCaixa (x-1, y-2) l)) (Jogador (x-1,y-1) d b)) 
                                     | otherwise   = (Jogo l (Jogador (x,y) d b))

                                where normalEste  = (m == Trepar && d == Este && trepaEste (x,y) l 0 && b == False)
                                      normalOeste = (m == Trepar && d == Oeste && trepaOeste (x,y) l 0 && b == False)
                                      caixaEste   = (m == Trepar && d == Este && trepaEste (x,y) l 0 && b == True && trepaCaixaE (x,y) l 0)
                                      caixaOeste  = (m == Trepar && d == Oeste && trepaOeste (x,y) l 0 && b == True && trepaCaixaO (x,y) l 0)




------ AndarDireita/Esquerda

{-| A função 'oesteLivre' determina se é ou não possível o Jogador andar no sentido Oeste, no caso de não estar a carregar uma caixa.
   
-}
oesteLivre :: Coordenadas -> Mapa -> Bool 
oesteLivre (x,y) m | (not offLimits && procura (x-1,y) m == Vazio) = True
                   | (not offLimits && procura (x-1,y) m == Porta) = True
                   | otherwise                                     = False 

                where offLimits = (x-1 < 0)
                
{-| A função 'esteLivre' determina se é ou não possível o Jogador andar no sentido Este, no caso de não estar a carregar uma caixa.
   
-}
esteLivre :: Coordenadas -> Mapa -> Bool 
esteLivre (x,y) m | (not offLimits && procura (x+1,y) m == Vazio) = True
                  | (not offLimits && procura (x+1,y) m == Porta) = True
                  | otherwise                                     = False 

                where offLimits = (x+1 >= length (head m))



{-| A função 'oesteCaixa' determina se é ou não possível o Jogador andar no sentido Oeste, no caso de estar a carregar uma caixa.
   
-}
oesteCaixa :: Coordenadas -> Mapa -> Bool 
oesteCaixa _ [] = False
oesteCaixa (x,y) m | (procura (x-1, y-1) m == Vazio) = True 
                   | otherwise                       = False

{-| A função 'esteCaixa' determina se é ou não possível o Jogador andar no sentido Este, no caso de estar a carregar uma caixa.
   
-}
esteCaixa :: Coordenadas -> Mapa -> Bool 
esteCaixa _ [] = False
esteCaixa (x,y) m | (procura (x+1, y-1) m == Vazio) = True 
                  | otherwise                       = False



{-| A função 'move' atualiza o estado do Jogo de modo a fazer com o que o Jogador se movimente para a Este ou Oeste, ou que apenas se vire para uma determinada direção (caso não seja possível movimentar-se de todo).
    A possibilidade desta movimentação é testada através das funções auxiliares anteriormente definidas.
   
-}
move :: Jogo -> Movimento -> Jogo 
move (Jogo l (Jogador (x,y) d b)) m | normalOeste = (Jogo l (Jogador (x-1, y+(vac (x-1, y) l 0)) Oeste b))
                                    | normalEste  = (Jogo l (Jogador (x+1, y+(vac (x+1, y) l 0)) Este b))
                                    | caixaOeste  = (Jogo (tornaVazio (x,y-1) (tornaCaixa (x-1, y-1+(vac (x-1, y) l 0) ) l)) (Jogador (x-1,y+(vac (x-1, y) l 0)) Oeste b))
                                    | caixaEste   = (Jogo (tornaVazio (x,y-1) (tornaCaixa (x+1, y-1+(vac (x+1, y) l 0) ) l)) (Jogador (x+1,y+(vac (x-1, y) l 0)) Este b))
                                    | viraOeste   = (Jogo l (Jogador (x,y) Oeste b))
                                    | viraEste    = (Jogo l (Jogador (x,y) Este b))
                                    | otherwise   = (Jogo l (Jogador (x,y) d b)) 

                                where normalOeste = (m == AndarEsquerda && b == False && oesteLivre (x,y) l)
                                      normalEste  = (m == AndarDireita && b == False && esteLivre (x,y) l) 
                                      caixaOeste  = (m == AndarEsquerda && b == True && oesteLivre (x,y) l && oesteCaixa (x,y) l)
                                      caixaEste   = (m == AndarDireita && b == True && esteLivre (x,y) l && esteCaixa (x,y) l)
                                      viraOeste   = (m == AndarEsquerda && not (oesteLivre (x,y) l)) 
                                      viraEste    = (m == AndarDireita && not (esteLivre (x,y) l)) 
--------
--------

{-| A função 'moveJogador' realiza um único Movimento, atualizando o estado do Jogo.
   
-}
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador j@(Jogo l (Jogador (x,y) d b)) m | (m == AndarEsquerda)               = move j m 
                                             | (m == AndarDireita)                = move j m 
                                             | (m == Trepar)                      = trepa j m 
                                             | (m == InterageCaixa && b == False) = apanha j m 
                                             | otherwise                          = larga j m 

{-| A função 'correrMovimentos' utiliza a função moveJogador recursivamente para correr uma lista de Movimentos, atualizando assim o estado do Jogo.
   
-}
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos j@(Jogo l (Jogador (x,y) d b)) [] = j
correrMovimentos j@(Jogo l (Jogador (x,y) d b)) (m:ms) = correrMovimentos (moveJogador j m) ms
