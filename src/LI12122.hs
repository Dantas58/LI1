{- |
Module      : LI12122
Description : Módulo auxiliar para LI1 21/22

Tipos de dados e funções auxiliares para a realização do projeto de LI1 em 2021/22.
 -}
module LI12122 (
    -- * Tipos de dados
    -- ** Básicos
  Coordenadas , Direcao(..),
    -- ** Mapas
  Mapa , Peca(..),
    -- ** Jogo
  Jogo(..) , Jogador(..) , Movimento(..)
  ) where



-- | Par de coordenadas de uma posição no 'Mapa'.

type Coordenadas = (Int, Int)



-- | Uma peça no 'Mapa'.

data Peca = Bloco | Caixa | Porta | Vazio
  deriving (Read, Eq, Ord)

  -- ^ um bloco que é indestrutível e não movivel
  -- ^ a caixa é como um bloco mas pode ser movida pelo 'Jogador'
  -- ^ a porta é a posição final do jogo
  -- ^ um espaço vazio no 'Mapa'



type Mapa = [[Peca]]



-- | Direção de um 'Jogador' no 'Mapa'.

data Direcao = Este | Oeste
  deriving (Show, Read, Eq, Ord)



-- | O personagem que é controlado pelo 'Jogador'.

data Jogador = Jogador Coordenadas Direcao Bool 
  deriving (Read, Eq, Ord)
-- ^ a posição atual no 'Mapa'
-- ^ a direção atual Bool
-- ^ um booleano que indica se o 'Jogador' está a carregar uma 'Caixa' ou não



-- | O nível de um jogo, que inclui o puzzle (mapa) e o personagem (jogador).

data Jogo = Jogo Mapa Jogador 
  deriving (Read, Eq)
-- ^ o puzzle em si 
-- ^ o personagem do jogo



-- | Os movimentos que podem ser tomados pelo jogador em cada estado do 'Jogo'.

data Movimento = AndarEsquerda | AndarDireita | Trepar | InterageCaixa 
  deriving (Show, Read, Eq, Ord)
-- ^ a acção de andar para a esquerda
-- ^ a ação de andar para a direita
-- ^ a ação de trepar uma caixa ou bloco
-- ^ a ação de pegar ou largar uma caixa