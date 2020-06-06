{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Control.Category as C
import qualified Control.Arrow as A
import Algebra.Graph.AdjacencyMap
import qualified Data.Vector as V
import Control.Lens

main :: IO ()
main = do
  putStrLn "vecfunk"

data Node = TRUE 
          | FALSE
          | NAND 
          | SPLIT 
          | JOIN 
          | INVERTER
  deriving (Show)

-- This forces overlay to duplicate identical nodes

instance Eq Node where
  (==) x y = False

instance Ord Node where
  compare x y = LT

data Comp = Comp
  { _func :: V.Vector Bool -> V.Vector Bool
  , _io's :: (Int, Int)
  , _circ :: AdjacencyMap Node
  }

makeLenses ''Comp

instance Show Comp where
  show comp = show (_circ comp) <> " " <> show (_io's comp)

instance Eq Comp where
  (==) x y = _io's x == _io's y

eval :: Comp -> V.Vector Bool -> V.Vector Bool
eval c v = _func c v

(-->) :: Comp -> Comp -> Comp
(-->) c1 c2
  | snd (_io's c1) /= fst (_io's c2) = error "arity mismatch"
  | otherwise = Comp
      { _func = _func c1 A.>>> _func c2
      , _io's = (fst (_io's c1), snd (_io's c2))
      , _circ = overlay (_circ c1) (_circ c2)
      }

-- Work in progress
(>->) :: Comp -> Comp -> Comp -> Comp
(>->) c1 c2 c3
  | snd (_io's c1) /= fst (_io's c3) || snd (_io's c2) /= fst (_io's c3) = error "arity mismatch"
  | otherwise = Comp
      { _func = undefined -- (_func c1, _func c2) A.>>> _func c2
      , _io's = (fst (_io's c1), snd (_io's c2))
      , _circ = overlay (_circ c1) (_circ c2)
      }

nand :: Comp
nand = Comp
  { _func = \v -> pure $ not $ (v V.! 0) && (v V.! 1)
  , _io's = (2,1)
  , _circ = vertex NAND 
  }

inv :: Comp
inv = Comp
  { _func = \v -> fmap not v
  , _io's = (1,1)
  , _circ = vertex INVERTER
  }

true :: Comp
true = Comp
  { _func = \v -> pure True
  , _io's = (0,1)
  , _circ = vertex TRUE 
  } 

false :: Comp
false = Comp
  { _func = \v -> pure False
  , _io's = (0,1)
  , _circ = vertex FALSE 
  } 
 
{--
(-->) c1 c2 = case _func c1 of 
  (OneTwo f) -> case _func c2 of
    (OneTwo _) -> eror "arity mismatch"
    (TwoOne g) -> Comp 
      { _func = OneOne (g . f)
      , _circ = connect (c1^.circ) (c2^.circ) }

(<--) :: Comp -> Comp -> Comp
(<--) = flip (-->)
--} 
