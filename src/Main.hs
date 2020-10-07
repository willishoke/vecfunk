{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (and, or)
import Data.Tuple
import Control.Arrow
import Control.Monad
import Control.Lens
import qualified Data.Vector as V

main :: IO ()
main = do
  putStrLn "vecfunk"

data Bit = Zero | One deriving (Eq, Ord)

instance Show Bit where
  show 0 = "0"
  show 1 = "1"

instance Num Bit where
  (+) = curry or
  (-) = undefined
  (*) = curry and
  negate = invert
  abs = undefined
  signum = undefined
  fromInteger n = case n of
    0 -> Zero
    1 -> One
    _ -> error "Invalid bit value"

-- "split" from Haskell wiki, but pointfree

(-<) :: Arrow a => a b (b, b)
(-<) = arr $ join (,)

-- discard second element

(>-.) :: Arrow a => a (b, b') b
(>-.)  = arr (^._1)

-- discard first element

(>-..) :: Arrow a => a (b, b') b'
(>-..) = arr (^._2)

-- swap two elements

(><) :: Arrow a => a (b, b') (b', b)
(><) = (-<) >>> first (>-..) >>> second (>-.)

-- swap middle two elements

(>><<) :: Arrow a => a ((b, b'), (c, c')) ((b, c), (b', c'))
(>><<) = (-<) 
  >>> first (first (>-.) >>> second (>-.)) 
  >>> second (first (>-..) >>> second (>-..))

-- change tuple grouping

(>/<) :: Arrow a => a ((b, b'), c) (b, (b', c))
(>/<) = second (-<)
  >>> (>><<)
  >>> first (>-.)

nand :: (Bit, Bit) -> Bit
nand (1, 1) = 0
nand _      = 1

invert :: Bit -> Bit
invert = (-<) >>> nand

and :: (Bit, Bit) -> Bit
and = nand >>> invert 

or :: (Bit, Bit) -> Bit
or = first invert >>> second invert >>> nand 

xor :: (Bit, Bit) -> Bit
xor = (-<) 
  >>> first nand 
  >>> first (-<)
  >>> (>><<)
  >>> first nand
  >>> second nand
  >>> nand

bitwise :: ((a, b) -> c) -> (V.Vector a, V.Vector b) -> V.Vector c 
bitwise = uncurry . V.zipWith . curry

mux :: ((Bit, Bit), Bit) -> Bit
mux = second (-<)
  >>> second (first invert)
  >>> (>><<)
  >>> first nand
  >>> second nand
  >>> nand

halfAdder :: (Bit, Bit) -> (Bit, Bit)
halfAdder = (&&&) xor and

fullAdder :: ((Bit, Bit), Bit) -> (Bit, Bit)
fullAdder = first halfAdder
  >>> undefined
