{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day16 where

import           Control.Monad        (mapM_)
import qualified Data.Attoparsec.Text as A
import           Data.Either          (either)
import           Data.List            (sortBy, sortOn)
import           Data.List.Extra      (chunksOf)
import qualified Data.Map.Strict      as Map
import           Data.Ord             (comparing)
import           Data.Text            (Text, pack)
import           Debug.Trace          (trace)
import Data.Bits ((.|.), (.&.))

type Params = (Int,Int,Int)

type Regs = (Int,Int,Int,Int)

type Opfun = Params -> Regs -> Regs

reg :: Regs -> Int -> Int
reg (v,_,_,_) 0 = v
reg (_,v,_,_) 1 = v
reg (_,_,v,_) 2 = v
reg (_,_,_,v) 3 = v
reg _ _ = 0

sreg :: Regs -> Int -> Int -> Regs
sreg (a,b,c,d) 0 v = (v,b,c,d)
sreg (a,b,c,d) 1 v = (a,v,c,d)
sreg (a,b,c,d) 2 v = (a,b,v,d)
sreg (a,b,c,d) 3 v = (a,b,c,v)
sreg r _ _ = r

rcmd :: (Int->Int->Int) -> Params -> Regs -> Regs
rcmd f (va,vb,vc) regs = sreg regs vc $ f (reg regs va) (reg regs vb)

icmd :: (Int->Int->Int) -> Params -> Regs -> Regs
icmd f (va,vb,vc) regs = sreg regs vc $ f (reg regs va) vb

addr :: Opfun
addr = rcmd (+)

addi :: Opfun
addi = icmd (+)

mulr :: Opfun
mulr = rcmd (*)

muli :: Opfun
muli = icmd (*)

banr :: Opfun
banr = rcmd (.&.)

bani :: Opfun
bani = icmd (.&.)

borr :: Opfun
borr = rcmd (.|.)

bori :: Opfun
bori = icmd (.|.)

setr :: Opfun
setr = rcmd const

seti :: Opfun
seti (va,_,vc) regs = sreg regs vc va

cmpir :: (Int -> Int -> Bool) -> Params -> Regs -> Regs
cmpir f (va,vb,vc) regs = sreg regs vc (if f va (reg regs vb) then 1 else 0)

cmpri :: (Int -> Int -> Bool) -> Params -> Regs -> Regs
cmpri f (va,vb,vc) regs = sreg regs vc (if f (reg regs va) vb then 1 else 0)

cmprr :: (Int -> Int -> Bool) -> Params -> Regs -> Regs
cmprr f (va,vb,vc) regs = sreg regs vc (if f (reg regs va) (reg regs vb) then 1 else 0)

gtir :: Opfun
gtir = cmpir (>)

gtri :: Opfun
gtri = cmpri (>)

gtrr :: Opfun
gtrr = cmprr (>)

eqir :: Opfun
eqir = cmpir (==)

eqri :: Opfun
eqri = cmpri (==)

eqrr :: Opfun
eqrr = cmprr (==)

allOps :: [Opfun]
allOps = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

data Test = Test Regs Regs Regs deriving (Show)

parseTest :: A.Parser Test
parseTest = do
  a <- "Before: [" *> A.decimal `A.sepBy` ", " <* "]" <* A.skipSpace
  b <- A.decimal `A.sepBy` " " <* A.skipSpace
  c <- "After:  [" *> A.decimal `A.sepBy` ", " <* "]" <* A.skipSpace

  pure $ Test (rs a) (rs b) (rs c)

  where rs [a,b,c,d] = (a,b,c,d)

evalTest :: Test -> Opfun -> Bool
evalTest (Test before (op,v1,v2,v3) after) f =
  let nregs = f (v1,v2,v3) before in
    reg after v3 == reg nregs v3

matches :: Test -> [Opfun]
matches t = filter (evalTest t) allOps

getInput :: IO (Either String [Test])
getInput = A.parseOnly (A.many1 parseTest) . pack <$> readFile "input/day16"

part1 :: IO ()
part1 = do
  (Right tests) <- getInput
  print $ length . filter (>= 3) . map (length . matches) $ tests
