{-# LANGUAGE OverloadedStrings #-}

module Day19 where

import           Control.Applicative  (liftA3, (<|>))
import           Control.Monad        (replicateM)
import qualified Data.Attoparsec.Text as A
import           Data.Bits            ((.&.), (.|.))
import           Data.List            (foldl')
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text, pack)
import qualified Data.Vector as V
import Debug.Trace (trace)

type Params = (Int,Int,Int)

type Regs = (Int,Int,Int,Int,Int,Int)

type Opfun = Params -> Regs -> Regs

reg :: Regs -> Int -> Int
reg (v,_,_,_,_,_) 0 = v
reg (_,v,_,_,_,_) 1 = v
reg (_,_,v,_,_,_) 2 = v
reg (_,_,_,v,_,_) 3 = v
reg (_,_,_,_,v,_) 4 = v
reg (_,_,_,_,_,v) 5 = v
reg _ _             = 0

sreg :: Regs -> Int -> Int -> Regs
sreg (a,b,c,d,e,f) 0 v = (v,b,c,d,e,f)
sreg (a,b,c,d,e,f) 1 v = (a,v,c,d,e,f)
sreg (a,b,c,d,e,f) 2 v = (a,b,v,d,e,f)
sreg (a,b,c,d,e,f) 3 v = (a,b,c,v,e,f)
sreg (a,b,c,d,e,f) 4 v = (a,b,c,d,v,f)
sreg (a,b,c,d,e,f) 5 v = (a,b,c,d,e,v)
sreg r _ _           = r

rcmd :: (Int->Int->Int) -> Params -> Regs -> Regs
rcmd f (va,vb,vc) regs = sreg regs vc $ f (reg regs va) (reg regs vb)

icmd :: (Int->Int->Int) -> Params -> Regs -> Regs
icmd f (va,vb,vc) regs = sreg regs vc $ f (reg regs va) vb

cmpir :: (Int -> Int -> Bool) -> Params -> Regs -> Regs
cmpir f (va,vb,vc) regs = sreg regs vc (if f va (reg regs vb) then 1 else 0)

cmpri :: (Int -> Int -> Bool) -> Params -> Regs -> Regs
cmpri f (va,vb,vc) regs = sreg regs vc (if f (reg regs va) vb then 1 else 0)

cmprr :: (Int -> Int -> Bool) -> Params -> Regs -> Regs
cmprr f (va,vb,vc) regs = sreg regs vc (if f (reg regs va) (reg regs vb) then 1 else 0)

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

opNames :: [Text]
opNames = ["addr", "addi", "mulr", "muli",
            "banr", "bani", "borr", "bori",
            "setr", "seti", "gtir", "gtri",
            "gtrr", "eqir", "eqri", "eqrr"]

namedOps :: Map.Map Text Opfun
namedOps = Map.fromList $ zip opNames allOps

--
-- Stuff for part1.
--

data Op = Op Text (Int,Int,Int) deriving (Show)

data Program = Program Int (V.Vector Op) deriving (Show)

parseProg :: A.Parser Program
parseProg = do
  ip <- "#ip " *> A.decimal <* "\n"
  ops <- op `A.sepBy` "\n"

  pure $ Program ip (V.fromList ops)

  where
    op :: A.Parser Op
    op = Op <$> A.takeTill (== ' ') <*> nums
    nums :: A.Parser (Int,Int,Int)
    nums = liftA3 (,,) num num num
    num :: A.Parser Int
    num = A.skipSpace *> A.decimal

getInput :: IO (Either String Program)
getInput = A.parseOnly parseProg . pack <$> readFile "input/day19"

evalOp :: Op -> Regs -> Regs
evalOp (Op iname params) regs = (namedOps Map.! iname) params regs

runOnce :: Program -> Int -> Regs -> (Int,Regs)
runOnce (Program ir ops) ip regs = let regs' = sreg regs ir ip
                                       op = ops V.! ip
                                       rr = evalOp op regs' in
                                     (1 + reg rr ir, rr)

haltingProblem :: Program -> Regs
haltingProblem p@(Program _ ops) = go (0,(0,0,0,0,0,0))
  where
    go s@(i,rs)
      | i >= V.length ops = rs
      | otherwise = go $ runOnce p i rs

-- (2520,865,1,865,256,864)
part1 :: IO ()
part1 = do
  (Right prog) <- getInput
  print $ haltingProblem prog
