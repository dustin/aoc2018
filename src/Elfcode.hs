{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Elfcode (Params, Regs, Opfun, Op,
                Program(..),
                reg, sreg,
                parseProg,
                execute,
                execute',
                execUntil,
                tron,
                runOnce,
                allOps,
                align
                )
where

import           Control.Applicative        (liftA3)
import           Data.Bits                  ((.&.), (.|.))
import           Data.Char                  (isSpace)
import           Data.List                  (intercalate, transpose)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Vector                as V
import           Debug.Trace                (trace)
import           Text.Megaparsec            (anySingle, skipManyTill, some)
import           Text.Megaparsec.Char       (eol, lowerChar, space)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Control.DeepSeq            (NFData (..))
import           GHC.Generics               (Generic)

import           AoC                        (Parser)

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
sreg (_,b,c,d,e,f) 0 v = (v,b,c,d,e,f)
sreg (a,_,c,d,e,f) 1 v = (a,v,c,d,e,f)
sreg (a,b,_,d,e,f) 2 v = (a,b,v,d,e,f)
sreg (a,b,c,_,e,f) 3 v = (a,b,c,v,e,f)
sreg (a,b,c,d,_,f) 4 v = (a,b,c,d,v,f)
sreg (a,b,c,d,e,_) 5 v = (a,b,c,d,e,v)
sreg r _ _             = r

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

data Op = Op Text !Opfun !(Int,Int,Int)

instance NFData Op where
  rnf op@(Op t _ p) = op `seq` op `seq` t `seq` p `seq` ()

instance Eq Op where
  (Op na _ pa) == (Op nb _ pb) = na == nb && pa == pb

instance Show Op where
  show (Op t _ p) = unpack t <> " " <> show p

data Program = Program !Int !(V.Vector Op) deriving (Eq, Generic)

instance NFData Program

align :: String -> String
align s = unlines . map padded $ lnscls
  where
    lns = lines s
    strip = reverse . dropWhile isSpace . reverse
    padded = strip . unwords . zipWith (\n w -> take n (w <> repeat ' ')) colens
    lnscls = map cols lns
    colens = map (maximum . map length) $ transpose lnscls
    cols [] = []
    cols l = w : cols (drop 1 s')
      where (w, s') = break (== '\t') l

instance Show Program where
  show (Program ir ops) = align $ "#ip " <> show ir <> "\n" <> intercalate "\n" (opss 0 (V.toList ops))
    where
      opss _ []      = []
      opss o (op:xs) = anop op o : opss (o+1) xs

      anop (Op "addi" _ a) ip = sicmd "addi" a "+" ip
      anop (Op "addr" _ a) ip = srcmd "addr" a "+" ip
      anop (Op "muli" _ a) ip = sicmd "muli" a "*" ip
      anop (Op "mulr" _ a) ip = srcmd "mulr" a "*" ip
      anop (Op "bori" _ a) ip = sicmd "bori" a "*" ip
      anop (Op "borr" _ a) ip = srcmd "borr" a "*" ip
      anop (Op "eqrr" _ a) ip = scmprr "eqrr" a "==" ip
      anop (Op "eqri" _ a) ip = scmpri "eqri" a "==" ip
      anop (Op "gtrr" _ a) ip = scmprr "gtrr" a ">" ip
      anop (Op "gtir" _ a) ip = scmpri "gtir" a ">" ip
      anop (Op "bani" _ a) ip = sicmd "bani" a "&" ip
      anop (Op "seti" _ (a,b,c)) ip = unwords ["seti", l a, l b, l c, "\t", l ip, r c, "=", l a]
      anop (Op "setr" _ (a,b,c)) ip = unwords ["setr", l a, l b, l c, "\t", l ip, r c, "=", r a]
      anop (Op n _ (a,b,c)) ip = unwords [unpack n, l a, l b, l c, "\t", l ip]

      sicmd name (a,b,c) op ip = unwords [name, l a, l b, l c, "\t", l ip, r c, "=", r a, op, l b]
      srcmd name (a,b,c) op ip = unwords [name, l a, l b, l c, "\t", l ip, r c, "=", r a, op, r b]
      scmprr name (a,b,c) op ip = unwords [name, l a, l b, l c, "\t", l ip, r c, "=", r a, op, r b, "? 1 : 0"]
      scmpri name (a,b,c) op ip = unwords [name, l a, l b, l c, "\t", l ip, r c, "=", l a, op, r b, "? 1 : 0"]

      r x
        | x == ir = "IR"
        | otherwise = "r" <> show x -- register
      l = show -- literal

parseProg :: Parser Program
parseProg = do
  ip <- "#ip " *> decimal <* "\n"
  ops <- some op

  pure $ Program ip (V.fromList ops)

  where
    op :: Parser Op
    op = do
      iname <- pack <$> some lowerChar
      params <- nums <* comment
      pure $ Op iname (namedOps Map.! iname) params

    nums :: Parser (Int,Int,Int)
    nums = liftA3 (,,) num num num
    num :: Parser Int
    num = space *> decimal
    comment = skipManyTill anySingle eol


evalOp :: Op -> Regs -> Regs
evalOp (Op _ f params) = f params

runOnce :: Program -> Int -> Regs -> (Int,Regs)
runOnce (Program ir ops) ip regs = let regs' = sreg regs ir ip
                                       op = ops V.! ip
                                       rr = evalOp op regs' in
                                     (1 + reg rr ir, rr)

execute :: Program -> Int -> Regs -> Regs
execute p@(Program _ ops) ip iregs = go (ip,iregs)
  where
    go (i,rs)
      | i >= V.length ops = rs
      | otherwise = go $ runOnce p i rs

execute' :: Program -> Int -> Regs -> Int -> (Int, Int, Regs)
execute' p@(Program _ ops) ip iregs ticks = go 0 (ip,iregs)
  where
    go t (i,rs)
      | t >= ticks = (0,i,rs)
      | i >= V.length ops = (t,i,rs)
      | otherwise = go (t+1) $ runOnce p i rs

tron :: Program -> Int -> Regs -> Int -> Regs
tron p@(Program _ ops) ip iregs ticks = go ticks (ip,iregs)
  where
    go t s@(i,rs)
      | t == 0 = rs
      | trace (show s) False = undefined
      | i >= V.length ops = rs
      | otherwise = go (t-1) $ runOnce p i rs

execUntil :: Program -> ((Int,Regs) -> Bool) -> Int -> Regs -> (Int,Regs)
execUntil p@(Program _ ops) f ip iregs = go $ runOnce p ip iregs
  where
    go s@(i,rs)
      | f s = s
      | i >= V.length ops = s
      | otherwise = go $ runOnce p i rs
