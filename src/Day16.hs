{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import           Control.Applicative  ((<|>))
import           Control.Monad        (replicateM)
import qualified Data.Attoparsec.Text as A
import           Data.Bits            ((.&.), (.|.))
import           Data.List            (foldl')
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text, pack)

type Params = (Int,Int,Int)

type Regs = (Int,Int,Int,Int)

type Opfun = Params -> Regs -> Regs

reg :: Regs -> Int -> Int
reg (v,_,_,_) 0 = v
reg (_,v,_,_) 1 = v
reg (_,_,v,_) 2 = v
reg (_,_,_,v) 3 = v
reg _ _         = 0

sreg :: Regs -> Int -> Int -> Regs
sreg (a,b,c,d) 0 v = (v,b,c,d)
sreg (a,b,c,d) 1 v = (a,v,c,d)
sreg (a,b,c,d) 2 v = (a,b,v,d)
sreg (a,b,c,d) 3 v = (a,b,c,v)
sreg r _ _         = r

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

--
-- Stuff for part1.
--

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

-- 563
part1 :: IO ()
part1 = do
  (Right tests) <- getInput
  print $ length . filter (>= 3) . map (length . matches) $ tests

--
-- Stuff for part 2 below.
--

fit :: Ord b => [a] -> [b] -> (a -> b -> Bool) -> Maybe (Map.Map b a)
fit as is f = go as is mempty

  where
    go [] _ m = Just m
    go (x:xs) ks m = foldr (\k o -> go xs (filter (/= k) ks) (Map.insert k x m) <|> o) Nothing (filter (f x) ks)

groupTests :: [Test] -> Map.Map Int [Test]
groupTests tests = Map.fromListWith (<>) $ map (\t@(Test _ (o,_,_,_) _) -> (o,[t])) tests

figureOutOpcodes :: [Test] -> Maybe (Map.Map Int Opfun)
figureOutOpcodes tests = fit allOps (Map.keys gt) passesSome

  where
    gt = groupTests tests
    gt' = take 10 <$> gt

    passesSome :: Opfun -> Int -> Bool
    passesSome f i = all (`evalTest` f) $ gt' Map.! i


data Program = Program [Test] [Regs]

parseProgram :: A.Parser Program
parseProgram = do
  tests <- A.many1 parseTest
  _ <- A.skipSpace
  regs <- A.many1 reg

  pure $ Program tests (map rs regs)

  where reg = replicateM 4 (A.decimal <* A.skipSpace)
        rs [a,b,c,d] = (a,b,c,d)

getInput2 :: IO (Either String Program)
getInput2 = A.parseOnly parseProgram . pack <$> readFile "input/day16"

-- (629,629,4,2)
part2 :: IO ()
part2 = do
  (Right (Program tests inputs)) <- getInput2
  let (Just opcodes) = figureOutOpcodes tests
  let r = foldl' (\r (op,va,vb,vc) ->
                     (opcodes Map.! op) (va,vb,vc) r) (0,0,0,0) inputs
  print r
