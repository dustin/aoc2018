{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import           Control.Applicative        ((<|>))
import           Control.Monad              (replicateM)
import           Data.List                  (foldl')
import qualified Data.Map.Strict            as Map
import           Text.Megaparsec            (sepBy, some)
import           Text.Megaparsec.Char       (space)
import           Text.Megaparsec.Char.Lexer (decimal)

import           AoC                        (Parser, parseFile)

import           Elfcode

--
-- Stuff for part1.
--

data Test = Test Regs Regs Regs deriving (Show)

parseTest :: Parser Test
parseTest = do
  a <- "Before: [" *> decimal `sepBy` ", " <* "]" <* space
  b <- decimal `sepBy` " " <* space
  c <- "After:  [" *> decimal `sepBy` ", " <* "]" <* space

  pure $ Test (rs a) (rs b) (rs c)

  where rs [a,b,c,d] = (a,b,c,d,0,0)

evalTest :: Test -> Opfun -> Bool
evalTest (Test before (_,v1,v2,v3,0,0) after) f =
  let nregs = f (v1,v2,v3) before in
    reg after v3 == reg nregs v3

matches :: Test -> [Opfun]
matches t = filter (evalTest t) allOps

getInput :: IO [Test]
getInput = parseFile (some parseTest) "input/day16"

-- 563
part1' :: [Test] -> Int
part1' = length . filter (>= 3) . map (length . matches)

part1 :: IO ()
part1 = print =<< part1' <$> getInput

--
-- Stuff for part 2 below.
--

fit :: (a -> b -> Bool) -> [a] -> [b] -> Maybe [(a,b)]
fit f = go []

  where
    go r [] _ = Just r
    go r ks (x:xs) = foldr (\(k,ks') o ->go ((k,x):r) ks' xs <|> o)
                     Nothing (filter (\(k,_) -> f k x) $ select ks)

    select []     = []
    select (x:xs) = [(x,xs)] <> (fmap (x:) <$> select xs)


groupTests :: [Test] -> Map.Map Int [Test]
groupTests tests = Map.fromListWith (<>) $ map (\t@(Test _ (o,_,_,_,_,_) _) -> (o,[t])) tests

figureOutOpcodes :: [Test] -> Maybe (Map.Map Int Opfun)
figureOutOpcodes tests = Map.fromList <$> fit passesSome (Map.keys gt) allOps

  where
    gt = groupTests tests
    gt' = take 10 <$> gt

    passesSome :: Int -> Opfun -> Bool
    passesSome i f = all (`evalTest` f) $ gt' Map.! i


data Prog = Prog [Test] [Regs]

parseProgram :: Parser Prog
parseProgram = do
  tests <- some parseTest
  _ <- space
  regs <- some reg

  pure $ Prog tests (map rs regs)

  where reg = replicateM 4 (decimal <* space)
        rs [a,b,c,d] = (a,b,c,d,0,0)

getInput2 :: IO Prog
getInput2 = parseFile parseProgram "input/day16"

part2' :: Prog -> Regs
part2' (Prog tests inputs) =
  let (Just opcodes) = figureOutOpcodes tests in
    foldl' (\r (op,va,vb,vc,_,_) -> (opcodes Map.! op) (va,vb,vc) r) (0,0,0,0,0,0) inputs

-- (629,629,4,2)
part2 :: IO ()
part2 = print =<< part2' <$> getInput2
