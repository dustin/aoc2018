{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Day24 where

import           Control.Applicative  ((<|>))
import qualified Data.Attoparsec.Text as A
import           Data.Char            (isAlpha)
import           Data.Foldable        (maximumBy)
import           Data.List            (intercalate, partition, sortBy)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (listToMaybe)
import           Data.Ord             (Down (..), comparing)
import           Data.Text            (Text, pack)

import           Search               (binSearch)

data Props = Weaknesses [Text] | Immunities [Text]

data Side = Immune | Infection deriving (Eq, Show)

data Army = Army {
  _id           :: Int
  , _side       :: Side
  , _units      :: Int
  , _hp         :: Int
  , _immunities :: [Text]
  , _weaknesses :: [Text]
  , _power      :: Int
  , _atType     :: Text
  , _initiative :: Int
  } deriving (Show)

instance Eq Army where
  Army{_id = ia, _side = sa} == Army{_id = ib, _side = sb} = ia == ib && sa == sb

parseArmy :: Side -> A.Parser Army
parseArmy side = do
  units <- A.decimal <* " units each with "
  hp <- A.decimal <* " hit points "
  (im,wk) <- ("(" *> weakAndImmune <* ")") <|> pure ([],[])
  pwr <- A.skipSpace *> "with an attack that does " *> A.decimal
  atyp <- A.skipSpace *> A.takeWhile (/= ' ') <* " damage at initiative "
  int <- A.decimal

  pure Army{_id=(-1), _side=side, _units=units, _hp=hp,
            _immunities=im, _weaknesses=wk, _power=pwr,
            _atType=atyp, _initiative=int}

  where
    weakAndImmune = do
      props <- parseProps `A.sepBy` "; "
      let wk = concatMap weakness props
      let str = concatMap strengths props
      pure (str,wk)

      where weakness (Weaknesses x) = x
            weakness _              = []
            strengths (Immunities x) = x
            strengths _              = []

    parseProps :: A.Parser Props
    parseProps = parseWeaknesses <|> parseStrengths

    parseWeaknesses = Weaknesses <$> ("weak to " *> A.takeWhile isAlpha `A.sepBy` ", ")
    parseStrengths = Immunities <$> ("immune to " *> A.takeWhile isAlpha `A.sepBy` ", ")

parseArmies :: A.Parser [Army]
parseArmies = do
  _ <- "Immune System:\n"
  is <- parseArmy Immune `A.sepBy` "\n"
  _ <- A.skipSpace
  _ <- "Infection:\n"
  inf <- parseArmy Infection `A.sepBy` "\n"

  pure (zipWith idify is [1..] <> zipWith idify inf [1..])

    where
      idify a i = a{_id=i}

getInput :: IO (Either String [Army])
getInput = getInput' "input/day24"

getInput' :: String -> IO (Either String [Army])
getInput' s = A.parseOnly parseArmies . pack <$> readFile s

targetOrder :: [Army] -> [Army]
targetOrder = sortBy (comparing (Down . effpwr) <> comparing (Down . _initiative))

damageEstimate :: Army -> Army -> Int
damageEstimate Army{_atType=atType, _units=sz, _power=pow} Army{_weaknesses=weak, _immunities=imm}
  | atType `elem` imm = 0
  | atType `elem` weak = sz*pow*2
  | otherwise = sz*pow

partitionArmies :: [Army] -> ([Army], [Army])
partitionArmies = partition (\Army{_side=a} -> a == Immune)

effpwr :: Army ->  Int
effpwr Army{..} = _units * _power

performTargeting :: [Army] -> [(Army, Maybe Army)]
performTargeting armies = assign (targetOrder side1) side2 <> assign (targetOrder side2) side1

  where
    (side1, side2) = partitionArmies armies

    assign :: [Army] -> [Army] -> [(Army,Maybe Army)]
    assign [] _ = []
    assign (a:xs) en
      | null candidates = (a,Nothing) : assign xs en
      | otherwise = (a,Just t) : assign xs (filter (/= t) en)
      where
        candidates = filter (\(x,_) -> x > 0) $ map (\x -> (damageEstimate a x,x)) en
        t = snd $ maximumBy (comparing fst <> comparing (effpwr . snd) <> comparing (_initiative . snd)) candidates

takeDamage :: Army -> Int -> Army
takeDamage a@Army{..} damage = a{_units = max 0 (_units - dmg)}
  where dmg = damage `div` _hp

performAttack :: [(Army, Maybe Army)] -> [Army]
performAttack ins = go ordered (Map.fromList $ map (\(a,_) -> (_initiative a, a)) ins)
  where
    ordered :: [(Army, Maybe Army)]
    ordered = sortBy (comparing (Down . _initiative . fst)) ins

    go :: [(Army, Maybe Army)] -> Map Int Army -> [Army]
    go [] st = Map.elems st
    go ((_,Nothing):xs) m = go xs m
    go ((Army{_initiative=aid},_):xs) m
      | Map.notMember aid m = go xs m -- recently killed
    go ((Army{_initiative=aid},Just Army{_initiative=tid}):xs) m = go xs attack
      where
        a = m Map.! aid -- current state of the attacker
        t = m Map.! tid -- current state of the victim
        attack = let newa@Army{_initiative=int, _units=u} = takeDamage t (damageEstimate a t) in
                   if u == 0 then Map.delete int m
                   else Map.insert int newa m

aRound :: [Army] -> [Army]
aRound = performAttack . performTargeting

showArmy :: Army -> String
showArmy Army{..} = mconcat [show _side, " Group ", show _id, ", Units:", show _units]

showArmies :: [Army] -> String
showArmies armies = "Immune System:\n" <> intercalate "\n" (map showArmy ims) <>
                    "\n\nInfections:\n" <> intercalate "\n" (map showArmy infs)

  where (ims, infs) = partitionArmies armies

gameOver :: [Army] -> Bool
gameOver armies = null p1 || null p2
  where (p1,p2) = partitionArmies armies

part1' :: [Army] -> Int
part1' = score . head . dropWhile (not . gameOver) . iterate aRound
  where score = foldr (\Army{..} o -> o + _units) 0

part1 :: IO ()
part1 = do
  (Right armies) <- getInput
  print $ part1' armies

increaseImmunity :: Int -> [Army] -> [Army]
increaseImmunity x = map incrim
  where incrim a@Army{_side=Immune, _power=pow} = a{_power=pow+x}
        incrim a                                = a

part2' :: [Army] -> Int
part2' army = let ans = binSearch tryAt 1 100000 in
                scoreAt ans
  where scoreAt n = score . head . play $ increaseImmunity n army
        play = dropWhile (not.gameOver) . take 100000 . iterate aRound
        score = foldr (\Army{..} o -> o + _units) 0
        tryAt n =
          case winner . listToMaybe . play $ (increaseImmunity n army) of
            Just Immune -> GT
            _           -> LT
        winner :: Maybe [Army] -> Maybe Side
        winner l = _side . head <$> l

part2 :: IO ()
part2 = do
  (Right armies) <- getInput
  print $ part2' armies
