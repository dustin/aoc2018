{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Day24 where

import           Control.Applicative        (liftA2)
import           Data.Foldable              (maximumBy)
import           Data.List                  (intercalate, partition, sortBy,
                                             sortOn)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust)
import           Data.Ord                   (Down (..), comparing)
import           Data.Text                  (Text, pack)
import           Data.Void                  (Void)
import           Text.Megaparsec            (ParseErrorBundle, Parsec, between,
                                             endBy, option, parse, sepBy, some)
import           Text.Megaparsec.Char       (alphaNumChar, space)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Search                     (binSearch)

data Props = Props Text [Text]

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
  } deriving (Eq, Show)

type Parser = Parsec Void Text

-- Eat whitspace around a parser.
spacey :: Parser a -> Parser a
spacey f = space *> f <* space

parseArmy :: Side -> Parser Army
parseArmy _side = do
  let _id = -1
  _units <- decimal <* " units each with "
  _hp <- decimal <* " hit points "
  (_immunities, _weaknesses) <- option ([],[]) $ between "(" ")" parseProps
  _power <- spacey "with an attack that does " *> decimal
  _atType <- spacey (word <* " damage at initiative")
  _initiative <- decimal

  pure Army{..}

  where
    parseProps :: Parser ([Text],[Text])
    parseProps = liftA2 (,) (vals "immune") (vals "weak") <$> (aProp `sepBy` spacey ";")

      where
        vals :: Text -> [Props] -> [Text]
        vals pt = concatMap (\(Props t v) -> if t == pt then v else [])

        aProp :: Parser Props
        aProp = Props <$> word <* " to " <*> (word `sepBy` spacey ",")

    word :: Parser Text
    word = pack <$> some alphaNumChar

parseArmies :: Parser [Army]
parseArmies = do
  is  <- spacey "Immune System:" *> parseArmy Immune `endBy` space
  inf <- spacey "Infection:"     *> parseArmy Infection `endBy` space

  pure (idify is <> idify inf)

    where idify = zipWith (\i a -> a{_id=i}) [1..]

getInput :: IO (Either (ParseErrorBundle Text Void) [Army])
getInput = getInput' "input/day24"

getInput' :: String -> IO (Either (ParseErrorBundle Text Void) [Army])
getInput' s = parse parseArmies s . pack <$> readFile s

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
      | otherwise = (a,Just t) : assign xs (filter (\Army{_id=i} -> i /= _id t) en)
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
    ordered = sortOn (Down . _initiative . fst) ins

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


fight :: [Army] -> [Army]
fight = go . iterate aRound
  where
    go (a:b:xs)
      | a == b = a
      | otherwise = go xs

finalScore :: [Army] -> Maybe (Side, Int)
finalScore armies
  | null p1 || null p2 = Just (_side . head $ armies, sum . map _units $ armies)
  | otherwise = Nothing
  where (p1,p2) = partitionArmies armies

part1' :: [Army] -> Maybe Int
part1' as = snd <$> (finalScore . fight) as

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
  where scoreAt n = snd . fromJust . finalScore . fight $ increaseImmunity n army
        tryAt n = case finalScore . fight $ increaseImmunity n army of
                    Just (Immune,_) -> GT
                    _               -> LT

part2 :: IO ()
part2 = do
  (Right armies) <- getInput
  print $ part2' armies
