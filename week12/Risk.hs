{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
    (aStream :: [DieValue]) <- getRandoms
    (bStream :: [DieValue]) <- getRandoms
    let rollPairs = zip (sort . take numAttackRolls   $ aStream)
                        (sort . take numDefenderRolls $ bStream)
    return $ foldl' (\newB (attackRoll, defendRoll) ->
                        if attackRoll > defendRoll
                        then newB { defenders = defenders newB - 1}
                        else newB { attackers = attackers newB - 1})
                    b
                    rollPairs
  where
    numAttackRolls   = min 3 (attackers b - 1)
    numDefenderRolls = min 2 (defenders b)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield a b)
  | a < 2 || b == 0 = return bf
  | otherwise = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  let num = 10000
  results <- replicateM num (invade bf)
  return $ fromIntegral (wins results) / fromIntegral num
  where wins = length . filter ((==0) . defenders)

{-evalRandIO  $ successProb $ Battlefield 2 3 -}
