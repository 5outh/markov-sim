{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Markov where

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Random          as R
import           Data.List                     (foldl')
import qualified Data.HashMap.Lazy             as M
import           Data.Maybe
import           Data.Monoid
import           Data.String                   (IsString)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           System.Random.Mersenne.Pure64
import           Data.Hashable

type MarkovI a = M.HashMap a (Maybe [(a, Rational)])
newtype Markov g a = Markov{ getMarkov :: M.HashMap a (Maybe (R.Rand g a)) }
data Outcome g a =
    Error String
  | Val a g
  | End
    deriving (Show, Eq)

runMarkov1 :: (R.RandomGen g, Hashable a, Eq a, Show a) => Markov g a -> g -> a -> Outcome g a
runMarkov1 mkv gen x = case M.lookup x (getMarkov mkv) of
  Nothing -> Error "Internal error; cannot find value"
  Just rs -> case flip R.runRand gen <$> rs of
    Nothing -> End
    Just (a, g) -> Val a g

runMarkov :: (R.RandomGen g, Hashable a, Eq a, Show a, Num n, Ord n) => n -> Markov g a -> g -> a -> Either String [a]
runMarkov n mkv gen x = go n
  where
    go m | m <= 0 = Right []
         | otherwise = (x:) <$> case runMarkov1 mkv gen x of
            Val a g -> runMarkov (n-1) mkv g a
            End -> Right []
            Error err -> Left err

fromMarkovI :: R.RandomGen g => MarkovI a -> Markov g a
fromMarkovI = Markov . M.map (R.fromList <$>)

insertMkvI :: (Hashable a, Eq a) => a -> a -> MarkovI a -> MarkovI a
insertMkvI k v mkv = M.insert k (Just $ case M.lookup k mkv of
  Nothing -> [(v, 1)]
  Just xs -> case xs of
    Nothing -> [(v, 1)]
    Just ys -> (v, 1):ys) mkv

insertEnd :: (Hashable a, Eq a) => a -> MarkovI a -> MarkovI a
insertEnd k = M.insert k Nothing

insertMkvPairsInto :: (Hashable a, Eq a) => MarkovI a -> [(a, a)] -> MarkovI a
insertMkvPairsInto mkv [] = mkv
insertMkvPairsInto mkv ps = insertEnd lst $ foldl' (flip (uncurry insertMkvI)) mkv ps
  where lst = snd $ last ps

example :: [(String, [(String, Rational)])]
example = [("E", [("E", 3), ("A", 7)]), ("A", [("E", 4), ("A", 6)])]

fromList :: (Hashable a, Eq a, R.RandomGen g) => [(a, [(a, Rational)])] -> Markov g a
fromList = Markov . foldl' (flip $ uncurry ins) M.empty
  where ins a b m = case b of
          [] -> M.insert a Nothing m
          _  -> M.insert a (Just $ R.fromList b) m
