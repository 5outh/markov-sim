{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
module Markov where

import           Control.Applicative
import qualified Control.Monad.Random          as R
import           Data.List                     (foldl')
import qualified Data.HashMap.Lazy             as M
import           System.Random.Mersenne.Pure64
import           Data.Hashable
import qualified Data.Text as T


type MarkovI a = M.HashMap a (Maybe [(a, Rational)])
newtype Markov g a = Markov{ getMarkov :: M.HashMap a (Maybe (R.Rand g a)) }

type Err = String
data Outcome g a =
    Error Err
  | Val a g
  | End
    deriving (Show, Eq)

runMarkov1 :: (R.RandomGen g, Hashable a, Eq a) => Markov g a -> g -> a -> Outcome g a
runMarkov1 mkv gen x = case M.lookup x (getMarkov mkv) of
  Nothing -> Error "Internal error; cannot find value"
  Just rs -> case flip R.runRand gen <$> rs of
    Nothing -> End
    Just (a, g) -> Val a g

runMarkov :: (R.RandomGen g, Hashable a, Eq a) => Integer -> Markov g a -> g -> a -> Either Err [a]
runMarkov n mkv gen x = go n
  where
    go m | m <= 0 = Right []
         | otherwise = (x:) <$> case runMarkov1 mkv gen x of
            Val a g -> runMarkov (n-1) mkv g a
            End -> Right []
            Error err -> Left err

fromMarkovI :: R.RandomGen g => MarkovI a -> Markov g a
fromMarkovI = Markov . M.map (R.fromList <$>)

insertMkvI :: (Hashable a, Eq a) => Rational -> a -> a -> MarkovI a -> MarkovI a
insertMkvI r k v mkv = M.insert k (Just $ case M.lookup k mkv of
  Nothing -> [(v, r)]
  Just xs -> case xs of
    Nothing -> [(v, r)]
    Just ys -> (v, r):ys) mkv

insertEnd :: (Hashable a, Eq a) => a -> MarkovI a -> MarkovI a
insertEnd k = M.insert k Nothing

insertMkvPairsInto :: (Hashable a, Eq a) => MarkovI a -> [(a, a)] -> MarkovI a
insertMkvPairsInto mkv [] = mkv
insertMkvPairsInto mkv ps = insertEnd lst $ foldl' (flip (uncurry (insertMkvI 1))) mkv ps
  where lst = snd $ last ps

fromList :: (Hashable a, Eq a, R.RandomGen g) => [(a, [(a, Rational)])] -> Markov g a
fromList = Markov . foldl' (flip $ uncurry ins) M.empty
  where ins a b m = case b of
          [] -> M.insert a Nothing m
          _  -> M.insert a (Just $ R.fromList b) m

wordPairs :: T.Text -> [(T.Text, T.Text)] 
wordPairs = (zip <*> tail) . T.words

insertSentence :: MarkovI T.Text -> T.Text -> MarkovI T.Text
insertSentence mkv = insertMkvPairsInto mkv . wordPairs

fromSentences :: R.RandomGen g => [T.Text] -> Markov g T.Text
fromSentences = fromMarkovI . foldl' insertSentence M.empty

runFromSentences :: Int -> [T.Text] -> IO (Either Err T.Text)
runFromSentences n sentences = do
  g <- newPureMT
  let hds = map (head . T.words) sentences
  seed <- R.uniform hds
  return $ T.unwords <$> runMarkov n (fromSentences sentences) g seed
