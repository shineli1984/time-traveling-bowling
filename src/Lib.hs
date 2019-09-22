{-# LANGUAGE RecursiveDo, NamedFieldPuns, LambdaCase #-}

module Lib ( emptyGame
           , addFrame
           , addLastFrame
           , scoreGame
           , Scores(..)
           , Frame(..)
           , LastFrame(..) ) where

import Control.Monad.Tardis ( getFuture
                            , getPast
                            , sendFuture
                            , sendPast
                            , evalTardis
                            , Tardis )
import Data.Maybe ( listToMaybe
                  , fromMaybe 
                  , isNothing )

data Game = Game
  { frames    :: [Frame]
  , lastFrame :: Maybe LastFrame }

data Frame = Strike
            | Spare { firstThrow              :: Int }
            | Frame { firstThrow, secondThrow :: Int }
            deriving Show

data LastFrame = LastStrike { bonus1, bonus2 :: Int }
               | LastSpare  { throw1, bonus1 :: Int }
               | LastFrame  { throw1, throw2 :: Int }

-- smart constructor
emptyGame :: Game
emptyGame = Game [] Nothing

addFrame :: Frame -> Game -> Maybe (Game, Scores)
addFrame frame game =
    if numOfFrames game == 9
    then Nothing
    else Just (newGame, scoreGame newGame)
      where
        newGame = Game (frames game <> [frame]) Nothing

addLastFrame :: LastFrame -> Game -> Maybe (Game, Scores)
addLastFrame lastFrame game =
    if numOfFrames game /= 9
    then Nothing
    else Just (newGame, scoreGame newGame)
      where
        newGame = Game (frames game) (Just lastFrame)

-- bw/fw states
newtype Scores     = Scores [Int] deriving (Show)
newtype NextThrows = NextThrows (Int, Int)

-- utils
numOfFrames :: Game -> Int
numOfFrames = length . frames

lastOr0 :: [Int] -> Int
lastOr0 = fromMaybe 0 . listToMaybe

isInProgress :: Game -> Bool
isInProgress = isNothing . lastFrame

-- score a game
scoreGame :: Game -> Scores
scoreGame game = flip evalTardis initialState . calc . frames $ game
  where
    calc :: [Frame] -> Tardis NextThrows Scores Scores
    calc = \case
        [Strike] | isInProgress game         -> do 
            sendPast . NextThrows $ (10, 0)
            getPast 
        [Strike, Strike] | isInProgress game -> do
            sendPast . NextThrows $ (10, 10)
            getPast 
        [Spare n] | isInProgress game        -> do
            sendPast . NextThrows $ (n, 10 - n)
            getPast 
        f1 : fs -> mdo
            let (score', throws') = addScore scores f1 throws
            sendPast throws'
            scores@(Scores scores') <- getPast
            throws <- getFuture
            sendFuture . Scores $ score' : scores'
            calc fs
        [] -> do
            scores <- getPast
            case lastFrame game of
                Nothing -> pure scores
                Just frame -> pure . addFinalScore (finalFrameScore frame) $ scores
          where
            finalFrameScore = \case
                LastStrike b1 b2 -> 10 + b1 + b2
                LastSpare  _  b1 -> 10 + b1
                LastFrame  t1 t2 -> t1 + t2
    
    initialState :: (NextThrows, Scores)
    initialState = (case lastFrame game of
        Just (LastStrike b1 _)  -> NextThrows (10, b1)
        Just (LastSpare  t1 _)  -> NextThrows (t1, 10 - t1)
        Just (LastFrame  t1 t2) -> NextThrows (t1, t2)
        Nothing -> NextThrows (0, 0)
        , Scores [0])

    addScore :: Scores -> Frame -> NextThrows -> (Int, NextThrows)
    addScore (Scores pastScores) frame (NextThrows (t1, t2)) = 
        case frame of
            Strike -> (lastScore + 10 + t1 + t2, NextThrows (10, t1))
            Spare n -> (lastScore + 10 + t1, NextThrows (n, 10 - n))
            Frame {firstThrow, secondThrow} -> 
                ( lastScore + firstThrow + secondThrow
                , NextThrows (firstThrow, secondThrow) )
      where
        lastScore = lastOr0 pastScores
    
    addFinalScore :: Int -> Scores -> Scores
    addFinalScore finalFrameScore (Scores scores) = 
        Scores $ finalFrameScore + lastOr0 scores : scores