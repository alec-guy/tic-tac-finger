{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types 
import GameLogic
-----------------------------------------
import Control.Monad (forever, when, void)
import qualified Data.List as List
import System.Exit 
import Control.Exception 
import Data.Maybe (fromJust)
import System.Random 
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.IO as TIO
import Data.Aeson 
import GHC.Generics 
import Web.Scotty
import qualified Control.Monad.State as State

initialGame :: TicTacToe 
initialGame = Rows 
            { rowOne   = emptyRow 
            , rowTwo   = emptyRow 
            , rowThree = emptyRow
            }
emptyRow :: Row 
emptyRow = Row (Nothing, Nothing, Nothing)

toChoice :: Int -> Choice 
toChoice 1 = First 
toChoice 2 = Second 
toChoice 3 = Third 

opposite :: Mark -> Mark 
opposite Circle = X 
opposite X = Circle 

stringToMark :: String -> Maybe Mark 
stringToMark "Circle" = Just Circle 
stringToMark "X"      = Just X 
stringToMark _        = Nothing 


main :: IO ()
main = do 
    homepage <- TIO.readFile "frontend/index.html"
    scotty 8000 $ do 
        get "/" (html $ LazyText.fromStrict homepage) 
        get "/runUser" $ do
            req <- jsonData :: ActionM ServerRequest
            let game = tictactoeReq req 
            json $ ServerResponse{win = show $ winCond game, tictactoe = game}

        get "/runNPC" $ do
            req <- jsonData :: ActionM ServerRequest
            let game = tictactoeReq req 
                mark = npcMark req 
            freeSpots <- return $ getFreeSpots game 
            case freeSpots of 
                Nothing     -> return ()
                (Just spot) -> do 
                                let row     = toChoice $ fst $ Prelude.head $ spot 
                                    column  = toChoice $ snd $ Prelude.head $ spot
                                    newGame = markGame (fromJust $ stringToMark mark) row column game 
                                json $ ServerResponse{win = show $ winCond newGame, tictactoe = newGame}

                                  

-- AI came up with this absolutley everything else is 
-- my work with 0 AI input
-- I was stuck here because I did not know how 
-- how to shuffle a list or get a random element. 
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    i <- randomRIO (0, length xs - 1)
    let (left, (x:right)) = splitAt i xs
    rest <- shuffle (left ++ right)
    return (x:rest)

getFreeSpots :: TicTacToe -> Maybe [(Int, Int)]
getFreeSpots t = do 
    let firstRow  = rowOne t 
        secondRow = rowTwo t 
        thirdRow  = rowThree t 
    spots <- return $ filter (/= Nothing) [getSpot 1 firstRow, getSpot 2 secondRow, getSpot 3 thirdRow] 
    case spots of 
        []       ->  Nothing 
        _        ->  Just $ fromJust <$> spots
    where getSpot :: Int -> Row -> Maybe (Int, Int)
          getSpot i (Row (_,_,Nothing))    = Just (i,3)
          getSpot i (Row (_, Nothing , _)) = Just (i,2)
          getSpot i (Row (Nothing, _ , _)) = Just (i,1)
          getSpot _  _                     = Nothing
