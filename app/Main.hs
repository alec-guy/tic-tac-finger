module Main where

import Types 
import GameLogic
-----------------------------------------
import Control.Monad (forever)
import qualified Data.List as List
import System.Exit 
import Control.Exception 
import Data.Maybe (fromJust)
import System.Random 
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson 



initialGame :: TicTacToe 
initialGame = Rows 
            { rowOne   = emptyRow 
            , rowTwo   = emptyRow 
            , rowThree = emptyRow
            }

emptyRow :: Row 
emptyRow = (Nothing, Nothing, Nothing)

toChoice :: Int -> Choice 
toChoice 1 = First 
toChoice 2 = Second 
toChoice 3 = Third 

opposite :: Mark -> Mark 
opposite Circle = X 
opposite X = Circle 


main :: IO ()
main = do 
    homepage <- TIO.readFile "frontend/index.html"
    scotty "8000" $ do 
        get "/" (html homepage)
        get "/runNPC"  $  do
            
        get "/runUser" $ do 
        
        
    runNPC False X initialGame  

runUser :: Mark -> (Choice, Choice) -> TicTacToe -> IO (TicTacToe, Maybe Mark )
runUser win shape (rowChoice, spotChoice) tictac = do 
    t <- return $ markGame shape rowChoice spotChoice tictac 
    maybeW <- return $ winCond t 
    return (t, maybeW)
    
runNPC :: Mark -> TicTacToe -> IO (TicTacToe, Maybe Mark)
runNPC shape tictac = do 
        freeSpots   <- do 
                        case getFreeSpots tictac of 
                         Nothing    -> exitFailure 
                         Just spots -> return spots
        shuffledSpots <- shuffle freeSpots 
        (row, spot)   <- return $ head shuffledSpots
        t   <- return $ markGame shape (toChoice row) (toChoice spot) tictac  -- NPC Playing against you
        maybeW  <- return $ winCond t 
        return (t, maybeW)



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
          getSpot i (_,_,Nothing)    = Just (i,3)
          getSpot i (_, Nothing , _) = Just (i,2)
          getSpot i (Nothing, _ , _) = Just (i,1)
          getSpot _  _               = Nothing
