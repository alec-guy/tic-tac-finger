module Main where

import Types 
import Control.Applicative (Alternative, asum) 
import Control.Monad (join, when)
import Network.Socket
import Network.HTTP
import qualified Data.List as List
import System.Exit 
import Control.Exception 
import Data.Maybe (fromJust)
import Data.Random 

initialGame :: TicTacToe 
initialGame = Rows 
            { rowOne   = emptyRow 
            , rowTwo   = emptyRow 
            , rowThree = emptyRow
            }

emptyRow :: Row 
emptyRow = (Nothing, Nothing, Nothing)

markRow :: Mark -> Choice -> Row -> Row 
markRow shape choice (x, y, z) = 
    case choice of 
        First -> case x of 
                  Nothing -> (Just shape, y , z)
                  _       -> (x, y, z)
        Second -> case y of 
                   Nothing -> (x, Just shape, z)
                   _       -> (x, y, z)
        Third -> case z of
                  Nothing -> (x, y, Just shape)
                  _       -> (x, y, z)


winCondTwo :: TicTacToe -> Maybe Mark 
winCondTwo t = do 
    asum [winCondOne $ rowOne t, winCondOne $ rowTwo t, winCondOne $ rowThree t]
    where winCondOne :: Row -> Maybe Mark
          winCondOne r = 
            case r of 
             (Just Circle, Just Circle, Just Circle) -> Just Circle 
             (Just X, Just X ,Just X)                -> Just Circle
             _                                       -> Nothing 


winCondThree :: TicTacToe -> Maybe Mark 
winCondThree t = 
    case (rowOne t, rowTwo t, rowThree t) of 
        ((Just Circle,_,_), (Just Circle,_,_), (Just Circle,_,_)) ->  Just Circle 
        ((_,Just Circle,_), (_,Just Circle,_), (_,Just Circle,_)) ->  Just Circle 
        ((_,_,Just Circle), (_,_,Just Circle), (_,_,Just Circle)) ->  Just Circle 
        ((Just Circle,_,_), (_,Just Circle,_), (_,_,Just Circle)) ->  Just Circle 
        ((Just X,_,_),(Just X,_,_),(Just X,_,_))                  ->  Just X
        ((_,Just X,_),(_,Just X,_),(_,Just X,_))                  ->  Just X
        ((_,_,Just X),(_,_,Just X),(_,_,Just X))                  ->  Just X
        ((Just X,_,_),(_,Just X,_),(_,_,Just X))                  ->  Just X 
        _                                          ->  Nothing 



winCond :: TicTacToe -> Maybe Mark 
winCond t = asum [winCondTwo t, winCondThree t]


opposite :: Mark -> Mark 
opposite Circle = X 
opposite X = Circle 

markGame :: Mark -> Choice -> Choice -> TicTacToe -> TicTacToe 
markGame shape rowChoice spotChoice game =  
         case rowChoice of 
            First  -> game {rowOne = markRow shape spotChoice (rowOne game)}
            Second -> game {rowTwo = markRow shape spotChoice (rowTwo game)}
            Third  -> game {rowThree = markRow shape spotChoice (rowThree game)}

toChoice :: Int -> Choice 
toChoice 1 = First 
toChoice 2 = Second 
toChoice 3 = Third 


main :: IO ()
main = runGame False X initialGame  

runGame :: Bool -> Mark -> TicTacToe -> IO ()
runGame win playerShape tictac = do
    when (win /= True) $ do 
        freeSpots   <- do 
                        case getFreeSpots tictac of 
                         Nothing    -> exitFailure 
                         Just spots -> return spots

        randomIndex <- getStdRandom (randomR (0 :: Int, length freeSpots)) 
        (row, spot) <- return $ freeSpots !! randomIndex
        t   <- return $ markGame (opposite playerShape) (toChoice row) (toChoice spot) tictac  -- NPC Playing against you
        maybeW  <- return $ winCond t 
        case maybeW of 
         Nothing -> do 
                     putStrLn $ show t 
         (Just m) -> do 
                      putStrLn $ (show m) ++ "is the winner"
                      putStrLn $ show t
                      runGame True playerShape t 
        do 
         runGame win playerShape t 
    putStrLn "Would you like to play again?"
    choice <- getLine 
    putStrLn "Well too bad."
    return ()


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
