module Main where

import Types 
import System.Random
import Control.Applicative (Alternative, asum) 
import Control.Monad (when)
import Network.Socket
import Network.HTTP
import qualified Data.List as List

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

initialPlayer :: Player
initialPlayer = Player {movesMade = []}

updateMovesMade :: (RowChosen, SpotTaken) -> Player -> Player 
updateMovesMade t p = p {movesMade = t : (movesMade p)}

main :: IO ()
main = do 
    let user = initialPlayer
        npc  = initialPlayer
    runGame (Just Circle) X initialGame user npc 

runGame :: Maybe Mark -> Mark -> TicTacToe -> Player -> Player -> IO ()
runGame shape playerShape tictac user npc = 
    when (shape /= Nothing) $ do 
        randomSpot  <- getStdRandom (randomR (1 :: Int, 3 :: Int)) 
        randomRow   <- getStdRandom (randomR (1 :: Int, 3 :: Int))   
        (row, spot) <- getSpotAndRow (movesMade npc) randomSpot randomRow 
        newNPC <-   return $ updateMovesMade (row, spot) npc
        t   <- return $ markGame (opposite playerShape) (toChoice row) (toChoice spot) tictac  -- NPC Playing against you
        maybeW  <- return $ winCond t 
        case maybeW of 
         Nothing -> do 
                     putStrLn $ show t 
         (Just m) -> do 
                      putStrLn $ (show m) ++ "is the winner"
                      putStrLn $ show t
                      runGame Nothing playerShape t user newNPC
        do 
         runGame shape playerShape t user newNPC


getSpotsFree :: TicTacToe -> [(Int, Int)]
getSpotAndRow :: [(RowChosen, SpotTaken)] -> SpotTaken -> RowChosen -> IO (RowChosen, SpotTaken)
getSpotAndRow choicesMade spot row = do 
        case List.lookup row choicesMade of 
            Nothing -> return (row, spot)
            _       -> do 
                        putStrLn "Calling this"
                        randomSpot  <- getStdRandom (randomR (1 :: Int, 3 :: Int)) 
                        randomRow   <- getStdRandom (randomR (1 :: Int, 3 :: Int))
                        getSpotAndRow choicesMade randomSpot randomRow  