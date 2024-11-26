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

stringToMark :: String -> Maybe Mark 
stringToMark "Circle" = Just Circle 
stringToMark "X"      = Just X 
stringToMark _        = Nothing 

data TicTacToeState = TicTacToeState 
                    { userMove     :: (Int, Int)
                    , userShape    :: String 
                    , npcMove      :: (Int,Int)
                    , winner       :: String     -- > Maybe String  -- >  Maybe Mark 
                    } deriving (Generic, Show)
                    
instance ToJSON TicTacToeState where 
instance FromJSON TicTacToeState where 

data GameLoopArgs = GameLoopArgs 
                  { choice :: (Maybe (Choice, Choice) )
                  , win :: (Maybe Mark) 
                  , game :: TicTacToe
                  }
-- I FUCKED UP REALLY BADLY BUT TOMMOROW I WILL REDEEM MYSELF HOPEFULLY 
-- DO NOT LAUGH AT MY MISTAKES 
main :: IO ()
main = do 
    homepage <- TIO.readFile "frontend/index.html"
    scotty 8000 $ do 
        get "/" (html $ LazyText.fromStrict homepage) 
        void $ return $ runGame $ Left $ GameLoopArgs {choice = Nothing, win = Nothing, game = initialGame} 


runGame :: (Either GameLoopArgs ()) -> ActionM (Either GameLoopArgs ())
runGame gameLoopArgs = 
    case gameLoopArgs of 
        Left g -> do 
            reqBody   <- body 
            maybeTics <- return $ (decode reqBody :: Maybe TicTacToeState)
            case maybeTics of 
             Nothing  -> return $ Right ()
             (Just t) -> do 
                          let shape    = userShape t 
                          case stringToMark shape of 
                            Nothing  -> return ()
                            (Just m) -> do 
                                         (choice0, maybeW, game0) <- runNPC m (game g)
                                         jsonData $ jsonNPCData m (choice0,maybeW)
                                         runGame $ GameLoopArgs {choice = choice0, win = maybeW, game = game0}

            reqBody     <- body 
            maybeTics   <- return $ (decode reqBody :: Maybe TicTacToeState)
            case maybeTics of 
            Nothing  -> return ()
            (Just t) -> do 
                             let rowChoice  = toChoice $ fst $ userMove t 
                                 cellChoice = toChoice $ snd $ userMove t 
                             case userShape t of 
                              "Circle" -> do 
                                           (choice0, maybeW, game0)  <- runUser Circle (rowChoice,cellChoice) (game g)
                                           jsonData $ jsonUserData "Circle" (choice0,maybeW)
                                           runGame $ GameLoopArgs {choice = choice0, win = maybeW, game = game0}


                              "X"      -> do 
                                           (choice0,maybeW,game0) <- runUser X (rowChoice,cellChoice) (game g)
                                           jsonData $ jsonUserData "X"
                                           runGame $ GameLoopArgs {choice = choice0, win = maybeW, game = game0}
        Right _ -> return $ Right ()
jsonNPCData :: Mark -> (Maybe (Int,Int), Maybe Mark)  -> TicTacToeState
jsonNPCData m (choice,maybeW) = TicTacToeState 
                              {userMove   = (-1,-1)
                              ,userShape  = show $ opposite $ m 
                              ,npcMove    = fromJust choice
                              ,winner     = show maybeW
                              }
jsonUserData :: String -> (Maybe (Int,Int), Maybe Mark) -> TicTacToeState
jsonUserData s (choice,maybeW) = TicTacToeState 
                               { userMove  = fromJust choice 
                               , userShape = "Circle"
                               , npcMove   = (-1, -1)
                               , winner    = show maybeW
                               }   

runUser :: Mark -> (Choice, Choice) -> TicTacToe -> ActionM GameLoopArgs
runUser win shape (rowChoice, spotChoice) tictac = do 
    t <- return $ markGame shape rowChoice spotChoice tictac 
    maybeW <- return $ winCond t 
    return $ GameLoopArgs
           {choice = Nothing 
           ,win    = maybeW 
           ,game   = t 
           }

    
runNPC :: Mark -> TicTacToe -> ActionM GameLoopArgs
runNPC shape tictac = do 
        freeSpots   <- do 
                        case getFreeSpots tictac of 
                         Nothing    -> exitFailure 
                         Just spots -> return spots
        shuffledSpots <- shuffle freeSpots 
        (row, spot)   <- return $ head shuffledSpots
        t   <- return $ markGame shape (toChoice row) (toChoice spot) tictac  -- NPC Playing against you
        maybeW  <- return $ winCond t 
        return $ GameLoopArgs
               {choice = Just (row,spot)
               ,win    = maybeW 
               ,game   = t 
               }



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
