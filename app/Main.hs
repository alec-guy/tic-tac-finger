{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types 
import GameLogic
-----------------------------------------
import Control.Monad (forever, when, void, )
import Control.Applicative ((<|>))
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
import qualified Data.ByteString as BS 
import Network.Wai.Middleware.Static (staticPolicy, addBase, static)

initialGame :: Rows 
initialGame = Rows 
            { rowOne   = rowToJSONRow emptyRow 
            , rowTwo   = rowToJSONRow emptyRow 
            , rowThree = rowToJSONRow emptyRow
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
        
        post "/runUser" $ do
            bodyText <- Web.Scotty.catch (TE.decodeUtf8 <$> BS.toStrict <$> body) $ \e -> do 
                        liftIO $ putStrLn (show $ (e :: ScottyException)) 
                        finish
            liftIO $ TIO.putStrLn bodyText 
            liftIO $ putStrLn "hello world"
            req <- Web.Scotty.catch (jsonData :: ActionM ServerRequest)  $ \e -> do 
                   liftIO $ putStrLn (show $ (e :: ScottyException))
                   finish
            let game = tictactoeReq req 
            json $ ServerResponse{win = show $ winCond game, tictactoe = game}
        
        
        post "/runNPC" $ do
            bodyText <- Web.Scotty.catch (TE.decodeUtf8 <$> BS.toStrict <$> body) $ \e -> do 
                        liftIO $ putStrLn (show $ (e :: ScottyException)) 
                        finish
            liftIO $ TIO.putStrLn bodyText 
            liftIO $ putStrLn "hello world"
            req <- Web.Scotty.catch (jsonData :: ActionM ServerRequest)  $ \e -> do 
                   liftIO $ putStrLn (show $ (e :: ScottyException))
                   finish
            let game = tictactoeReq req 
                mark = npcMark req 
            maybeFreeSpots     <- return $ getFreeSpots game 
            case maybeFreeSpots of 
             Nothing -> return ()
             (Just freeSpots) -> do 
                                  shuffledSpots <- shuffle freeSpots 
                                  let row     = toChoice $ fst $ Prelude.head $ shuffledSpots
                                      column  = toChoice $ snd $ Prelude.head $ shuffledSpots
                                      newGame = markGame (fromJust $ stringToMark mark) row column game 
                                  json $ ServerResponse{win = show $ winCond newGame, tictactoe = newGame}
        


                                  

-- AI came up with this absolutley everything else is 
-- my work with 0 AI input
-- I was stuck here because I did not know how 
-- how to shuffle a list or get a random element. 
-- but they made it return an IO monad but I made it return ActionM monad
shuffle :: [a] -> ActionM [a]
shuffle [] = return []
shuffle xs = do
    i <- randomRIO (0, length xs - 1)
    let (left, (x:right)) = splitAt i xs
    rest <- shuffle (left ++ right)
    return (x:rest)

getFreeSpots :: Rows -> Maybe [(Int, Int)]
getFreeSpots t = do 
    let firstRow  = rowOne t 
        secondRow = rowTwo t 
        thirdRow  = rowThree t 
    spots <- return $ filter (/= Nothing) [ getSpot 1  $ jsonRowToRow firstRow
                                          , getSpot 2  $ jsonRowToRow secondRow
                                          , getSpot 3  $ jsonRowToRow thirdRow
                                          ] 
    case spots of 
        []       ->  Nothing 
        _        ->  Just $ fromJust <$> spots
    where getSpot :: Int -> Row -> Maybe (Int, Int)
          getSpot i (Row (_,_,Nothing))    = Just (i,3)
          getSpot i (Row (_, Nothing , _)) = Just (i,2)
          getSpot i (Row (Nothing, _ , _)) = Just (i,1)
          getSpot _  _                     = Nothing
