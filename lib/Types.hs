{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import Data.Aeson.Key
import Data.Text
import Data.Maybe (fromJust)

-- Data Types
data ServerResponse = ServerResponse 
                    { win :: String 
                    , tictactoe :: TicTacToe
                    } deriving (Eq, Show)

data ServerRequest = ServerRequest 
                   {tictactoeReq :: TicTacToe
                   ,npcMark :: String
                   }
                   deriving (Eq, Show)

data TicTacToe = Rows 
               { rowOne   :: Row
               , rowTwo   :: Row 
               , rowThree :: Row 
               } deriving (Eq, Show)

newtype Row = Row (Maybe Mark, Maybe Mark , Maybe Mark) deriving (Show, Eq)

data Mark = X 
          | Circle 
          deriving (Eq, Show)

data Choice = First 
            | Second 
            | Third 
            deriving (Eq, Show, Enum, Ord)

-- ToJSON instance for Mark
instance ToJSON Mark where 
    toJSON X      = String "X"
    toJSON Circle = String "Circle"

-- FromJSON instance for Mark
instance FromJSON Mark where
    parseJSON :: Value -> Parser Mark
    parseJSON (String "X")      = return X
    parseJSON (String "Circle") = return Circle
    parseJSON _                = fail "Invalid Mark value"

-- ToJSON instance for ServerResponse
instance ToJSON ServerResponse where 
    toJSON :: ServerResponse -> Value 
    toJSON (ServerResponse {win = w' , tictactoe = t'}) = 
        Object $ KM.fromList [(fromString "win", String $ pack w')
                             ,(fromString "tictactoe", toJSON $ t')
                             ]

-- ToJSON instance for TicTacToe
instance ToJSON TicTacToe where 
    toJSON :: TicTacToe -> Value 
    toJSON (Rows {rowOne = r1, rowTwo = r2, rowThree = r3}) = 
        Object $ KM.fromList 
               [(fromString "rowOne", toJSON r1)
               ,(fromString "rowTwo", toJSON r2)
               ,(fromString "rowThree", toJSON r3)
               ]

-- FromJSON instance for ServerRequest
instance FromJSON ServerRequest where 
    parseJSON :: Value -> Parser ServerRequest 
    parseJSON (Object m) = do 
        mark   <- return $ Prelude.head $ KM.keys m
        tictac <- parseJSON $ fromJust $ KM.lookup mark m
        return $ ServerRequest {npcMark = toString mark , tictactoeReq = tictac}

-- FromJSON instance for TicTacToe
instance FromJSON TicTacToe where 
    parseJSON :: Value -> Parser TicTacToe 
    parseJSON (Object v) = do 
        row1 <- v .:? "rowOne"  -- Try to parse "rowOne"
        row2 <- v .:? "rowTwo"  -- Try to parse "rowTwo"
        row3 <- v .:? "rowThree"  -- Try to parse "rowThree"
        return $ Rows {rowOne = fromJust row1, rowTwo = fromJust row2, rowThree = fromJust row3}

-- ToJSON instance for Row (which is a tuple of Maybe Mark values)
instance ToJSON Row where 
    toJSON :: Row -> Value 
    toJSON (Row (one,two,three)) = 
        Object $ KM.fromList 
               [(fromString "first", String $ pack $ showMaybe one)
               ,(fromString "second", String $ pack $ showMaybe two)
               ,(fromString "third", String $ pack $ showMaybe three)
               ]
               where showMaybe :: Maybe Mark -> String 
                     showMaybe Nothing  = "Nothing"
                     showMaybe (Just m) = show m

-- FromJSON instance for Row
instance FromJSON Row where
    parseJSON :: Value -> Parser Row
    parseJSON (Object v) = do
        one   <- v .:? "first"   -- Try to parse the "first" key as Maybe Mark
        two   <- v .:? "second"  -- Try to parse the "second" key as Maybe Mark
        three <- v .:? "third"   -- Try to parse the "third" key as Maybe Mark
        return $ Row (one, two, three)  -- Return the tuple
    parseJSON _ = fail "Expected an object for Row"  -- If it's not an object, fail
