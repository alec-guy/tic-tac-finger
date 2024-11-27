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
import GHC.Generics 

-- Data Types
data ServerResponse = ServerResponse 
                    { win :: String 
                    , tictactoe :: Rows
                    } deriving (Eq, Show, Generic)

instance ToJSON ServerResponse where
instance FromJSON ServerResponse where

data ServerRequest = ServerRequest 
                   {tictactoeReq :: Rows
                   ,npcMark :: String
                   }
                   deriving (Eq, Show, Generic)
instance FromJSON ServerRequest 

data Rows = Rows 
               { rowOne   :: JSONRow
               , rowTwo   :: JSONRow
               , rowThree :: JSONRow 
               } deriving (Eq, Show, Generic)
instance ToJSON Rows where 
instance FromJSON Rows where 

data JSONRow = JSONRow 
             { first :: Maybe Mark
             , second :: Maybe Mark
             , third :: Maybe Mark
             } deriving (Show, Eq, Generic)

newtype Row = Row (Maybe Mark, Maybe Mark , Maybe Mark)

rowToJSONRow (Row (m1, m2, m3)) = JSONRow {first = m1, second = m2, third = m3}
jsonRowToRow (JSONRow {first    = m1,second=m2,third=m3}) = Row(m1,m2,m3)

instance ToJSON JSONRow where 
instance FromJSON JSONRow where 

data Mark = X 
          | Circle 
          deriving (Eq, Show, Generic)
instance ToJSON Mark where 
instance FromJSON Mark where 

data Choice = First 
            | Second 
            | Third 
            deriving (Eq, Show, Enum, Ord)


{-
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

-- ToJSON instance for Rows
instance ToJSON Rows where 
    toJSON :: Rows -> Value 
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
        tictac <- parseJSON $ fromJust $ KM.lookup (fromString "tictactoeReq") m
        mark   <- parseJSON $ fromJust $ KM.lookup (fromString "npcMark") m
        return $ ServerRequest {npcMark = mark , tictactoeReq = tictac}
    parseJSON _ = fail "Expected an object for Rows"

-- FromJSON instance for Rows
instance FromJSON Rows where 
    parseJSON :: Value -> Parser Rows 
    parseJSON (Object v) = do 
        row1 <- v .:? "rowOne"  -- Try to parse "rowOne"
        row2 <- v .:? "rowTwo"  -- Try to parse "rowTwo"
        row3 <- v .:? "rowThree"  -- Try to parse "rowThree"
        return $ Rows {rowOne = fromJust row1, rowTwo = fromJust row2, rowThree = fromJust row3}
    parseJSON _ = fail "Expected an object for Rows"

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
-}