module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Dict exposing (Dict)


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Mark = X | Circle 
oppositeMark : Maybe Mark ->Maybe Mark 
oppositeMark m = case m of 
                  Nothing       -> Nothing 
                  (Just Circle) -> Just X
                  (Just X)      -> Just Circle

init : () -> (Model, Cmd Msg)
init _ = (initialModel , Cmd.none)

initialModel : Model 
initialModel = { mark = Nothing
               , userGoesFirst = Nothing
               , rows = [GridItem Nothing ("rowOne", "first")
                        ,GridItem Nothing ("rowOne", "second")
                        ,GridItem Nothing ("rowOne", "third")
                        ,GridItem Nothing ("rowTwo", "first")
                        ,GridItem Nothing ("rowTwo","second")
                        ,GridItem Nothing ("rowTwo", "third")
                        ,GridItem Nothing ("rowThree", "first")
                        ,GridItem Nothing ("rowThree","second")
                        ,GridItem Nothing ("rowThree", "third")
                        ]
               , recentlyClicked = Nothing
               , httpError = Nothing 
               , win       = Nothing 
               }
  
type GridItem = GridItem (Maybe Mark) (String, String)

gridItemToRow : GridItem -> (String,String)
gridItemToRow g = case g of 
                   (GridItem Nothing (_ , s))     -> ("Nothing", s)
                   (GridItem (Just Circle) (_,s)) -> ("Circle", s)
                   (GridItem _ (_,s))             -> ("X",s)
gridItemsToRows : List GridItem -> Rows 
gridItemsToRows l = 
                  { rowOne   = (List.map gridItemToRow) <| (List.filter (\(GridItem _ (r, _)) -> r == "rowOne") l) 
                  , rowTwo   = (List.map gridItemToRow) <| (List.filter (\(GridItem _ (r, _)) -> r == "rowTwo") l) 
                  , rowThree = (List.map gridItemToRow) <| (List.filter (\(GridItem _ (r, _)) -> r == "rowOne") l) 
                  }
type ProtoGridItem = ProtoGridItem (Maybe Mark) (Maybe String, String)

rowToGridItem : (String, String) -> ProtoGridItem
rowToGridItem t = case t of 
                   ("Nothing", s) ->  ProtoGridItem Nothing        (Nothing, s)
                   ("Circle", s)  ->  ProtoGridItem (Just Circle)  (Nothing, s)
                   ("X", s)       ->  ProtoGridItem (Just X)       (Nothing, s)
                   _              ->  ProtoGridItem Nothing        (Nothing, "")

rowsToGridItems : Rows -> List GridItem 
rowsToGridItems r = 
    let protoOne   = (List.map rowToGridItem r.rowOne) 
        protoTwo   = (List.map rowToGridItem r.rowTwo) 
        protoThree = (List.map rowToGridItem r.rowThree)
        f          = (\(ProtoGridItem m (_,s)) -> GridItem m ("rowOne", s))
        g          = (\(ProtoGridItem m (_,s)) -> GridItem m ("rowTwo", s))
        h          = (\(ProtoGridItem m (_,s)) -> GridItem m ("rowThree", s))
    in (List.map f protoOne) ++ (List.map g protoTwo) ++ (List.map h protoThree)


getMark : GridItem -> Maybe Mark 
getMark (GridItem m _ ) = m

getGridItem : List GridItem -> GridItem -> Maybe GridItem 
getGridItem l i = case List.filter (\item -> item == i) l of 
                   [x] -> Just x
                   _   -> Nothing
hasSameRowAndC : GridItem -> GridItem -> Bool 
hasSameRowAndC (GridItem _ t) (GridItem _ tPrime) = t == tPrime
insertGridItem : List GridItem -> GridItem -> List GridItem 
insertGridItem l i = List.map (\item -> if hasSameRowAndC i item then i else item) l

------------------------------------------------------

type alias Model = 
                 { mark            : Maybe Mark
                 , userGoesFirst   : Maybe Bool
                 , rows            : List GridItem
                 , recentlyClicked : Maybe GridItem
                 , httpError       : Maybe Http.Error
                 , win             : Maybe String 
                 }
------------------------------------------------
-- UPDATE
type Msg = ClickedMe GridItem--(Row,Cell)
         | GoesFirst (Maybe Bool)
         | Chose Mark 
         | GotServerResponse (Result Http.Error ServerResponse)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
 case msg of 
  (Chose m)     -> ({model | mark = Just m}, Cmd.none)
  (GoesFirst m) -> ({model | userGoesFirst = m}, Cmd.none)
  _         -> case model.userGoesFirst of 
                Nothing      -> (model,Cmd.none)
                (Just True)  -> case msg of  
                                 (GotServerResponse result) -> case result of 
                                                                (Err e)  -> ({model | httpError = Just e}, Cmd.none)
                                                                (Ok r)   -> ({model | win = Just r.win
                                                                             , rows = rowsToGridItems r.tictactoe
                                                                             }
                                                                            , Cmd.none
                                                                            )
                                 (ClickedMe gridit) -> ({model | rows = insertGridItem model.rows gridit
                                                        ,recentlyClicked = Just gridit
                                                        }
                                                       , runUser 
                                                         (gridItemsToRows model.rows) 
                                                         (showMaybeMark <| oppositeMark <| model.mark)
                                                       )
                                 _                  -> (model
                                                       ,runNPC 
                                                        (gridItemsToRows model.rows) 
                                                        (showMaybeMark <| oppositeMark <| model.mark)
                                                       )
                                
                                
                (Just False) ->  case msg of 
                                  (GotServerResponse result) -> case result of 
                                                                 (Err e)  -> ({model | httpError = Just e}, Cmd.none)
                                                                 (Ok r)   -> ({model | win = Just r.win
                                                                              , rows = rowsToGridItems r.tictactoe
                                                                              }
                                                                             , Cmd.none
                                                                             )
                                  _                          -> ({model | userGoesFirst = Just True}
                                                                 ,runNPC 
                                                                  (gridItemsToRows model.rows) 
                                                                  (showMaybeMark <| oppositeMark <| model.mark)
                                                                ) 
 
showMaybeMark : Maybe Mark -> String 
showMaybeMark m = case m of 
                   Nothing       -> "Nothing"
                   (Just Circle) -> "Circle"
                   (Just X)      -> "X"
-------------------------------------------------
--- VIEW 
view : Model -> Html Msg 
view model = 
   case model.userGoesFirst of 
    Nothing  -> goesFirstMenu 
    (Just b) -> case model.mark of 
                 Nothing -> chooseShape 
                 _       -> div 
                            [] 
                            [h1 [] [text "tic-tac-finger"]
                            ,grid model.rows model
                            ]
chooseShape : Html Msg 
chooseShape = div 
            [] 
            [h1 [] [text "choose shape"]
            ,button [onClick <| Chose Circle] [text "Circle"]
            ,button [onClick <| Chose X]      [text "X"]
            ]
goesFirstMenu : Html Msg 
goesFirstMenu = div 
                [] 
                [
                  h1 [] [text "TIC TAC FINGER"]
                , button [onClick <| GoesFirst <| Just True] [text "MAKE FIRST MOVE"]
                , button [onClick <| GoesFirst <| Just False] [text "LET NPC MAKE FIRST MOVE"]
                ]
grid : List GridItem -> Model -> Html Msg 
grid l m = div
         [ style "display" "grid"
         , style "grid-template-columns" "repeat(3, 1fr)"
         , style "grid-gap" "10px"
         , style "width" "300px"
         , style "margin" "20px auto"
         ]
         (List.map (displayGridItem m) l) 
               

                
               
displayGridItem : Model -> GridItem -> Html Msg 
displayGridItem m g = 
                    div 
                    [ class "grid-item"
                    , style "border" "1px solid black"
                    , style "height" "100px"
                    , style "display" "flex"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "font-size" "24px"
                    ]
                    [ button 
                    [onDoubleClick <| ClickedMe <| g]
                    [ case m.recentlyClicked of 
                       Nothing -> case g of 
                                    (GridItem Nothing _)        -> text ""
                                    (GridItem (Just Circle) _ ) -> text "O"
                                    (GridItem (Just X) _ )      -> text "X"
                               
                                   
                       (Just gPrime) -> case g == gPrime of 
                                         True -> case gPrime of 
                                                  (GridItem Nothing _)        -> text <| showMaybeMark m.mark
                                                  (GridItem (Just Circle) _ ) -> text "O"
                                                  (GridItem (Just X) _ )      -> text "X"
                                         False -> case g of 
                                                   (GridItem Nothing _)        -> text ""
                                                   (GridItem (Just Circle) _ ) -> text "O"
                                                   (GridItem (Just X) _ )      -> text "X"
                    ]
                  ]

-------------------------------------------------------
--- HTTP

type alias ServerRequest = 
             {tictactoeReq : Rows 
             ,npcMark      : String 
             }

type alias ServerResponse  = 
       { win : String      -- > Maybe String -- > Maybe Mark 
       , tictactoe : Rows
       }
type alias Rows = 
          {rowOne   :   List (String, String)
          ,rowTwo   :   List (String, String)
          ,rowThree :   List (String,String)
          }
    

-- Update your runUser function to use this encoder
runUser : Rows -> String -> Cmd Msg
runUser rows npcMark =
    Http.post
        { url = "/runUser"
        , body = Http.jsonBody <| 
                Encode.object
                [ ("tictactoeReq", encodeRows rows)
                , ("npcMark", Encode.string npcMark)
                ]
        , expect = Http.expectJson GotServerResponse responseDecoder
        }
encodeRows : Rows -> Encode.Value 
encodeRows rows = 
    Encode.object 
        [("rowOne", encodeRowList rows.rowOne)
        ,("rowTwo", encodeRowList rows.rowTwo)
        ,("rowThree",encodeRowList rows.rowThree)
        ]
encodeRowList : List (String, String) -> Encode.Value
encodeRowList rowList =
    Encode.list 
        (\(mark, cell) -> 
            Encode.object 
                [ (mark, Encode.string cell)
                , (mark, Encode.string cell)
                ]
        ) 
        rowList

runNPC : Rows -> String -> Cmd Msg 
runNPC rows npcMark = 
 Http.post 
   { url = "/runNPC"
   , body = Http.jsonBody <| 
            Encode.object
            [ ("tictactoeReq", encodeRows rows)
            , ("npcMark", Encode.string npcMark)
            ]
   , expect = Http.expectJson GotServerResponse responseDecoder 
   }


responseDecoder : Decoder ServerResponse 
responseDecoder =
  map2 ServerResponse
    (field "win" Decode.string)
    (field "tictactoe" rowsDecoder)
rowsDecoder : Decoder Rows 
rowsDecoder = 
  map3 Rows 
   (field "rowOne" (keyValuePairs Decode.string))
   (field "rowTwo" (keyValuePairs Decode.string))
   (field "rowThree" (keyValuePairs Decode.string))
 
-----------------
subscriptions : Model -> Sub Msg 
subscriptions _ = Sub.none