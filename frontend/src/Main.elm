import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

init : () -> (Model, Cmd Msg)
init _ = (initialModel , Cmd.none)

type alias Model = 
                 { mark          : Maybe Mark
                 , userGoesFirst : Maybe Bool
                 , takenSpots    : [(Int,Int)] -- (Row,Cell)
                 }
type Mark = X | Circle 
oppositeMark : Mark -> Mark 
oppositeMark m = if m == X then Circle else X
type Msg = EnteredShape String 
         | ClickedMe (Int,Int) --(Row,Cell)
         | GoesFirst Bool 
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
   case msg of 
    (EnteredShape s) -> 
    (ClickedMe t)    -> ({model | takenSpots = t :: (takenSpots model)} ,runUser)
    (GoesFirst b)    -> case b of 
                         True ->  ({model | userGoesFirst = Just b}, Cmd.none)
                         False -> ({model | userGoesFirst = Just b}, runNPC)
                
    
view : Model -> Html Msg 
view model = 
   case model.userGoesFirst of 
    Nothing  -> goesFirstMenu 
    (Just b) -> grid model.mark
goesFirstMenu : Html Msg 
goesFirstMenu = div 
                [] 
                [
                  h1 [] [text "TIC TAC FINGER"]
                  button [onClick <| GoesFirst True] [text "MAKE FIRST MOVE"]
                  button [onClick <| GoesFirst False] [text "LET NPC MAKE FIRST MOVE"]
                ]
grid : Maybe Mark -> Html Msg 
grid maybeMark = div
               [class "grid-container"]
               [gridItem (1,1)
               ,gridItem (1,2)
               ,gridItem (1,3)
               ,gridItem (2,1)
               ,gridItem (2,2)
               ,gridItem (2,3)
               ,gridItem (3,1)
               ,gridItem (3,2)
               ,gridItem (3,3)
               ]
               
gridItem : Maybe Mark -> (Int, Int) -> Html Msg 
gridItem maybeMark (row,cell ) = 
                        div 
                        [class "grid-item"
                        ,onDoubleClick <| ClickedMe (row,cell)
                        ]
                        [case maybeMark of 
                          Nothing     -> text ""
                          (Just mark) -> showMark mark
                        ]
showMark : Mark -> Html Msg 
showMark mark = if mark == X then text "X" else text "Circle"



runUser : Cmd Msg
runUser =
  Http.post
    { url    = "/runUser"
    , body   = 
    , expect = Http.expectJson TicTacToeState quoteDecoder
    }

runNPC : Cmd Msg 
runNPC = 
 Http.post 
   { url = "/runNPC"
   , body = 
   , expect = Http.expectJson TicTacToeState
   }
type alias TicTacToeState = 
       { userMove : (Int,Int)
       , npcMove : (Int,Int)
       , win      : Maybe Mark 
       }
quoteDecoder : Decoder Quote
quoteDecoder =
  map4 Quote
    (field "quote" string)
    (field "source" string)
    (field "author" string)
    (field "year" int)