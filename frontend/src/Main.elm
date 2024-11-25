import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map4, field, int, string)



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

type alias Model = {shape : Mark}
type Mark = X | Circle 
oppositeMark : Mark -> Mark 
oppositeMark m = if m == X then Circle else X
type Msg = EnteredShape String 
         | 
update : Msg -> Model -> (Model, Cmd Msg)
view : Model -> Html Msg 
view model = grid 

grid : Html Msg 
grid = div
       [class "grid-container"]
       [ gridItem maybeMark
       , gridItem maybeMark
       , gridItem maybeMark
       , gridItem maybeMark
       , gridItem maybeMark
       , gridItem maybeMark
       , gridItem maybeMark
       , gridItem maybeMark
       , gridItem maybeMark
       ]
gridItem1 : Maybe Mark -> Html Msg 
gridItem maybeMark = div 
                     [class "grid-item"]
                     [case maybeMark of 
                       Nothing     -> text ""
                       (Just mark) -> showMark mark
                     ]
showMark : Mark -> Html Msg 