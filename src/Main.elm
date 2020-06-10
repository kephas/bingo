module Main exposing (..)

import Browser
import Element exposing (Element, centerX, column, el, layout, padding, rgb255, row, spacing, text)
import Element.Border as Brd
import Element.Font as Font
import Element.Region exposing (heading)
import Html exposing (Html)
import Html.Attributes exposing (src)
import List exposing (drop, take)



---- MODEL ----


type alias BingoBoard =
    { title : String
    , cells : List String
    }


type alias Model =
    { board : BingoBoard }


bingoFeministe =
    { title = "Bingo féministe"
    , cells =
        [ "Faut souffrir pour être belle"
        , "Vous desservez la cause"
        , "T'es juste mal baisée"
        , "C'est du sexisme à l'envers"
        , "Les poils, c'est dégeulasse"
        , "On peut plus rien dire"
        , "Elle l'avait quand même bien cherché"
        , "C'est la théorie du djendeur"
        , "Les femmes sont fragiles"
        ]
    }


fakeBoard =
    { title = "Un bingo de test"
    , cells = List.range 1 25 |> List.map String.fromInt
    }


init : ( Model, Cmd Msg )
init =
    ( { board = bingoFeministe }, Cmd.none )


estEntier : Float -> Bool
estEntier nombre =
    (abs <| nombre - (toFloat <| round nombre)) < 0.001


decoupeListe : Int -> List a -> List (List a)
decoupeListe taille elements =
    case elements of
        [] ->
            []

        _ ->
            take taille elements :: (decoupeListe taille <| drop taille elements)


squareSplit : List a -> Maybe (List (List a))
squareSplit cells =
    let
        racine =
            List.length cells
                |> toFloat
                |> sqrt

        tailleLigne =
            truncate racine
    in
    if estEntier racine then
        Just <| decoupeListe tailleLigne cells

    else
        Nothing



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


black =
    rgb255 0 0 0


viewCell : String -> Element msg
viewCell cell =
    el [ Brd.color black, Brd.width 1, padding 20 ] <| text cell


viewRow oneRow =
    oneRow
        |> List.map viewCell
        |> row [ centerX, spacing 40 ]


viewRows : Maybe (List (List String)) -> List (Element msg)
viewRows rows =
    case rows of
        Nothing ->
            []

        Just someRows ->
            someRows |> List.map viewRow


view : Model -> Html Msg
view model =
    layout [] <|
        column [ centerX, spacing 40, padding 40 ] <|
            [ el [ heading 1, Font.size 32 ] <| text model.board.title ]
                ++ (model.board.cells
                        |> squareSplit
                        |> viewRows
                   )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
