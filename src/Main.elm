module Main exposing (..)

import Browser
import Element exposing (Element, centerX, column, el, layout, padding, rgb255, row, spacing, text)
import Element.Background as Bck
import Element.Border as Brd
import Element.Events as Ev
import Element.Font as Font
import Element.Region exposing (heading)
import Html exposing (Html)
import Html.Attributes exposing (src)
import List exposing (drop, take)
import List.Extra exposing (getAt, setAt)



---- MODEL ----


type alias Cell =
    { ticked : Bool
    , text : String
    }


type alias BingoBoard =
    { title : String
    , cells : List Cell
    }


type alias Model =
    { board : BingoBoard
    }


toCell string =
    { ticked = False, text = string }


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
            |> List.map toCell
    }


fakeBoard =
    { title = "Un bingo de test"
    , cells = List.range 1 25 |> List.map (String.fromInt >> toCell)
    }


init : ( Model, Cmd Msg )
init =
    ( { board = bingoFeministe }, Cmd.none )


estEntier : Float -> Bool
estEntier nombre =
    (abs <| nombre - (toFloat <| round nombre)) < 0.001


numerote : Int -> List a -> List ( Int, a )
numerote start liste =
    case liste of
        [] ->
            []

        premier :: reste ->
            ( start, premier ) :: numerote (start + 1) reste


decoupeListe : Int -> Int -> List a -> List (List ( Int, a ))
decoupeListe start taille elements =
    case elements of
        [] ->
            []

        _ ->
            (take taille elements |> numerote start) :: (decoupeListe (start + taille) taille <| drop taille elements)


squareSplit : List a -> Maybe (List (List ( Int, a )))
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
        Just <| decoupeListe 0 tailleLigne cells

    else
        Nothing



---- UPDATE ----


type Msg
    = Ticked Int


tickCell cells num =
    case getAt num cells of
        Nothing ->
            cells

        Just { ticked, text } ->
            setAt num { ticked = not ticked, text = text } cells


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ticked num ->
            let
                thisBoard =
                    model.board
            in
            ( { model | board = { thisBoard | cells = tickCell model.board.cells num } }, Cmd.none )



---- VIEW ----


black =
    rgb255 0 0 0


white =
    rgb255 255 255 255


pink =
    rgb255 255 64 224


viewCell : ( Int, Cell ) -> Element Msg
viewCell ( num, cell ) =
    let
        back =
            if cell.ticked then
                pink

            else
                white
    in
    el [ Brd.color black, Brd.width 1, Bck.color back, padding 20, Ev.onClick <| Ticked num ] <| text cell.text


viewRow oneRow =
    oneRow
        |> List.map viewCell
        |> row [ centerX, spacing 40 ]


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
