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
import Maybe.Extra as ME



---- MODEL ----


type alias Cell =
    { ticked : Bool
    , text : String
    }


type alias BingoBoard =
    { title : String
    , size : Int
    , cells : List Cell
    }


type alias Model =
    { board : BingoBoard
    , bingo : Bool
    }


toCell string =
    { ticked = False, text = string }


bingoFeministe =
    { title = "Bingo féministe"
    , size = 3
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
    , size = 5
    , cells = List.range 1 25 |> List.map (String.fromInt >> toCell)
    }


init : ( Model, Cmd Msg )
init =
    ( { board = bingoFeministe, bingo = False }, Cmd.none )


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


type alias Coord =
    { row : Int
    , col : Int
    }


numToCoord : Int -> Int -> Coord
numToCoord size num =
    { row = num // size, col = modBy size num }


coordToNum : Int -> Coord -> Int
coordToNum size cell =
    cell.row * size + cell.col


sameRow : Int -> Coord -> List Coord
sameRow size cell =
    List.range 0 (size - 1)
        |> List.map (\thisCol -> { row = cell.row, col = thisCol })


sameCol : Int -> Coord -> List Coord
sameCol size cell =
    List.range 0 (size - 1)
        |> List.map (\thisRow -> { row = thisRow, col = cell.col })


type Diagonal
    = Slash -- /
    | AntiSlash -- \


diagonalStart : Diagonal -> Int -> Coord
diagonalStart diag size =
    case diag of
        Slash ->
            { row = size - 1, col = 0 }

        AntiSlash ->
            { row = 0, col = 0 }


nextDiagonalCell : Diagonal -> Coord -> Coord
nextDiagonalCell diag cell =
    case diag of
        Slash ->
            { row = cell.row - 1, col = cell.col + 1 }

        AntiSlash ->
            { row = cell.row + 1, col = cell.col + 1 }


nextDiagonalCells : Diagonal -> Int -> Coord -> List Coord
nextDiagonalCells diag size cell =
    let
        nextCell =
            nextDiagonalCell diag cell
    in
    if nextCell.col >= size then
        [ cell ]

    else
        cell :: nextDiagonalCells diag size nextCell


isOnDiag : Diagonal -> Int -> Coord -> Bool
isOnDiag diag size cell =
    case diag of
        Slash ->
            cell.col == size - 1 - cell.row

        AntiSlash ->
            cell.row == cell.col


sameDiag : Diagonal -> Int -> Coord -> List Coord
sameDiag diag size cell =
    if isOnDiag diag size cell then
        nextDiagonalCells diag size <| diagonalStart diag size

    else
        []


areCellsTicked : BingoBoard -> List Coord -> Maybe Bool
areCellsTicked board cells =
    if cells == [] then
        Just False

    else
        cells
            |> List.map (coordToNum board.size)
            |> List.map (\num -> getAt num board.cells)
            |> ME.combine
            |> Maybe.map (List.all .ticked)


isRowTicked : BingoBoard -> Coord -> Maybe Bool
isRowTicked board cell =
    sameRow board.size cell |> areCellsTicked board


isColTicked : BingoBoard -> Coord -> Maybe Bool
isColTicked board cell =
    sameCol board.size cell |> areCellsTicked board


isDiagTicked : BingoBoard -> Diagonal -> Coord -> Maybe Bool
isDiagTicked board diag cell =
    sameDiag diag board.size cell |> areCellsTicked board


anyJustTrue : List (Maybe Bool) -> Bool
anyJustTrue mbools =
    case mbools of
        [] ->
            False

        (Just True) :: _ ->
            True

        _ :: rest ->
            anyJustTrue rest


hasBingoAt : Int -> BingoBoard -> Bool
hasBingoAt num board =
    let
        coord =
            numToCoord board.size num

        mRow =
            isRowTicked board coord

        mCol =
            isColTicked board coord

        mSlash =
            isDiagTicked board Slash coord

        mAntiSlash =
            isDiagTicked board AntiSlash coord
    in
    anyJustTrue [ mRow, mCol, mSlash, mAntiSlash ]



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

                newBoard =
                    { thisBoard | cells = tickCell model.board.cells num }

                hasBingo =
                    hasBingoAt num newBoard
            in
            ( { model
                | board = newBoard
                , bingo = hasBingo
              }
            , Cmd.none
            )



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
                ++ [ el [] <|
                        text <|
                            if model.bingo then
                                "BINGO!"

                            else
                                ""
                   ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
