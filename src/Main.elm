module Main exposing (..)

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Cmd.Extra exposing (withCmd, withNoCmd)
import Dict
import Edit
import Element exposing (Element, centerX, centerY, column, el, fill, fillPortion, height, layout, maximum, padding, paragraph, px, rgb255, row, spacing, text, width)
import Element.Background as Bck
import Element.Border as Brd
import Element.Events as Ev
import Element.Font as Font
import Element.Input as In
import Element.Region exposing (heading)
import Html exposing (Html, button)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import List exposing (drop, take)
import List.Extra exposing (getAt, removeAt, setAt, setIf)
import Maybe.Extra as ME
import Platform.Cmd
import Task



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


type ViewportModel
    = UnknownViewport EarlyModel
    | KnownViewport Model


type alias EarlyModel =
    { bingoDrafts : Edit.BingoDraftDict
    }


type alias Model =
    { page : Page
    , board : BingoBoard
    , bingo : Bool
    , editModel : Edit.Model
    , viewport :
        { height : Int
        , width : Int
        }
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


init : ( ViewportModel, Cmd Msg )
init =
    ( UnknownViewport { bingoDrafts = Dict.empty }
    , Task.perform GotViewport Dom.getViewport
    )


initialModelData =
    { page = PlayPage
    , board = bingoFeministe
    , bingo = False
    , editModel =
        { viewMode = Edit.ViewChoices
        , newTitle = ""
        , newSize = 1
        , tempChoice = ""
        , newChoices = []
        , storedBingoDrafts = Dict.empty
        }
    , viewport = { height = 0, width = 0 }
    }


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


type Page
    = PlayPage
    | EditPage


type Msg
    = GotViewport Viewport
    | Ticked Int
    | Navigate Page
    | EditMsg Edit.Msg
    | LocalStorage Edit.BingoDraftDict


tickCell cells num =
    case getAt num cells of
        Nothing ->
            cells

        Just { ticked, text } ->
            setAt num { ticked = not ticked, text = text } cells


update : Msg -> ViewportModel -> ( ViewportModel, Cmd Msg )
update msg viewportModel =
    case ( viewportModel, msg ) of
        ( UnknownViewport model, GotViewport viewport ) ->
            let
                initialEditModel =
                    initialModelData.editModel

                newEditModel =
                    { initialEditModel | storedBingoDrafts = model.bingoDrafts }
            in
            KnownViewport
                { initialModelData
                    | viewport = { width = viewport.viewport.width |> round, height = viewport.viewport.height |> round }
                    , editModel = newEditModel
                }
                |> withNoCmd

        ( UnknownViewport model, LocalStorage drafts ) ->
            UnknownViewport { model | bingoDrafts = drafts } |> withNoCmd

        ( UnknownViewport _, _ ) ->
            viewportModel |> withNoCmd

        ( KnownViewport _, GotViewport _ ) ->
            viewportModel |> withNoCmd

        ( KnownViewport model, Ticked num ) ->
            let
                thisBoard =
                    model.board

                newBoard =
                    { thisBoard | cells = tickCell model.board.cells num }

                hasBingo =
                    hasBingoAt num newBoard
            in
            ( KnownViewport
                { model
                    | board = newBoard
                    , bingo = hasBingo
                }
            , Cmd.none
            )

        ( KnownViewport model, Navigate page ) ->
            KnownViewport { model | page = page } |> withNoCmd

        ( KnownViewport model, EditMsg submsg ) ->
            let
                ( subModel, effect ) =
                    Edit.update submsg model.editModel

                newModel =
                    KnownViewport { model | editModel = subModel }
            in
            case effect of
                Edit.NoEffect ->
                    newModel |> withNoCmd

                Edit.EditCmd subCmd ->
                    newModel |> withCmd (Platform.Cmd.map EditMsg subCmd)

                Edit.PlayDraft draft ->
                    newModel |> withNoCmd

        ( KnownViewport model, LocalStorage drafts ) ->
            let
                oldEditModel =
                    model.editModel

                newEditModel =
                    { oldEditModel | storedBingoDrafts = drafts }

                newModel =
                    { model | editModel = newEditModel }
            in
            KnownViewport newModel |> withNoCmd



---- VIEW ----


black =
    rgb255 0 0 0


white =
    rgb255 255 255 255


pink =
    rgb255 255 64 224


viewCell : Int -> ( Int, Cell ) -> Element Msg
viewCell cellSize ( num, cell ) =
    let
        back =
            if cell.ticked then
                pink

            else
                white
    in
    paragraph [ centerX, Brd.color black, Brd.width 1, Bck.color back, padding 20, height fill, width <| maximum cellSize fill, Ev.onClick <| Ticked num ]
        [ text cell.text ]


viewRow cellSize oneRow =
    oneRow
        |> List.map (viewCell cellSize)
        |> row [ spacing 40, width fill ]


viewRows cellSize rows =
    case rows of
        Nothing ->
            []

        Just someRows ->
            someRows |> List.map (viewRow cellSize)


viewPlayPage : Model -> List (Element Msg)
viewPlayPage model =
    [ el [ heading 1, centerX, Font.size 32 ] <| text model.board.title
    ]
        ++ (model.board.cells
                |> squareSplit
                |> viewRows (model.viewport.width // 8)
           )
        ++ [ el [] <|
                text <|
                    if model.bingo then
                        "BINGO!"

                    else
                        ""
           ]


view : ViewportModel -> Html Msg
view viewportModel =
    case viewportModel of
        UnknownViewport _ ->
            layout [] Element.none

        KnownViewport model ->
            layout [] <|
                row [ width fill ] <|
                    [ column [ width fill, spacing 40, padding 40 ] <|
                        [ row [ padding 20, spacing 20 ]
                            [ In.button []
                                { onPress = Just <| Navigate PlayPage
                                , label = text "Play"
                                }
                            , In.button []
                                { onPress = Just <| Navigate EditPage
                                , label = text "Edit"
                                }
                            ]
                        ]
                            ++ (case model.page of
                                    PlayPage ->
                                        viewPlayPage model

                                    EditPage ->
                                        List.map (Element.map EditMsg) <| Edit.view model.editModel
                               )
                    ]



---- PROGRAM ----


main : Program () ViewportModel Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always <| Edit.loadDrafts (\json -> LocalStorage <| Edit.decodeDrafts json)
        }
