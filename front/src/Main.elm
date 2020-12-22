port module Main exposing (..)

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Cmd.Extra exposing (withCmd, withNoCmd)
import Colors exposing (..)
import Common exposing (DataUploadState(..))
import Dict
import Edit
import Element exposing (Element, centerX, centerY, column, el, fill, fillPortion, height, layout, maximum, none, padding, paragraph, px, row, spacing, text, width)
import Element.Background as Bck
import Element.Border as Brd
import Element.Events as Ev
import Element.Font as Font
import Element.Input as In
import Element.Region exposing (heading)
import Html exposing (Html, button)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Json.Encode as E
import List exposing (drop, take)
import List.Extra exposing (getAt, remove, removeAt, setAt, setIf, uncons)
import Maybe.Extra as ME
import Platform.Cmd
import Task


port storeSavedState : String -> Cmd msg


port loadSavedState : (String -> msg) -> Sub msg



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
    { boards : List BingoBoard
    , bingoDrafts : Edit.BingoDraftDict
    }


type alias Model =
    { page : Page
    , boards : List BingoBoard
    , bingo : Bool
    , editModel : Edit.Model
    , viewport :
        { height : Int
        , width : Int
        }
    , endpointUrl : String
    , dataUploadState : DataUploadState
    }



---- JSON ----


bingoBoardDecoder : D.Decoder BingoBoard
bingoBoardDecoder =
    D.map3 BingoBoard
        (D.field "title" D.string)
        (D.field "size" D.int)
        (D.field "cells" <| D.list cellDecoder)


cellDecoder : D.Decoder Cell
cellDecoder =
    D.map2 Cell
        (D.field "ticked" D.bool)
        (D.field "text" D.string)


encodeBingoBoard : BingoBoard -> E.Value
encodeBingoBoard board =
    E.object
        [ ( "title", E.string board.title )
        , ( "size", E.int board.size )
        , ( "cells", E.list encodeCell board.cells )
        ]


encodeCell : Cell -> E.Value
encodeCell cell =
    E.object
        [ ( "ticked", E.bool cell.ticked )
        , ( "text", E.string cell.text )
        ]


encodeSavedState : Model -> E.Value
encodeSavedState model =
    E.object
        [ ( "boards", E.list encodeBingoBoard model.boards )
        , ( "drafts", Edit.encodeDrafts model.editModel.storedBingoDrafts )
        ]


savedStateJson model =
    encodeSavedState model
        |> E.encode 0


type alias SavedState =
    ( List BingoBoard, Edit.BingoDraftDict )


savedStateDecoder : D.Decoder SavedState
savedStateDecoder =
    D.map2 Tuple.pair
        (D.field "boards" <| D.list bingoBoardDecoder)
        (D.field "drafts" Edit.draftDictDecoder)


decodeSavedState : String -> SavedState
decodeSavedState json =
    D.decodeString savedStateDecoder json
        |> Result.withDefault ( [], Dict.empty )



---- INIT ----


toCell string =
    { ticked = False, text = string }


init : ( ViewportModel, Cmd Msg )
init =
    ( UnknownViewport { boards = [], bingoDrafts = Dict.empty }
    , Task.perform GotViewport Dom.getViewport
    )


initialModelData =
    { page = PlayPage
    , boards = []
    , bingo = False
    , editModel = Edit.init
    , viewport = { height = 0, width = 0 }
    , endpointUrl = "http://localhost:4000/states/EQo9LztvPsYR5vBo_PecDQ"
    , dataUploadState = Done
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
    | ChangePlayBoards (List BingoBoard)
    | RemovePlayBoard BingoBoard
    | Navigate Page
    | EditMsg Edit.Msg
    | LocalStorage SavedState
    | GotDownloadResponse (Result Http.Error SavedState)
    | GotUploadResponse (Result Http.Error Bool)


tickCell cells num =
    case getAt num cells of
        Nothing ->
            cells

        Just { ticked, text } ->
            setAt num { ticked = not ticked, text = text } cells


tickBoard num board =
    { board | cells = tickCell board.cells num }


changeBoards model boards =
    { model
        | boards = boards
        , bingo = False
    }


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
                    , boards = model.boards
                }
                |> withCmd (downloadSavedState initialModelData.endpointUrl)

        ( UnknownViewport model, LocalStorage ( boards, drafts ) ) ->
            UnknownViewport { model | boards = boards, bingoDrafts = drafts } |> withNoCmd

        ( UnknownViewport _, _ ) ->
            viewportModel |> withNoCmd

        ( KnownViewport _, GotViewport _ ) ->
            viewportModel |> withNoCmd

        ( KnownViewport model, Ticked num ) ->
            let
                mThisBoard =
                    List.head model.boards

                mNewBoard =
                    Maybe.map (tickBoard num) mThisBoard

                newBoards =
                    List.tail model.boards
                        |> Maybe.map2 (::) mNewBoard
                        |> Maybe.withDefault []

                hasBingo =
                    mNewBoard
                        |> Maybe.map (hasBingoAt num)
                        |> Maybe.withDefault False
            in
            KnownViewport
                { model
                    | boards = newBoards
                    , bingo = hasBingo
                }
                |> viewportWithStoreDataCmd

        ( KnownViewport model, ChangePlayBoards newBoards ) ->
            changeBoards model newBoards
                |> KnownViewport
                |> withNoCmd

        ( KnownViewport model, RemovePlayBoard board ) ->
            changeBoards model (remove board model.boards)
                |> KnownViewport
                |> viewportWithStoreDataCmd

        ( KnownViewport model, Navigate page ) ->
            KnownViewport { model | page = page } |> withNoCmd

        ( KnownViewport model, EditMsg submsg ) ->
            let
                ( subModel, effect ) =
                    Edit.update submsg model.editModel

                newModelContent =
                    { model | editModel = subModel }

                newModel =
                    KnownViewport newModelContent
            in
            case effect of
                Edit.NoEffect ->
                    newModel |> withNoCmd

                Edit.EditCmd subCmd ->
                    newModel |> withCmd (Platform.Cmd.map EditMsg subCmd)

                Edit.PlayDraft draft ->
                    switchDraftToPlay newModelContent draft
                        |> KnownViewport
                        |> viewportWithStoreDataCmd

                Edit.StoreData ->
                    newModel |> withCmd (storeDataCmd newModelContent)

        ( KnownViewport model, LocalStorage ( boards, drafts ) ) ->
            let
                oldEditModel =
                    model.editModel

                newEditModel =
                    { oldEditModel | storedBingoDrafts = drafts }

                newModel =
                    { model
                        | boards = boards
                        , editModel = newEditModel
                    }
            in
            KnownViewport newModel |> withNoCmd

        ( KnownViewport model, GotDownloadResponse response ) ->
            case response of
                Ok ( boards, dict ) ->
                    let
                        editModel =
                            model.editModel
                    in
                    KnownViewport
                        { model
                            | editModel = { editModel | storedBingoDrafts = dict }
                            , boards = boards
                        }
                        |> withNoCmd

                Err _ ->
                    KnownViewport model |> withNoCmd

        ( KnownViewport model, GotUploadResponse response ) ->
            withNoCmd <| KnownViewport <| withUploadState model <| responseToDataUploadState response


switchDraftToPlay : Model -> Edit.BingoDraft -> Model
switchDraftToPlay model draft =
    { model
        | page = PlayPage
        , boards =
            { title = draft.title
            , size = draft.size
            , cells = List.map toCell draft.choices
            }
                :: model.boards
        , bingo = False
    }


viewportWithStoreDataCmd : ViewportModel -> ( ViewportModel, Cmd Msg )
viewportWithStoreDataCmd viewportModel =
    case viewportModel of
        UnknownViewport _ ->
            viewportModel |> withNoCmd

        KnownViewport model ->
            viewportModel |> withCmd (storeDataCmd model)


storeDataCmd : Model -> Cmd Msg
storeDataCmd model =
    saveState model.endpointUrl model


withUploadState : Model -> DataUploadState -> Model
withUploadState model state =
    let
        editModel =
            model.editModel
    in
    { model
        | dataUploadState = state
        , editModel = { editModel | dataUploadState = state }
    }


responseToDataUploadState : Result a Bool -> DataUploadState
responseToDataUploadState response =
    case response of
        Ok done ->
            if done then
                Done

            else
                Failed

        Err _ ->
            Failed


downloadSavedState : String -> Cmd Msg
downloadSavedState url =
    Http.get
        { url = url
        , expect = Http.expectJson GotDownloadResponse savedStateDecoder
        }


saveState : String -> Model -> Cmd Msg
saveState url content =
    let
        data =
            savedStateJson content
    in
    Cmd.batch
        [ storeSavedState data
        , uploadSavedState url data
        ]


uploadSavedState : String -> String -> Cmd Msg
uploadSavedState url json =
    Http.post
        { url = url
        , body = Http.stringBody "application/json" json
        , expect = Http.expectJson GotUploadResponse D.bool
        }



---- VIEW ----


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


moveToHead value list =
    value :: remove value list


otherBoardButton currentBoards board =
    In.button []
        { onPress = Just <| ChangePlayBoards <| moveToHead board currentBoards
        , label = text board.title
        }


viewPlayPage : Model -> List (Element Msg)
viewPlayPage model =
    case uncons model.boards of
        Nothing ->
            [ none ]

        Just ( board, otherBoards ) ->
            [ row [ spacing 20 ] <| List.map (otherBoardButton model.boards) otherBoards
            , el [ heading 1, centerX, Font.size 32 ] <| text board.title
            ]
                ++ (board.cells
                        |> squareSplit
                        |> viewRows (model.viewport.width // 8)
                   )
                ++ [ el [] <|
                        text <|
                            if model.bingo then
                                "BINGO!"

                            else
                                ""
                   , In.button []
                        { onPress = Just <| RemovePlayBoard board
                        , label = text "Stop playing"
                        }
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
        , subscriptions = always <| loadSavedState (\json -> LocalStorage <| decodeSavedState json)
        }
