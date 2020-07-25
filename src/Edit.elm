port module Edit exposing (..)

import Cmd.Extra exposing (withCmd, withNoCmd)
import Colors exposing (..)
import Dict
import Element exposing (Element, alignTop, centerX, column, el, fill, fillPortion, padding, row, spacing, text, width)
import Element.Font as Font
import Element.Input as In
import Element.Region exposing (heading)
import Html exposing (Html, button)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import List.Extra exposing (removeAt, setAt)


type alias Model =
    { viewMode : ViewMode
    , newTitle : String
    , newSize : Int
    , tempChoice : String
    , newChoices : List String
    , newIsPlayable : Bool
    , storedBingoDrafts : BingoDraftDict
    }


init : Model
init =
    { viewMode = ViewChoices
    , newTitle = ""
    , newSize = 1
    , tempChoice = ""
    , newChoices = []
    , newIsPlayable = False
    , storedBingoDrafts = Dict.empty
    }


type alias BingoDraftDict =
    Dict.Dict String BingoDraft


type alias BingoDraft =
    { title : String
    , size : Int
    , choices : List String
    }


draftFromModel : Model -> BingoDraft
draftFromModel model =
    { title = model.newTitle
    , size = model.newSize
    , choices = model.newChoices
    }


type ViewMode
    = ViewChoices
    | ViewLoad


port storeDrafts : String -> Cmd msg


port loadDrafts : (String -> msg) -> Sub msg


encodeDraft : BingoDraft -> E.Value
encodeDraft draft =
    E.object
        [ ( "title", E.string draft.title )
        , ( "size", E.int draft.size )
        , ( "choices", E.list E.string draft.choices )
        ]


encodeDrafts : BingoDraftDict -> String
encodeDrafts dict =
    dict
        |> Dict.values
        |> E.list encodeDraft
        |> E.encode 0


draftDecoder : D.Decoder BingoDraft
draftDecoder =
    D.map3 BingoDraft
        (D.field "title" D.string)
        (D.field "size" D.int)
        (D.field "choices" <| D.list D.string)


draftListDecoder : D.Decoder (List BingoDraft)
draftListDecoder =
    D.list draftDecoder


decodeDrafts : String -> BingoDraftDict
decodeDrafts json =
    case D.decodeString draftListDecoder json of
        Ok drafts ->
            List.foldl (\draft dict -> Dict.insert draft.title draft dict) Dict.empty drafts

        Err _ ->
            Dict.empty


storeDraftsEffect : Model -> ( Model, Effect )
storeDraftsEffect model =
    ( model, model.storedBingoDrafts |> encodeDrafts |> storeDrafts |> EditCmd )


initializeBingoDrafts : Maybe BingoDraftDict -> BingoDraftDict
initializeBingoDrafts mdict =
    case mdict of
        Nothing ->
            Dict.empty

        Just dict ->
            dict


type Msg
    = DecrementSize
    | IncrementSize
    | ChangeNewSize String
    | ChangeNewTitle String
    | SaveBingoDraft BingoDraft
    | ResetBingoDraft
    | ChangeViewMode ViewMode
    | ChangeTempChoice String
    | AddNewChoice
    | ChangeExistingChoice Int String
    | RemoveChoice Int
    | LoadDraft String
    | DeleteDraft String
    | PlayBingoDraft BingoDraft


type Effect
    = NoEffect
    | EditCmd (Cmd Msg)
    | PlayDraft BingoDraft


withNoEffect : Model -> ( Model, Effect )
withNoEffect model =
    ( model, NoEffect )


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        ChangeNewSize strSize ->
            withNoEffect <|
                updatePlayable <|
                    case strSize |> String.toInt of
                        Nothing ->
                            model

                        Just size ->
                            if size >= 1 then
                                { model | newSize = size }

                            else
                                model

        IncrementSize ->
            withNoEffect <|
                updatePlayable <|
                    { model | newSize = model.newSize + 1 }

        DecrementSize ->
            withNoEffect <|
                updatePlayable <|
                    if model.newSize > 1 then
                        { model | newSize = model.newSize - 1 }

                    else
                        model

        ChangeTempChoice new ->
            { model | tempChoice = new } |> withNoEffect

        AddNewChoice ->
            withNoEffect <|
                updatePlayable <|
                    case model.tempChoice of
                        "" ->
                            model

                        str ->
                            { model
                                | tempChoice = ""
                                , newChoices = model.newChoices ++ [ str ]
                            }

        ChangeExistingChoice index new ->
            { model | newChoices = setAt index new model.newChoices } |> withNoEffect

        RemoveChoice index ->
            { model | newChoices = removeAt index model.newChoices }
                |> updatePlayable
                |> withNoEffect

        SaveBingoDraft draft ->
            let
                newModel =
                    { model | storedBingoDrafts = Dict.insert draft.title draft model.storedBingoDrafts }
            in
            storeDraftsEffect newModel

        ChangeNewTitle title ->
            { model | newTitle = title } |> withNoEffect

        ResetBingoDraft ->
            { model
                | newTitle = ""
                , newChoices = []
                , newIsPlayable = False
            }
                |> withNoEffect

        ChangeViewMode mode ->
            { model | viewMode = mode } |> withNoEffect

        LoadDraft key ->
            let
                mDraft =
                    Dict.get key model.storedBingoDrafts
            in
            case mDraft of
                Nothing ->
                    model |> withNoEffect

                Just draft ->
                    { model
                        | newTitle = draft.title
                        , newSize = draft.size
                        , tempChoice = ""
                        , newChoices = draft.choices
                    }
                        |> updatePlayable
                        |> withNoEffect

        DeleteDraft key ->
            let
                newModel =
                    { model | storedBingoDrafts = Dict.remove key model.storedBingoDrafts }
            in
            storeDraftsEffect newModel

        PlayBingoDraft draft ->
            ( model, PlayDraft draft )


updatePlayable : Model -> Model
updatePlayable model =
    { model | newIsPlayable = model.newSize ^ 2 <= List.length model.newChoices }


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (D.field "key" D.string
                |> D.andThen
                    (\key ->
                        if key == "Enter" then
                            D.succeed msg

                        else
                            D.fail "Not the enter key"
                    )
            )
        )


viewChoiceInput : Int -> String -> Element Msg
viewChoiceInput index choice =
    row [ width fill, spacing 10 ]
        [ In.text [ width fill ]
            { onChange = ChangeExistingChoice index
            , text = choice
            , placeholder = Nothing
            , label = In.labelHidden "Choice:"
            }
        , In.button []
            { onPress = Just <| RemoveChoice index
            , label = text "Delete"
            }
        ]


draftLoadButton : String -> Element Msg
draftLoadButton draftKey =
    row [ spacing 20 ]
        [ In.button []
            { onPress = Just <| LoadDraft draftKey
            , label = text draftKey
            }
        , In.button []
            { onPress = Just <| DeleteDraft draftKey
            , label = text "X"
            }
        ]


view : Model -> List (Element Msg)
view model =
    [ row []
        [ In.text []
            { onChange = ChangeNewSize
            , text = String.fromInt model.newSize
            , placeholder = Nothing
            , label = In.labelLeft [] <| text <| "Size: " ++ String.fromInt model.newSize ++ " x "
            }
        , column []
            [ el [] <| Element.html <| button [ onClick IncrementSize ] [ Html.text "+" ]
            , el [] <| Element.html <| button [ onClick DecrementSize ] [ Html.text "-" ]
            ]
        ]
    , row [ spacing 20 ]
        [ In.text []
            { onChange = ChangeNewTitle
            , text = model.newTitle
            , placeholder = Nothing
            , label = In.labelLeft [] <| text "Title: "
            }
        , In.button []
            { onPress = Just <| SaveBingoDraft <| draftFromModel model
            , label = text "Save"
            }
        , let
            color =
                Font.color <|
                    if model.newIsPlayable then
                        black

                    else
                        grey

            action =
                if model.newIsPlayable then
                    Just <| PlayBingoDraft <| draftFromModel model

                else
                    Nothing
          in
          In.button [ color ]
            { onPress = action
            , label = text "Play"
            }
        , In.button []
            { onPress = Just <| ResetBingoDraft
            , label = text "Reset"
            }
        ]
    , row [ width fill, spacing 20 ]
        [ In.text [ width fill, onEnter AddNewChoice ]
            { onChange = ChangeTempChoice
            , text = model.tempChoice
            , placeholder = Nothing
            , label = In.labelHidden "New choice:"
            }
        , In.button []
            { onPress = Just AddNewChoice
            , label = text "Add"
            }
        ]
    , row [ width fill ]
        [ column [ width <| fillPortion 2 ] <| List.indexedMap viewChoiceInput model.newChoices
        , column [ width <| fillPortion 1, alignTop, padding 20, spacing 10 ] <|
            [ el [ heading 1, centerX, Font.size 24 ] <| text "Saved Bingo Drafts"
            ]
                ++ (Dict.keys model.storedBingoDrafts
                        |> List.sort
                        |> List.map draftLoadButton
                   )
        ]
    ]
