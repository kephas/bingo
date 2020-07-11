module Edit exposing (..)

import Cmd.Extra exposing (withNoCmd)
import Dict
import Element exposing (Element, alignTop, centerX, column, el, fill, fillPortion, row, spacing, text, width)
import Element.Font as Font
import Element.Input as In
import Element.Region exposing (heading)
import Html exposing (Html, button)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List.Extra exposing (removeAt, setAt)


type alias Model =
    { viewMode : ViewMode
    , newTitle : String
    , newSize : Int
    , tempChoice : String
    , newChoices : List String
    , storedBingoDrafts : Dict.Dict String BingoDraft
    }


type alias BingoDraft =
    { title : String
    , size : Int
    , choices : List String
    }


type ViewMode
    = ViewChoices
    | ViewLoad


type Msg
    = DecrementSize
    | IncrementSize
    | ChangeNewSize String
    | ChangeNewTitle String
    | SaveBingoDraft String Int (List String)
    | ResetBingoDraft
    | ChangeViewMode ViewMode
    | ChangeTempChoice String
    | AddNewChoice
    | ChangeExistingChoice Int String
    | RemoveChoice Int
    | LoadDraft String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeNewSize strSize ->
            withNoCmd <|
                case strSize |> String.toInt of
                    Nothing ->
                        model

                    Just size ->
                        if size > 1 then
                            { model | newSize = size }

                        else
                            model

        IncrementSize ->
            withNoCmd <| { model | newSize = model.newSize + 1 }

        DecrementSize ->
            withNoCmd <|
                if model.newSize > 1 then
                    { model | newSize = model.newSize - 1 }

                else
                    model

        ChangeTempChoice new ->
            { model | tempChoice = new } |> withNoCmd

        AddNewChoice ->
            withNoCmd <|
                case model.tempChoice of
                    "" ->
                        model

                    str ->
                        { model
                            | tempChoice = ""
                            , newChoices = model.newChoices ++ [ str ]
                        }

        ChangeExistingChoice index new ->
            { model | newChoices = setAt index new model.newChoices } |> withNoCmd

        RemoveChoice index ->
            { model | newChoices = removeAt index model.newChoices } |> withNoCmd

        SaveBingoDraft title size choices ->
            let
                draft =
                    { title = title, size = size, choices = choices }
            in
            { model | storedBingoDrafts = Dict.insert title draft model.storedBingoDrafts } |> withNoCmd

        ChangeNewTitle title ->
            { model | newTitle = title } |> withNoCmd

        ResetBingoDraft ->
            { model
                | newTitle = ""
                , newChoices = []
            }
                |> withNoCmd

        ChangeViewMode mode ->
            { model | viewMode = mode } |> withNoCmd

        LoadDraft key ->
            let
                mDraft =
                    Dict.get key model.storedBingoDrafts
            in
            case mDraft of
                Nothing ->
                    model |> withNoCmd

                Just draft ->
                    { model
                        | newTitle = draft.title
                        , newSize = draft.size
                        , tempChoice = ""
                        , newChoices = draft.choices
                    }
                        |> withNoCmd


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
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
    In.button []
        { onPress = Just <| LoadDraft draftKey
        , label = text draftKey
        }


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
            { onPress = Just <| SaveBingoDraft model.newTitle model.newSize model.newChoices
            , label = text "Save"
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
        , column [ width <| fillPortion 1, alignTop ] <|
            [ el [ heading 1, centerX, Font.size 24 ] <| text "Saved Bingo Drafts"
            ]
                ++ (Dict.keys model.storedBingoDrafts
                        |> List.sort
                        |> List.map draftLoadButton
                   )
        ]
    ]
