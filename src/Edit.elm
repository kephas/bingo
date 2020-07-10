module Edit exposing (..)

import Cmd.Extra exposing (withNoCmd)
import Dict
import Element exposing (Element, column, el, row, spacing, text)
import Element.Input as In
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
    row [ spacing 10 ]
        [ In.text []
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
        , In.button []
            { onPress = Just <| ChangeViewMode ViewLoad
            , label = text "Load"
            }
        ]
    , row [ spacing 20 ]
        [ In.text [ onEnter AddNewChoice ]
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
    ]
        ++ List.indexedMap viewChoiceInput model.newChoices
