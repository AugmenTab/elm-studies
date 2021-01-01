module Main exposing (..)

import Browser
import Html exposing (div, text, input, button)
import Html.Events exposing (onClick, onInput)
import String exposing (fromInt, toInt)
import Debug exposing (log)


type Messages =
    Add
    | ChangedAddText String


init =
    { value = 0
    , numberToAdd = 0
    }


view model =
    div [] [ 
        text (fromInt model.value) 
        , div [] []
        , input [ onInput ChangedAddText ] []
        , button [ onClick Add ] [ text "Add" ]
        ]


parseUserNumber text = 
    let
        maybe = toInt text
    in
        case maybe of
            Just val ->
                val
            Nothing ->
                0


update msg model =
    case msg of
        Add -> 
            { model | value = model.value + model.numberToAdd }
        ChangedAddText inputText ->
            { model | numberToAdd = parseUserNumber inputText }


main =
    Browser.sandbox
        {
            init = init
            , view = view
            , update = update
        }