module Main exposing (..)

import Browser exposing (sandbox)
import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- MODEL --
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


main = Browser.sandbox
    { init = initialModel
    , view = view
    , update = update
    }


-- UPDATE --
update msg model =
    if msg.description == "ClickedPhoto" then
        { model | selectedUrl = msg.data }
    
    else
        model


-- VIEW --
urlPrefix =
    "https://elm-in-action.com/"


view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ] 
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img 
            [ class "large" 
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


viewThumbnail selectedUrl thumb =
    img 
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick { description = "ClickedPhoto", data = thumb.url }
        ]
        []
