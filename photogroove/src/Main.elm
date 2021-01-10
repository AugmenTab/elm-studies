module Main exposing (main)

import Browser exposing (sandbox)
import Html exposing (Html, div, h1, h3, img, text, button, label, input, select)
import Html.Attributes exposing (id, class, classList, src, type_, name, checked, title)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Random


-- MODEL --
type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Photo =
    { url : String 
    , size : Int
    , title : String    
    }


photoDecoder : Decoder Photo
photoDecoder =
    succeed buildPhoto
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


buildPhoto : String -> Int -> String -> Photo
buildPhoto url size title =
    { url = url, size = size, title = title }


type Status
    = Loading
    | Loaded (List Photo) String
    | Error String


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder)
        }


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


-- UPDATE --
type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )
        
        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model
                
                Loaded [] _ ->
                    ( model, Cmd.none )
                
                Loading ->
                    ( model, Cmd.none )

                Error errorMessage ->
                    ( model, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )
        
        GotPhotos (Ok photos) ->
            case photos of
                (first :: rest) ->
                    ( { model | status = Loaded photos first.url } 
                    , Cmd.none
                    )
                
                [] ->
                    ( { model | status = Error "0 photos found" }, Cmd.none )
        
        GotPhotos (Err httpError) ->
            ( { model | status = Error "Server error!" }, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url
        
        Loading ->
            status

        Error errorMessage ->
            status


-- VIEW --
urlPrefix : String
urlPrefix =
    "https://elm-in-action.com/"


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize
            
            Loading ->
                []

            Error errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
        [ h1 [] [ text "Photo Groove" ]
        , button [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size" ]
        , div [ id "choose-size" ] 
            (List.map (viewSizeChooser chosenSize) [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString chosenSize) ] <|
            List.map (viewThumbnail selectedUrl) photos
        , img 
            [ class "large" 
            , src (urlPrefix ++ "large/" ++ selectedUrl)
            ]
            []
        ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img 
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSize size =
    label []
        [ input [ type_ "radio", name "size", checked (size == chosenSize), onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"