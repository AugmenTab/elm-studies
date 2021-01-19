port module PhotoGroove exposing (Model, Msg(..), Photo, Status(..),
    initialModel, main, photoDecoder, update, urlPrefix, view)

import Browser exposing (..)
import Html exposing (Html, 
    button, canvas, div, h1, h3, img, input, label, node, select, text)
import Html.Attributes as Attr exposing (
    checked, class, classList, id, name, src, title, type_)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Random


-- MODEL --
port setFilters : FilterOptions -> Cmd msg
port activityChanges : (String -> msg) -> Sub msg


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


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
    , activity : String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


initialModel : Model
initialModel =
    { status = Loading
    , activity = ""
    , chosenSize = Medium
    , hue = 0
    , ripple = 0
    , noise = 0
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder)
        }


init : Float -> (  Model, Cmd Msg )
init flags =
    let
        activity =
            "Initializing with Pasta v" ++ String.fromFloat flags
    
    in
        ( { initialModel | activity = activity }, initialCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    activityChanges GotActivity


main : Program Float Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- UPDATE --
type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotActivity String
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }
        
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

        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }
        
        GotPhotos (Ok photos) ->
            case photos of
                (first :: rest) ->
                    applyFilters
                        { model 
                            | status = 
                                case List.head photos of
                                    Just photo ->
                                        Loaded photos photo.url

                                    Nothing ->
                                        Loaded [] ""
                        } 
                
                [] ->
                    ( { model | status = Error "0 photos found" }, Cmd.none )
        
        GotPhotos (Err httpError) ->
            ( { model | status = Error "Server error!" }, Cmd.none )

        SlidHue hue ->
            applyFilters { model | hue =hue }
        
        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlidNoise noise ->
            applyFilters { model | noise = noise }


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded photos selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]
                
                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            
            in
                ( model, setFilters { url = url, filters = filters } )
        
        Loading ->
            ( model, Cmd.none )
        
        Error errorMessage ->
            ( model, Cmd.none )


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
                viewLoaded photos selectedUrl model
            
            Loading ->
                []

            Error errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ] 
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
        [ h1 [] [ text "Photo Groove" ]
        , button [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
        , div [ class "activity" ] [ text model.activity ]
        , div [ class "filters" ]
            [ viewFilter SlidHue "Hue" model.hue
            , viewFilter SlidRipple "Ripple" model.ripple
            , viewFilter SlidNoise "Noise" model.noise
            ]
        , h3 [] [ text "Thumbnail Size" ]
        , div [ id "choose-size" ] 
            (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ] <|
            List.map (viewThumbnail selectedUrl) photos
        , canvas
            [ class "large" 
            , id "main-canvas"
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
        [ input [ type_ "radio"
                , name "size"
                , checked (size == chosenSize)
                , onClick (ClickedSize size) 
                ]
                []
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


rangeSlider : List (Html.Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


onSlide : (Int -> Msg) -> Html.Attribute Msg
onSlide toMsg =
    at [ "detail", "userSlidTo" ] int
        |> Json.Decode.map toMsg
        |> on "slide"