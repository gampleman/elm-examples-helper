module Example exposing
    ( loading, error
    , switchableViews, linkTo, navigation
    , configurable, Configuration, configuration, verticalPanel, intSlider, floatSlider, colorPicker, ConfigMsg
    )

{-| This module is designed to make certain kinds of example easier to understand by ommitting some of the boring details and yet making them attractive.


## Common Views

These views help flesh out some of the views for common things that would needlesly complicate an application:

@docs loading, error


## Example Applications

These are application templates that provide simple interactive functionality for certain kinds of examples. They are quite specific to their usecase, so it is worth considering if they are appropriate to the example at hand or wheather they should be custom built.


### Multi-view Examples

Some examples consist of multiple tabs (each with its own URL) that simply vary some configuration. This provides a simplified application template to make an example like this.

@docs switchableViews, linkTo, navigation


### Configurable Examples

Some examples can be made interactive by allowing the user to configure parts of the view with some parameters. This application type will handle managing the model and messages for you. It includes an automatic form builder as well as updating the serialized representation to the URL.

@docs configurable, Configuration, configuration, verticalPanel, intSlider, floatSlider, colorPicker, ConfigMsg

-}

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation exposing (Key)
import Color exposing (Color)
import Hex
import Html exposing (Html, div, text, th)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Http
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query as QueryParser



-- Common Views


{-| Displays a loading spinner indicating an operation in process.
-}
loading : List (Html.Attribute msg) -> Html msg
loading attrs =
    div (style "display" "flex" :: style "flex-direction" "column" :: style "align-items" "center" :: style "max-width" "900px" :: style "margin-top" "50px" :: attrs)
        [ Html.node "style" [] [ text """
        .lds-ring {
            display: inline-block;
            position: relative;
            width: 60px;
            height: 60px;
        }
        .lds-ring div {
            box-sizing: border-box;
            display: block;
            position: absolute;
            width: 44px;
            height: 44px;
            margin: 8px;
            border: 4px solid #fff;
            border-radius: 50%;
            animation: lds-ring 1.2s cubic-bezier(0.5, 0, 0.5, 1) infinite;
            border-color: #000 transparent transparent transparent;
        }
        .lds-ring div:nth-child(1) {
            animation-delay: -0.45s;
        }
            .lds-ring div:nth-child(2) {
            animation-delay: -0.3s;
        }
        .lds-ring div:nth-child(3) {
            animation-delay: -0.15s;
        }
        @keyframes lds-ring {
            0% {
                transform: rotate(0deg);
            }
            100% {
                transform: rotate(360deg);
            }
        }
""" ]
        , div [ class "lds-ring" ] [ div [] [], div [] [], div [] [] ]
        , div [ class "lds-label" ] [ text "Loading..." ]
        ]


{-| Shows a friendly (albeit generic) error message based on an HTTP.Error type. Useful for showing when an HTTP request fails.

You can optionally give it a message. If you do, then certain errror will get a "Try again?" button for retrying the request.

-}
error : Maybe msg -> Http.Error -> Html msg
error maybeTryAgain err =
    let
        wrapInRetry el =
            case maybeTryAgain of
                Just tryAgain ->
                    div []
                        [ el
                        , Html.button [ onClick tryAgain ] [ text "Try again?" ]
                        ]

                Nothing ->
                    el
    in
    case err of
        Http.BadUrl url ->
            text <| "The url requested (" ++ url ++ ") was not valid."

        Http.Timeout ->
            wrapInRetry (text "The requested timed out.")

        Http.NetworkError ->
            wrapInRetry (text "You appear to be offline.")

        Http.BadStatus status ->
            case status of
                400 ->
                    div []
                        [ Html.h4 [] [ text "400 Bad Request" ]
                        , text "The request was malformed and the server couldn't process it."
                        ]

                403 ->
                    div []
                        [ Html.h4 [] [ text "403 Forbidden" ]
                        , text "You are not authorized to access this resource."
                        ]

                404 ->
                    wrapInRetry <|
                        div []
                            [ Html.h4 [] [ text "404 Not Found" ]
                            , text "The requested resource could not be found."
                            ]

                500 ->
                    wrapInRetry <|
                        div []
                            [ Html.h4 [] [ text "500 Internal Server Error" ]
                            , text "Something went unexpectedly wrong in the server."
                            ]

                otherwise ->
                    text <| "Your request failed with status code: " ++ String.fromInt status

        Http.BadBody errorStr ->
            Html.details []
                [ Html.summary [] [ text "Parsing of the request body failed." ]
                , Html.pre [ Html.Attributes.style "white-space" "pre-wrap" ]
                    [ text errorStr
                    ]
                ]



-- Mutli-tab apps


stringify : String -> String
stringify =
    String.toLower
        >> String.map
            (\char ->
                case char of
                    ' ' ->
                        '-'

                    ':' ->
                        '-'

                    a ->
                        a
            )


parse : List ( String, a ) -> Url -> Maybe String
parse conf url =
    url.fragment


view : List ( String, a ) -> (a -> Html msg) -> Model -> Document Msg
view config viewFn model =
    let
        ( route, subView ) =
            if model.view == "" then
                List.head config
                    |> Maybe.map (\( r, m ) -> ( r, viewFn m ))
                    |> Maybe.withDefault ( "Not found", Html.text "404 not found" )

            else
                List.filter (\( l, _ ) -> stringify l == model.view) config
                    |> List.head
                    |> Maybe.map (\( r, m ) -> ( r, viewFn m ))
                    |> Maybe.withDefault ( "Not found", Html.text "404 not found" )
    in
    { body = [ Html.map (always Noop) subView ], title = route }


{-| Shows a simple tab bar with a title and links to the individual views.
-}
navigation : String -> List ( String, a ) -> Html msg
navigation title config =
    Html.p [] <|
        text
            (title ++ ": ")
            :: (List.intersperse (text " | ") <| List.map (\( a, _ ) -> linkTo a [] [ text a ]) config)


{-| A simple app that shows different tabs based on a string identifier. This identifier will be as the fragment in the URL.
-}
switchableViews : List ( String, a ) -> (a -> Html msg) -> Program () Model Msg
switchableViews config viewFn =
    Browser.application
        { init = init config
        , update = update
        , view = view config viewFn
        , subscriptions = always Sub.none
        , onUrlRequest = onUrlRequest config
        , onUrlChange = onUrlChange config
        }


type Msg
    = Noop
    | View String
    | ExternalUrl String


type alias Model =
    { view : String
    , key : Key
    }


init : List ( String, a ) -> () -> Url -> Key -> ( Model, Cmd Msg )
init config () url key =
    case parse config url of
        Just viewPath ->
            ( { view = viewPath, key = key }, Cmd.none )

        Nothing ->
            let
                defaultPath =
                    List.head config
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault ""
                        |> stringify
            in
            ( { view = "", key = key }, Navigation.replaceUrl key ("#" ++ defaultPath) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        View viewPath ->
            ( { model | view = viewPath }
            , if model.view == viewPath then
                Cmd.none

              else
                Navigation.pushUrl model.key ("#" ++ viewPath)
            )

        ExternalUrl url ->
            ( model, Navigation.load url )


onUrlRequest : List ( String, a ) -> UrlRequest -> Msg
onUrlRequest config urlRequest =
    case urlRequest of
        Internal url ->
            View (parse config url |> Maybe.withDefault "")

        External url ->
            ExternalUrl url


onUrlChange : List ( String, a ) -> Url -> Msg
onUrlChange config url =
    View (parse config url |> Maybe.withDefault "")


{-| Makes a link to a particular identifier (i.e. this is a wrapper for the `<a>` element, but it will take care of setting up the approriate `href` attribute.)
-}
linkTo : String -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
linkTo viewId attrs children =
    let
        hrefAttr =
            href ("#" ++ stringify viewId)

        attributes =
            hrefAttr :: attrs
    in
    Html.a attributes children



-- Configurable example


{-| This type stores how to modify the model.
-}
type Configuration model
    = Configuration model (List (Field model))


type Field model
    = Field
        { label : String
        , getter : model -> String
        , setter : String -> model -> model
        , type_ : FieldType
        }


type FieldType
    = IntSlider Int Int
    | FloatSlider Float Float
    | ColorPicker


{-| -}
type ConfigMsg model
    = ConfigMsg (model -> model)
    | ConfigNoop


{-| Accepts the initial (or default) model as well as a list of fields that describe how to modify the model.
-}
configuration : model -> List (Field model) -> Configuration model
configuration =
    Configuration


{-| Modify an int field in the model. Takes a label, a getter, a setter and min/max bounds on the value. Renders as a slider in the form.
-}
intSlider : String -> (model -> Int) -> (Int -> model -> model) -> Int -> Int -> Field model
intSlider title getter setter min max =
    Field
        { label = title
        , getter = getter >> String.fromInt
        , setter = \value -> setter (String.toInt value |> Maybe.withDefault min)
        , type_ = IntSlider min max
        }


{-| Modify an float field in the model. Takes a label, a getter, a setter and min/max bounds on the value. Renders as a slider in the form.
-}
floatSlider : String -> (model -> Float) -> (Float -> model -> model) -> Float -> Float -> Field model
floatSlider title getter setter min max =
    Field
        { label = title
        , getter = getter >> toFixed (precisionFixed (tickStep min max 200))
        , setter = \value -> setter (String.toFloat value |> Maybe.withDefault min)
        , type_ = FloatSlider min max
        }


{-| Modify an Color field in the model. Takes a label, a getter, and a setter. Renders as a color picker in the form.
-}
colorPicker : String -> (model -> Color) -> (Color -> model -> model) -> Field model
colorPicker title getter setter =
    Field
        { label = title
        , getter = getter >> colorToHex
        , setter = hexToColor >> setter
        , type_ = ColorPicker
        }


{-| An application type where the view will simply render some output based on some input that we want the user to modify.

This application will take care of dealing with the model and messages as well as persisting the model state to the URL and then restoring it on reload.

-}
configurable : Configuration model -> (model -> Html (ConfigMsg model)) -> Program () ( Key, model ) (ConfigMsg model)
configurable config view_ =
    Browser.application
        { init = \_ url key -> ( ( key, parseConfigUrl url config ), Cmd.none )
        , update =
            \msg ( key, model ) ->
                case msg of
                    ConfigMsg fn ->
                        let
                            newModel =
                                fn model
                        in
                        ( ( key, newModel ), Navigation.replaceUrl key (serialize config newModel) )

                    ConfigNoop ->
                        ( ( key, model ), Cmd.none )
        , view = \( _, model ) -> { title = "Example", body = [ view_ model ] }
        , subscriptions = always Sub.none
        , onUrlRequest = \_ -> ConfigNoop
        , onUrlChange = \_ -> ConfigNoop
        }


parseConfigUrl : Url -> Configuration model -> model
parseConfigUrl url (Configuration initial fields) =
    case fields of
        [] ->
            initial

        (Field first) :: tail ->
            let
                parser =
                    List.foldl
                        (\(Field field) ->
                            QueryParser.map2
                                (\parsed rest ->
                                    case parsed of
                                        Just v ->
                                            field.setter v << rest

                                        Nothing ->
                                            rest
                                )
                                (QueryParser.string (stringify field.label))
                        )
                        (QueryParser.map (Maybe.map first.setter >> Maybe.withDefault identity) (QueryParser.string (stringify first.label)))
                        tail
            in
            case Url.Parser.parse (Url.Parser.top <?> parser) { url | path = "/" } of
                Just fn ->
                    fn initial

                Nothing ->
                    initial


serialize : Configuration model -> model -> String
serialize (Configuration _ fields) model =
    List.map (\(Field field) -> Url.Builder.string (stringify field.label) (field.getter model)) fields |> Url.Builder.toQuery


{-| Renders a form for changing the configuration in a vertical layout.

The first argument is a heading for the form.

-}
verticalPanel : String -> Configuration model -> model -> Html (ConfigMsg model)
verticalPanel title (Configuration _ fields) model =
    div [ style "font-family" "-apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, Helvetica, Arial, sans-serif" ]
        [ Html.h2 [] [ text title ]
        , div [ style "display" "table" ] <|
            List.map
                (\(Field field) ->
                    div [ style "display" "table-row" ]
                        [ Html.label [ style "display" "table-cell", style "padding-bottom" "8px", Html.Attributes.for (stringify field.label) ] [ text field.label ]
                        , case field.type_ of
                            IntSlider min max ->
                                Html.span [ style "display" "table-cell" ]
                                    [ Html.input
                                        [ Html.Attributes.type_ "range"
                                        , Html.Events.onInput (\v -> ConfigMsg (field.setter v))
                                        , Html.Attributes.min (String.fromInt min)
                                        , Html.Attributes.max (String.fromInt max)
                                        , Html.Attributes.step "1"
                                        , Html.Attributes.value (field.getter model)
                                        ]
                                        []
                                    , Html.span [] [ text (field.getter model) ]
                                    ]

                            FloatSlider min max ->
                                Html.span []
                                    [ Html.input
                                        [ Html.Attributes.type_ "range"
                                        , Html.Events.onInput (\v -> ConfigMsg (field.setter v))
                                        , Html.Attributes.min (String.fromFloat min)
                                        , Html.Attributes.max (String.fromFloat max)
                                        , Html.Attributes.step (String.fromFloat ((max - min) / 200))
                                        , Html.Attributes.value (field.getter model)
                                        ]
                                        []
                                    , Html.span [] [ text (field.getter model) ]
                                    ]

                            ColorPicker ->
                                Html.input
                                    [ Html.Attributes.type_ "color"
                                    , Html.Events.onInput (\v -> ConfigMsg (field.setter v))
                                    , Html.Attributes.value (field.getter model)
                                    ]
                                    []
                        ]
                )
                fields
        ]


toFixed : Int -> Float -> String
toFixed precision value =
    let
        power_ =
            toFloat 10 ^ toFloat precision

        pad num =
            case num of
                [ x, y ] ->
                    [ x, String.padRight precision '0' y ]

                [ val ] ->
                    if precision > 0 then
                        [ val, String.padRight precision '0' "" ]

                    else
                        [ val ]

                val ->
                    val
    in
    (round (value * power_) |> toFloat)
        / power_
        |> String.fromFloat
        |> String.split "."
        |> pad
        |> String.join "."


tickStep : Float -> Float -> Int -> Float
tickStep start stop count =
    let
        step0 =
            abs (stop - start) / max 0 (toFloat count)

        step1 =
            toFloat (10 ^ floor (logBase e step0 / logBase e 10))

        err =
            step0 / step1

        step2 =
            if err >= sqrt 50 then
                step1 * 10

            else if err >= sqrt 10 then
                step1 * 5

            else if err >= sqrt 2 then
                step1 * 2

            else
                step1
    in
    if stop < start then
        -step2

    else
        step2


exponent x =
    if x == 0 then
        0

    else if x < 1 then
        1 + exponent (x * 10)

    else
        0


precisionFixed step =
    max 0 (exponent (abs step))


{-| Hexadecimal color string to Color
-}
hexToColor : String -> Color
hexToColor hex =
    hex
        |> String.dropLeft 1
        |> (\s ->
                let
                    r =
                        String.slice 0 2 s
                            |> Hex.fromString
                            |> Result.withDefault 0

                    g =
                        String.slice 2 4 s
                            |> Hex.fromString
                            |> Result.withDefault 0

                    b =
                        String.slice 4 6 s
                            |> Hex.fromString
                            |> Result.withDefault 0
                in
                Color.rgb255 r g b
           )


colorToHex : Color -> String
colorToHex color =
    let
        { red, green, blue } =
            Color.toRgba color

        digit value =
            String.padLeft 2 '0' (Hex.toString (round (255 * value)))
    in
    "#" ++ digit red ++ digit green ++ digit blue
