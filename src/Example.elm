module Example exposing
    ( loading, error
    , application, Program, Configuration, tabbed, configuration
    , Field, intSlider, floatSlider, textField, colorPicker, enumPicker
    , withTitle, animatedWith, makeFormOnTop, makeFormOnRight, withCustomCss
    )

{-| This module is designed to make certain kinds of example easier to understand by ommitting some of the boring details and yet making them attractive.
You can get creative with it if you want:

    module MyExample exposing (main)

    import Example as OmittedForBrevity

    view model =
        case model of
            Loading ->
                OmmitedForBrevity.loading


# HTTP

If your example is doing an HTTP request, Elm will force you to handle some states like what to show when the request is in progress and what happens when it fails. However, your example might be about the "happy path" and everything else is just noise. Here we provide some helper views to show something nicer than `Html.text "Loading..."`, but help keep the focus on what's important.

@docs loading, error


# Configurable Views

A common type of example is trying to demonstrate something that happens in a view. Maybe your example is rendering a cool fractal or 3D scene or just a neat way to render a HTML table. Often such functions can be customised by the user with various parameters. The functions here help you with minimal fuss make your view function interactive:

    module MyExample exposing (main)

    import Example

    type alias Model = -- this is your configuration
        { foo : Int
        }


    view : Model -> Html msg
    view model =
        -- do whatever you want here, this your example
        ...


    main : Example.Program Model
    main =
        Example.configuration
            { foo = 3 } -- this is your default or initial model
            -- here you configure your form
            [ Example.intSlider "Foo count" 0 5 .foo (\value model -> {model | foo = value})]
            |> Example.withTitle "My cool example" -- you can customize with options
            |> Example.application view -- Connect it to your view

This gives you a number of built in behaviors:

  - it will render your view function with the default model
  - it will render alongside it a form that allows the user to edit the model
  - it serializes the model into the url as a query string, allowing users to share a particularly configured example
  - no need to write Msg, update functions or HTML forms
  - easily enable additional features like animations or custom titles...

@docs application, Program, Configuration, tabbed, configuration


## Building forms

@docs Field, intSlider, floatSlider, textField, colorPicker, enumPicker


## Customizing the example

These are options you can apply to a configuration to add a few more bells and whistles to your examples:

@docs withTitle, animatedWith, makeFormOnTop, makeFormOnRight, withCustomCss

-}

import Browser exposing (Document, UrlRequest(..))
import Browser.Events
import Browser.Navigation as Navigation exposing (Key)
import Color exposing (Color)
import Hex
import Html exposing (Html, details, div, text, th)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Http
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query as QueryParser


{-| Displays an animated loading spinner indicating an operation is in progress.
-}
loading : List (Html.Attribute msg) -> Html msg
loading attrs =
    div (style "display" "flex" :: style "flex-direction" "column" :: style "align-items" "center" :: style "max-width" "900px" :: style "margin-top" "50px" :: attrs)
        [ Html.node "style" [] [ text """
        .example-ring {
            display: inline-block;
            position: relative;
            width: 60px;
            height: 60px;
        }
        .example-ring div {
            box-sizing: border-box;
            display: block;
            position: absolute;
            width: 44px;
            height: 44px;
            margin: 8px;
            border: 4px solid #fff;
            border-radius: 50%;
            animation: example-ring 1.2s cubic-bezier(0.5, 0, 0.5, 1) infinite;
            border-color: #000 transparent transparent transparent;
        }
        .example-ring div:nth-child(1) {
            animation-delay: -0.45s;
        }
        .example-ring div:nth-child(2) {
            animation-delay: -0.3s;
        }
        .example-ring div:nth-child(3) {
            animation-delay: -0.15s;
        }
        @keyframes example-ring {
            0% {
                transform: rotate(0deg);
            }
            100% {
                transform: rotate(360deg);
            }
        }
""" ]
        , div [ class "example-ring" ] [ div [] [], div [] [], div [] [] ]
        , div [ class "example-label" ] [ text "Loading..." ]
        ]


{-| Shows a friendly (albeit generic) error message based on the `HTTP.Error` type. Useful for showing when an HTTP request fails.

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



-- Configurable example


type Model model
    = Model
        { key : Navigation.Key
        , model : Wrapper model
        }


type Wrapper model
    = Animating
        { elapsed : Int
        , interpolation : Float -> model
        }
    | Static model


{-| All the details needed to make an example app get stored in here.
-}
type Configuration model
    = Configuration
        { initialModel : model
        , fields : List (Field model)
        , animation : Maybe ( model -> model -> Float -> model, Int )
        , title : Maybe String
        , css : Maybe String
        , layout : Layout
        }
    | InvalidConfiguration String


type Layout
    = FormOnTop
    | FormOnRight


{-| -}
type Field model
    = Field
        { label : String
        , getter : model -> String
        , setter : String -> model -> model
        , view : model -> Html (ConfigMsg model)
        }


{-| -}
type ConfigMsg model
    = ConfigMsg (model -> model)
    | ConfigNoop
    | Tick Int


{-| This is the more flexible way of building example configurations. You will need to provide an initial model which is what users will see before they mess with the forms.

Then you need to provide some fields that will show as the form.

-}
configuration : model -> List (Field model) -> Configuration model
configuration initialModel fields =
    Configuration { initialModel = initialModel, fields = fields, animation = Nothing, title = Nothing, css = Nothing, layout = FormOnRight }


mapConfig fn config =
    case config of
        Configuration cfg ->
            Configuration (fn cfg)

        InvalidConfiguration reason ->
            InvalidConfiguration reason


{-| Animate transitions between different model states. To do this, you will need to supply an interpolation function,
that takes a starting state, a target state and a parameter that goes betwen 0 and 1.

Also, you should provide a duration in ms.

-}
animatedWith : (model -> model -> Float -> model) -> Int -> Configuration model -> Configuration model
animatedWith interp t =
    mapConfig (\config -> { config | animation = Just ( interp, t ) })


{-| Add a title to your form. Will also set it as the HTML <title> tag.
-}
withTitle : String -> Configuration model -> Configuration model
withTitle title =
    mapConfig (\config -> { config | title = Just title })


{-| For normal configurations we normally render the form on the right of the view. This will change it to be rendered on top.
-}
makeFormOnTop : Configuration model -> Configuration model
makeFormOnTop =
    mapConfig (\config -> { config | layout = FormOnTop })


{-| Tabbed configurations render the form on the top of the view. This will change it to be rendered on the right.
-}
makeFormOnRight : Configuration model -> Configuration model
makeFormOnRight =
    mapConfig (\config -> { config | layout = FormOnRight })


{-| Give this a block of CSS as a string and it will take care of adding it to the DOM with a proper tag.

Useful for conveniently adding some flair without needing to mess with `Html`.

Also note that the elements we add are style-able and have stable classes. Simply look through the generated DOM to find the appropriate class names.

-}
withCustomCss : String -> Configuration model -> Configuration model
withCustomCss css =
    mapConfig (\config -> { config | css = Just css })


customField :
    { label : String
    , getter : model -> String
    , setter : String -> model -> model
    , view : model -> Html (ConfigMsg model)
    }
    -> Field model
customField =
    Field


{-| Modify an int field in the model. Takes a label, min/max bounds on the value, a getter, and a setter. Renders as a slider in the form.
-}
intSlider : String -> { min : Int, max : Int } -> (model -> Int) -> (Int -> model -> model) -> Field model
intSlider title { min, max } getter setter =
    let
        getter_ =
            getter >> String.fromInt

        setter_ =
            String.toInt >> Maybe.withDefault min >> clamp min max >> setter
    in
    Field
        { label = title
        , getter = getter_
        , setter = setter_
        , view =
            \model ->
                Html.span [ style "display" "table-cell", class "example-field", class "example-intSlider", class ("example-intSlider-" ++ stringify title) ]
                    [ Html.input
                        [ Html.Attributes.type_ "range"
                        , Html.Events.onInput (\v -> ConfigMsg (setter_ v))
                        , Html.Attributes.min (String.fromInt min)
                        , Html.Attributes.max (String.fromInt max)
                        , Html.Attributes.step "1"
                        , Html.Attributes.value (getter_ model)
                        ]
                        []
                    , Html.span [] [ text (getter_ model) ]
                    ]
        }


{-| Modify an float field in the model. Takes a label, min/max bounds on the value, a getter, and a setter. Renders as a slider in the form.
-}
floatSlider : String -> { min : Float, max : Float } -> (model -> Float) -> (Float -> model -> model) -> Field model
floatSlider title { min, max } getter setter =
    let
        getter_ =
            getter >> String.fromFloat

        setter_ =
            String.toFloat >> Maybe.withDefault min >> clamp min max >> setter
    in
    Field
        { label = title
        , getter = getter_
        , setter = setter_
        , view =
            \model ->
                Html.span []
                    [ Html.input
                        [ Html.Attributes.type_ "range"
                        , Html.Events.onInput (\v -> ConfigMsg (setter_ v))
                        , Html.Attributes.min (String.fromFloat min)
                        , Html.Attributes.max (String.fromFloat max)
                        , Html.Attributes.step (String.fromFloat ((max - min) / 200))
                        , Html.Attributes.value (getter_ model)
                        ]
                        []
                    , Html.span [] [ text (getter_ model) ]
                    ]
        }


{-| Modify an Color field in the model (this type comes from avh4/elm-color). Takes a label, a getter, and a setter. Renders as a color picker in the form.
-}
colorPicker : String -> (model -> Color) -> (Color -> model -> model) -> Field model
colorPicker title getter setter =
    let
        getter_ =
            getter >> colorToHex

        setter_ =
            hexToColor >> setter
    in
    Field
        { label = title
        , getter = getter_
        , setter = setter_
        , view =
            \model ->
                Html.input
                    [ Html.Attributes.type_ "color"
                    , Html.Events.onInput (\v -> ConfigMsg (setter_ v))
                    , Html.Attributes.value (getter_ model)
                    ]
                    []
        }


{-| Modify an String field in the model). Takes a label, a getter, and a setter. Renders as a text field in the form.
-}
textField : String -> (model -> String) -> (String -> model -> model) -> Field model
textField title getter setter =
    Field
        { label = title
        , getter = getter
        , setter = setter
        , view =
            \model ->
                Html.input
                    [ Html.Attributes.type_ "text"
                    , Html.Events.onInput (\v -> ConfigMsg (setter v))
                    , Html.Attributes.value (getter model)
                    ]
                    []
        }


{-| Pick from a list of values. Takes a label, a list of values and their labels (the labels are what you will see in the UI as well as what will be used in the URL), a getter and a setter.
-}
enumPicker : String -> List ( String, a ) -> (model -> a) -> (a -> model -> model) -> Field model
enumPicker title opts getter setter =
    case opts of
        [] ->
            Field
                { label = title
                , getter = always ""
                , setter = \val model -> model
                , view = \_ -> Html.text "needs at least on option"
                }

        ( defaultLabel, defaultOption ) :: options ->
            let
                stringified =
                    List.map (Tuple.mapFirst stringify) options

                findByValue val =
                    List.filter (\( label, option ) -> option == val) stringified
                        |> List.head
                        |> Maybe.withDefault ( stringify defaultLabel, defaultOption )
                        |> Tuple.first

                findByLabel lab =
                    List.filter (\( label, option ) -> label == lab) stringified
                        |> List.head
                        |> Maybe.withDefault ( defaultLabel, defaultOption )
                        |> Tuple.second

                getter_ =
                    getter >> findByValue

                setter_ =
                    findByLabel >> setter
            in
            Field
                { label = title
                , getter = getter_
                , setter = setter_
                , view =
                    \model ->
                        (( defaultLabel, defaultOption ) :: options)
                            |> List.map
                                (\( label, value ) ->
                                    Html.a
                                        [ Html.Attributes.href "#"
                                        , onClick (ConfigMsg (setter value))
                                        , Html.Attributes.class
                                            (if value == getter model then
                                                "selected"

                                             else
                                                "unselected"
                                            )
                                        ]
                                        [ text label ]
                                )
                            |> List.intersperse (text " | ")
                            |> Html.span []
                }


{-| This is the most simple example app, where you want to switch between a discrete set of pre-made configurations.

You need to provide a form label, and a list of labels and possible models.

-}
tabbed : String -> List ( String, model ) -> Configuration model
tabbed label options =
    case options of
        ( initialLabel, initialModel ) :: rest ->
            configuration initialModel [ enumPicker label options identity always ]
                |> makeFormOnTop

        [] ->
            InvalidConfiguration "You need to provide at least one option to Example.tabbed"


{-| This is a type alias for `Program`, mostly useful for type annotations, as not using the type alias
exposes too many implementation details of how this module is implemented.

I recommend using it prefixed:

    main : Example.Program Model

-}
type alias Program model =
    Platform.Program () (Model model) (ConfigMsg model)


{-| Start an application based on a `view` (this is your example) and a `Configuration`.
-}
application : (model -> Html (ConfigMsg model)) -> Configuration model -> Program model
application view_ config =
    case config of
        InvalidConfiguration reason ->
            let
                crash t =
                    crash t
            in
            Browser.application
                { init = \_ url key -> ( Model { key = key, model = Animating { elapsed = 0, interpolation = crash } }, Cmd.none )
                , view = \_ -> { title = reason, body = [ Html.h1 [] [ Html.text "Example Configuration Error" ], Html.p [] [ Html.text reason ] ] }
                , update = \msg model -> ( model, Cmd.none )
                , subscriptions = \_ -> Sub.none
                , onUrlRequest = \_ -> ConfigNoop
                , onUrlChange = \_ -> ConfigNoop
                }

        Configuration details ->
            Browser.application
                { init = \_ url key -> ( Model { key = key, model = Static (parseConfigUrl url details.initialModel details.fields) }, Cmd.none )
                , update =
                    \msg (Model data) ->
                        case msg of
                            ConfigMsg fn ->
                                let
                                    model =
                                        case data.model of
                                            Static m ->
                                                m

                                            Animating { elapsed, interpolation } ->
                                                case details.animation of
                                                    Just ( _, duration ) ->
                                                        interpolation (easeing (toFloat elapsed / toFloat duration))

                                                    Nothing ->
                                                        -- impossible state :(
                                                        interpolation 1

                                    newModel =
                                        fn model

                                    newWrapped =
                                        case details.animation of
                                            Just ( interp, _ ) ->
                                                Animating { interpolation = interp model newModel, elapsed = 0 }

                                            Nothing ->
                                                Static newModel
                                in
                                ( Model { data | model = newWrapped }, Navigation.replaceUrl data.key (serialize details.fields newModel) )

                            Tick delta ->
                                case data.model of
                                    Static m ->
                                        ( Model data, Cmd.none )

                                    Animating animationState ->
                                        case details.animation of
                                            Just ( _, duration ) ->
                                                if duration > animationState.elapsed + delta then
                                                    ( Model { data | model = Animating { animationState | elapsed = animationState.elapsed + delta } }, Cmd.none )

                                                else
                                                    ( Model { data | model = Static (animationState.interpolation 1) }, Cmd.none )

                                            Nothing ->
                                                ( Model { data | model = Static (animationState.interpolation 1) }, Cmd.none )

                            ConfigNoop ->
                                ( Model data, Cmd.none )
                , view =
                    \(Model { model }) ->
                        { title = details.title |> Maybe.withDefault "Example"
                        , body =
                            withCss details.css
                                (case model of
                                    Static m ->
                                        layout details.layout (view_ m) (form details.fields details.title m)

                                    Animating { elapsed, interpolation } ->
                                        case details.animation of
                                            Just ( _, duration ) ->
                                                layout details.layout (view_ (interpolation (easeing (toFloat elapsed / toFloat duration)))) (form details.fields details.title (interpolation 1))

                                            Nothing ->
                                                -- impossible state :(
                                                layout details.layout (view_ (interpolation 1)) (form details.fields details.title (interpolation 1))
                                )
                        }
                , subscriptions =
                    case details.animation of
                        Just _ ->
                            \(Model { model }) ->
                                case model of
                                    Static _ ->
                                        Sub.none

                                    Animating _ ->
                                        Browser.Events.onAnimationFrameDelta (round >> Tick)

                        Nothing ->
                            always Sub.none
                , onUrlRequest = \_ -> ConfigNoop
                , onUrlChange = \_ -> ConfigNoop
                }


layout : Layout -> Html (ConfigMsg model) -> Html (ConfigMsg model) -> Html (ConfigMsg model)
layout expectedLayout userView configForm =
    div
        [ class "example-layout"
        , class
            (case expectedLayout of
                FormOnTop ->
                    "example-layout-vertical"

                FormOnRight ->
                    "example-layout-horizontal"
            )
        ]
        [ configForm
        , userView
        ]


withCss : Maybe String -> Html (ConfigMsg model) -> List (Html (ConfigMsg model))
withCss maybeCss view =
    [ Html.node "style" [ Html.Attributes.type_ "text/css" ] [ Html.text ("""
        .example-form {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
        }
        .example-layout-horizontal {
            display: flex;
            flex-direction: row-reverse;
        }
        .example-form-fields {
            display: table;
        }
        .example-field {
            display: table-row;
        }
        .example-field-label {
            display: table-cell;
            padding-bottom: 8px;
        }
    """ ++ Maybe.withDefault "" maybeCss) ], view ]


easeing : Float -> Float
easeing time =
    let
        t =
            time * 2
    in
    if t <= 1 then
        t ^ 3 / 2

    else
        ((t - 2) ^ 3 + 2) / 2


parseConfigUrl : Url -> model -> List (Field model) -> model
parseConfigUrl url initialModel fields =
    case fields of
        [] ->
            initialModel

        [ Field onlyField ] ->
            case Url.Parser.parse (Url.Parser.fragment identity) { url | path = "/" } of
                Just (Just frag) ->
                    onlyField.setter frag initialModel

                _ ->
                    initialModel

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
                    fn initialModel

                Nothing ->
                    initialModel


serialize : List (Field model) -> model -> String
serialize fields model =
    case fields of
        [ Field singleField ] ->
            "#" ++ singleField.getter model

        _ ->
            List.map (\(Field field) -> Url.Builder.string (stringify field.label) (field.getter model)) fields |> Url.Builder.toQuery


{-| Renders a form for changing the configuration in a vertical layout.

The first argument is a heading for the form.

-}
form : List (Field model) -> Maybe String -> model -> Html (ConfigMsg model)
form fields title model =
    div [ class "example-form" ]
        [ case title of
            Just title_ ->
                Html.h2 [ class "example-form-title" ] [ text title_ ]

            Nothing ->
                text ""
        , div [ class "example-form-fields" ] <|
            List.map
                (\(Field field) ->
                    div [ class "example-field" ]
                        [ Html.label [ class "example-field-label", Html.Attributes.for (stringify field.label) ] [ text field.label ]
                        , field.view model
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


stringify : String -> String
stringify =
    String.toLower
        >> String.trim
        >> String.replace ":" ""
        >> String.replace " " "-"
