# elm-examples-helper

A simple package that makes elm-visualization examples simpler. You can use it too if you want.

## Installation

```elm
elm install gampleman/elm-examples-helper
```

## What's included?

Some helpers for making examples that do HTTP:

```elm

import Example
import Browser
import Http

type Model
    = Loading
    | Error Http.Error
    | Loaded { data : List String }

type Msg
    = RecievedData (Result Http.Error (List RawBrand))
    | Retry

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : () -> ( Model, Cmd Msg )
init () =
    ( Loading
    , Http.get
        { url = "data.csv"
        , expect = expectCsv RecievedData decoder
        }
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( RecievedData (Ok data), _ ) ->
            ( Loaded { data = data} , Cmd.none)

        ( RecievedData (Err e), _ ) ->
            ( Error e, Cmd.none )

        Retry ->
            init ()

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            Example.loading []

        Error err ->
            Example.error (Just Retry) err

        Loaded data ->
            viewData data
```

An application type for doing tabbed UIs:

```elm
import Example

exampleConfig : List ( String, Model )
exampleConfig =
    [ ( "First page", { page = 1 })
    , ( "Second page", { page = 2 })
    ]

main =
    Example.switchableViews exampleConfig view

view : Model -> Html msg
view model =
    div []
    [ Example.navigation "Pages" exampleConfig
    , h2 [] [text (String.fromInt model.page)]
    ]
```

An application type for having a configurable view:

```elm
import Example
import Color exposing (Color)

type alias Model =
    { fromColorValue : Color
    , toColorValue : Color
    , count : Int
    }


configuration : Example.Configuration Model
configuration =
    Example.configuration
        { fromColorValue = Color.rgb255 0 255 0
        , toColorValue = Color.rgb255 255 2 0
        , count = 50
        }
        [ Example.colorPicker "Start Color" .fromColorValue (\v m -> { m | fromColorValue = v })
        , Example.colorPicker "End Color" .toColorValue (\v m -> { m | toColorValue = v })
        , Example.intSlider "Number of Colors" .count (\v m -> { m | count = v }) 3 100
        ]

view : Model -> Html (Example.ConfigMsg Model)
view model =
    Html.div [ style "display" "flex", style "min-height" "100vh", style "padding-right" "10px" ]
        [ viewColors model
        , Example.verticalPanel "Color Space Interpolations" configuration model
        ]

main =
    Example.configurable configuration view
```

## License

MIT
