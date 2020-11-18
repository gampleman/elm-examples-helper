# elm-examples-helper

When writing examples either as documentation for packages or for blog posts, one has two conflicting goals:

1. The example should be as short as possible, removing as much boilerplate as can be done, and focusing the example code on the point being made.
2. The running example should look good, be engaging (interactive/animated) and be robust (proper error handling).

The aim of this package is to help you reconcile these goals by adding simple, yet powerful behaviors to your example.

## Installation

```sh
elm install gampleman/elm-examples-helper
```

## What's included?

A `loading` and `error` views suitable for doing HTTP in an example. This allows your example to focus on the "happy path" while still getting a nice loading spinner and a solid error message (with optional retry functionality):

```elm

import Example
import Browser
import Http

type Model
    = Loading
    | Error Http.Error
    | Loaded { data : List String }


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            Example.loading []

        Error err ->
            Example.error (Just Retry) err

        Loaded data ->
            viewData data


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



```

An application type for doing examples, where a view function accepts some config. This application type gives you a form for doing the configuring, while also giving you a url based shareing feature for free.

The simplest form allows you to switch between predefined views:

```elm
import Example

type alias Model =
    { page : Int
    }

view : Model -> Html msg
view model =
    div []
    [ h2 [] [text (String.fromInt model.page)]
    ]

main : Example.Program Model
main =
    Example.tabbed "pages"
        [ ( "First page", { page = 1 })
        , ( "Second page", { page = 2 })
        ]
        |> Example.application view
```

You can of course configure it to use a more complex form, as well as enabling some bells and whistles (like animated transitions):

```elm
import Example
import Color exposing (Color)
import Interpolation

type alias Model =
    { start : Color
    , end : Color
    , count : Int
    }

view : Model -> Html msg
view model =
    yourCoolViewFunction


main : Example.Program Model
main =
    Example.configuration
        { start = Color.rgb255 0 255 0
        , end = Color.rgb255 255 2 0
        , count = 50
        }
        [ Example.colorPicker "Start Color" .fromColorValue (\v m -> { m | fromColorValue = v })
        , Example.colorPicker "End Color" .toColorValue (\v m -> { m | toColorValue = v })
        , Example.intSlider "Number of Colors" { min = 3, max = 100} .count (\v m -> { m | count = v })
        ]
        |> Example.withTitle "My super cool example"
        |> Example.withCustomCss """
            .some-class { opacity: 0.5; }
            """
        |> Example.animatedWith interpolator
        |> Example.application view


interpolator : Model -> Model -> Float -> Model
interpolator from to t =
    { start = Interpolation.hsl from.start to.start t
    , end = Interpolation.hsl from.end to.end t
    , count = Interpolation.count from.count to.count t
    }
```

## License

MIT
