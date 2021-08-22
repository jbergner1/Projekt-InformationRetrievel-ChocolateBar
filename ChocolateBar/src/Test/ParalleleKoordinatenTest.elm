module Main exposing (main)

import Axis
import Browser
import Color
import Csv
import Csv.Decode
import Html exposing (Html, a, li, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import List.Extra
import Path
import Scale exposing (ContinuousScale)
import Shape
import Statistics
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))


--MAIN
main =
  Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

--MODEL
type Model
  = Failure
  | Loading
  | Success 
    { data : List Chocolate
    , ersteFunktion : Chocolate -> Float
    , zweiteFunktion : Chocolate -> Float
    , dritteFunktion : Chocolate -> Float
    , vierteFunktion : Chocolate -> Float
    , ersterName : String
    , zweiterName : String
    , dritterName : String
    , vierterName : String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , holenVonCsv GotText
    )

holenVonCsv : (Result Http.Error String -> Msg) -> Cmd Msg
holenVonCsv x = 
    liste
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://raw.githubusercontent.com/jbergner1/Projekt-InformationRetrievel-ChocolateBar/main/" ++ datensatz
                    , expect = Http.expectString x
                    }
            )
        |> Cmd.batch

liste : List String
liste =
    [ "datenMinimal.csv"]

csvString_to_data : String -> List Chocolate
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeChocolate
        |> Result.toMaybe
        |> Maybe.withDefault []

type alias Chocolate =
    { index : String
    , review_date : Float
    , cocoa_percent : Float
    , ref : Float
    , counts_of_ingedients : Float
    }

decodeChocolate : Csv.Decode.Decoder (Chocolate -> a) a
decodeChocolate =
    Csv.Decode.map Chocolate
        (Csv.Decode.field "index" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "review_date"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "cocoa_percent"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "ref"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "counts_of_ingedients"(String.toFloat >> Result.fromMaybe "error parsing string"))
        )


-- UPDATE
type Msg
    = GotText (Result Http.Error String)
    | Ändere1 (Chocolate -> Float, String)
    | Ändere2 (Chocolate -> Float, String)
    | Ändere3 (Chocolate -> Float, String)
    | Ändere4 (Chocolate -> Float, String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = chocolateListe [ fullText ], ersteFunktion = .review_date, zweiteFunktion = .cocoa_percent, dritteFunktion = .ref, vierteFunktion = .counts_of_ingedients , ersterName = "Alkohol", zweiterName = "cocoa_percent", dritterName = "Süße", vierterName = "Säuregehalt"}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        Ändere1 (x, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, ersteFunktion = x, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = a, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = m.vierterName}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        Ändere2 (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = y, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = a, dritterName = m.dritterName, vierterName = m.vierterName}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        Ändere3 (z, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = z, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = a, vierterName = m.vierterName}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        Ändere4 (c, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = c , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = a}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
chocolateListe :List String -> List Chocolate
chocolateListe liste1 =
    List.map(\t -> csvString_to_data t) liste1
        |> List.concat
        
-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount_ : Int
tickCount_ =
    8


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        closeExtent =
            Statistics.extent values
                |> Maybe.withDefault defaultExtent

        extension =
            (Tuple.second closeExtent - Tuple.first closeExtent) / toFloat (2 * tickCount_)
    in
    ( Tuple.first closeExtent - extension |> max 0
    , Tuple.second closeExtent + extension
    )

type alias MultiDimPoint =
    { pointName : String, value : List Float }


type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }



parallelCoodinatesPlot : Float -> Float -> MultiDimData -> Svg msg
parallelCoodinatesPlot w ar model =
    let
        h : Float
        h =
            w / ar

        listTransformieren : List (List Float)
        listTransformieren =
            model.data
                |> List.concat
                |> List.map .value
                |> List.Extra.transpose

        listWideExtent : List ( Float, Float )
        listWideExtent =
            listTransformieren |> List.map wideExtent

        listScale =
            List.map (Scale.linear ( h, 0 )) listWideExtent

        listAxis =
            List.map (Axis.left [ Axis.tickCount tickCount_ ]) listScale

        xScale =
            Scale.linear ( 0, w ) ( 1, List.length model.dimDescription |> toFloat )
    in
    svg
        [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 90
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 90
        ]
    <|
        [ TypedSvg.style []
            []
        , g [ TypedSvg.Attributes.class [ "parallelAxis" ] ]
            [ g [ transform [ Translate (padding - 1) padding ] ] <|
                List.indexedMap
                    (\i axis ->
                        g
                            [ transform
                                [ Translate (Scale.convert xScale (toFloat i + 1)) 0
                                ]
                            ]
                            [ axis ]
                    )
                    listAxis
            , g [ transform [ Translate (padding - 1) 0 ] ] <|
                List.indexedMap
                    (\i desc ->
                        text_
                            [ fontFamily [ "sans-serif" ]
                            , fontSize (Px 10)
                            , x <| Scale.convert xScale (toFloat i + 1)
                            , y <| padding * 7 / 8
                            , textAnchor AnchorMiddle
                            ]
                            [ TypedSvg.Core.text desc ]
                    )
                    model.dimDescription
            ]
        ]
            ++ (let
                    drawPoint p =
                        let
                            linePath : Path.Path
                            linePath =
                                List.map3
                                    (\desc s px ->
                                        Just
                                            ( Scale.convert xScale <| toFloat desc
                                            , Scale.convert s px
                                            )
                                    )
                                    (List.range 1 (List.length model.dimDescription))
                                    listScale
                                    p
                                    |> Shape.line Shape.linearCurve
                        in
                        Path.element linePath
                            [ stroke <| Paint <| Color.rgba 0 0 0 0.8
                            , strokeWidth <| Px 0.5
                            , fill PaintNone
                            ]
                in
                model.data
                    |> List.map
                        (\dataset ->
                            g [ transform [ Translate (padding - 1) padding ] ]
                                (List.map (.value >> drawPoint) dataset)
                        )
               )

view : Model -> Html Msg
view model =
    case model of
        Failure ->
            Html.text "Ich konnte Ihre chocolate nicht öffnen."

        Loading ->
            Html.text "chocolate werden geöffnet..."

        Success l ->
                    let
                        multiDimDaten : List Chocolate -> (Chocolate -> Float) -> (Chocolate -> Float) -> (Chocolate -> Float) -> (Chocolate -> Float) -> (Chocolate -> String) -> String -> String -> String -> String-> MultiDimData
                        multiDimDaten listeChocolate a b c d e f g h i=
                         MultiDimData [f, g, h, i]
                            [ List.map
                                (\x ->
                                    [(a x), (b x), (c x), (d x)]
                                        |> MultiDimPoint (e x)
                                )
                                listeChocolate
                            ]

                        plotDaten = 
                            multiDimDaten l.data l.ersteFunktion l.zweiteFunktion l.dritteFunktion l.vierteFunktion .index l.ersterName l.zweiterName l.dritterName l.vierterName       
                    in
                    Html.div []
                        [
                            ul[][
                                li[][
                                    Html.text <| "Suchen eine Eigenschaft für die erste Spalte aus"
                                    , Html.button [onClick (Ändere1 (.review_date, "Durchschnittlicher Alkoholgehalt"))][Html.text "Alkoholgehalt"]
                                    , Html.button [onClick (Ändere1 (.cocoa_percent, "Durchschnittliche Trinktemperatur"))][Html.text "Trinktemperatur"]
                                    , Html.button [onClick (Ändere1 (.ref, "Süße"))][Html.text "Süße"]
                                    , Html.button [onClick (Ändere1 (.counts_of_ingedients, "Säuregehalt"))][Html.text "Säuregehalt"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Suchen eine Eigenschaft für die zweite Spalte aus"
                                    , Html.button [onClick (Ändere2 (.review_date, "Durchschnittlicher Alkoholgehalt"))][Html.text "Alkoholgehalt"]
                                    , Html.button [onClick (Ändere2 (.cocoa_percent, "Durchschnittliche Trinktemperatur"))][Html.text "Trinktemperatur"]
                                    , Html.button [onClick (Ändere2 (.ref, "Süße"))][Html.text "Süße"]
                                    , Html.button [onClick (Ändere2 (.counts_of_ingedients, "Säuregehalt"))][Html.text "Säuregehalt"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Suchen eine Eigenschaft für die dritte Spalte aus"
                                    , Html.button [onClick (Ändere3 (.review_date, "Durchschnittlicher Alkoholgehalt"))][Html.text "Alkoholgehalt"]
                                    , Html.button [onClick (Ändere3 (.cocoa_percent, "Durchschnittliche Trinktemperatur"))][Html.text "Trinktemperatur"]
                                    , Html.button [onClick (Ändere3 (.ref, "Süße"))][Html.text "Süße"]
                                    , Html.button [onClick (Ändere3 (.counts_of_ingedients, "Säuregehalt"))][Html.text "Säuregehalt"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Suchen eine Eigenschaft für die vierte Spalte aus"
                                    , Html.button [onClick (Ändere4 (.review_date, "Durchschnittlicher Alkoholgehalt"))][Html.text "Alkoholgehalt"]
                                    , Html.button [onClick (Ändere4 (.cocoa_percent, "Durchschnittliche Trinktemperatur"))][Html.text "Trinktemperatur"]
                                    , Html.button [onClick (Ändere4 (.ref, "Süße"))][Html.text "Süße"]
                                    , Html.button [onClick (Ändere4 (.counts_of_ingedients, "Säuregehalt"))][Html.text "Säuregehalt"]
                                ]
                             ]
                                ,parallelCoodinatesPlot 600 2 plotDaten
                        ]