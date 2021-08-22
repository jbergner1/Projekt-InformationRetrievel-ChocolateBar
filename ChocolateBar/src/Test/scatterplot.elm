-- Für Elli
module Main exposing (main)
-- Für lokale Elm-Installation: 
-- module Scatterplot1_4_public exposing (main)

import Axis
import Browser
import Csv
import Csv.Decode
import Http
import Html exposing (Html)
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..), px)
import TypedSvg.Attributes exposing (name)
import Html exposing (ul)
import Html exposing (li)
import Html.Events exposing (onClick)

--Main 

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

--Model
type Model
    = Failure
    | Loading
    | Success
        { data : List Chocolate
        , xAAFunction : (Chocolate -> Float)
        , yAAFunction : (Chocolate -> Float)
        , xName : String
        , yName : String
        }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading 
    , definedDataPath GotText
    )


definedDataPath : (Result Http.Error String -> Msg) -> Cmd Msg
definedDataPath x =
                Http.get
                    { url = "https://raw.githubusercontent.com/jbergner1/Projekt-InformationRetrievel-ChocolateBar/main/DatenÜberarbeitet.csv"
                    , expect = Http.expectString x
                    }

csvString_to_data : String -> List Chocolate
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodingChocolate
        |> Result.toMaybe
        |> Maybe.withDefault []

type alias Chocolate =
    { --company : String
    --, company_location : String
    index : String
    , review_date : Float
    --, country_of_bean_origin : String
    --, specific_bean_origin_or_bar_name : String
    , cocoa_percent : Float
    , ref : Float
    , counts_of_ingedients : Float
    --, beans : String
    --, cocoa_butter : String
    --, vanilla : String
    --, lecithin : String
    --, salt : String
    --, sugar : String
    --, sweetener_without_sugar : String
    --, first_taste : String
    --, second_taste : String
    --, third_taste : String
    --, fourth_taste : String
    }

decodingChocolate : Csv.Decode.Decoder (Chocolate -> a) a
decodingChocolate =
    Csv.Decode.map Chocolate
        (Csv.Decode.field "index" Ok
            --|> Csv.Decode.andMap (Csv.Decode.field "company"(String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "company_location"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "review_date"(String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "country_of_bean_origin"(String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "specific_bean_origin_or_bar_name"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "cocoa_percent"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "ref"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "counts_of_ingedients"(String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "beans"(String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "cocoa_butter"(String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "vanilla"(String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "lecithin"(String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "salt"(String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "sugar"(String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "sweetener_without_sugar"(String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "first_taste"(String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "second_taste"(String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "third_taste"(String.toFloat >> Result.fromMaybe "error parsing string"))
            --|> Csv.Decode.andMap (Csv.Decode.field "fourth_taste"(String.toFloat >> Result.fromMaybe "error parsing string"))
        )

type Msg
    = GotText (Result Http.Error String)
    | ChangeX (Chocolate -> Float, String)
    | ChangeY (Chocolate -> Float, String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = chocolateList [ fullText ], xAAFunction = .review_date, yAAFunction = .ref , xName = "Review Date", yName = "ref"}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


        ChangeX (x, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunction = x, yAAFunction = m.yAAFunction, xName = a, yName = m.yName }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeY (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunction = m.xAAFunction, yAAFunction = y, xName = m.xName, yName = a }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

chocolateList : List String -> List Chocolate
chocolateList list1 =
    List.map (\t -> csvString_to_data t) list1
        |> List.concat

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5

defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )

scatterplot : XyData -> Svg msg
scatterplot model =
    let     
        xValues : List Float
        xValues =
            List.map .x model.data

        yValues : List Float
        yValues =
            List.map .y model.data

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
    in

    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 155,0.05); fill: rgba(0, 0, 155,0.05); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
            
          """ ]
        , g [ transform [ Translate 60 390 ] ]
            [ xAxis xValues
            , text_
                [ x 360 --(Scale.convert xScaleLocal labelPositions.x)
                , y 35

                , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

                --, fontWeight FontWeightBold
                ]
                [ TypedSvg.Core.text model.xDescription ]
            ]
        , g [ transform [ Translate 60 60 ] ]
            [ yAxis yValues
            , text_
                [ x -30
                , y -30

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

                --, fontWeight FontWeightBold
                ]
                [ TypedSvg.Core.text model.yDescription ]
            ]
        , g [ transform [ Translate padding padding ] ]
            
            (List.map (\n -> point xScaleLocal yScaleLocal n model.data) model.data)
        ] 
 
point : ContinuousScale Float -> ContinuousScale Float -> Point  -> List Point -> Svg msg -- List Point
point scaleX scaleY xyPoint pointList=
    g
        [ class [ "point" ]
        , fontSize <| Px 15.0
        , fontFamily [ "serif" ]
        , transform
            [ Translate
                (Scale.convert scaleX xyPoint.x)
                (Scale.convert scaleY xyPoint.y)
            ]
        ]
        [ circle [ cx 0, cy 0, r 3 ] []
        , text_ [ x 10, y -20, textAnchor AnchorMiddle ] [ Html.text xyPoint.pointName ]
        , text_ [ x 10, y -40, textAnchor AnchorMiddle ] [ Html.text <|"Anzahl: " ++ String.fromInt(sumX xyPoint pointList) ]
        ]

type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }


xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )

yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )

addieren : ( Float, Float ) -> Float -> ( Float, Float )
addieren ( min, max ) shift =
    if min <= 0 then
        ( 0, max + shift )

    else
        ( min - shift, max + shift )

wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        result =
            Maybe.withDefault ( 0, 0 )
                (Statistics.extent values)

        max =
            Maybe.withDefault 0
                (List.maximum values)

        result1 =
            addieren result (toFloat tickCount * max / 50)

        result2 =
            addieren result1 0.0
    in
    result2

xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)

filterAndReduceChocolate : List Chocolate -> (Chocolate -> String) -> (Chocolate -> Float) -> (Chocolate -> Float) -> String -> String -> XyData
filterAndReduceChocolate chocolatelist a b c x y =
    XyData x y (List.map (\n -> pointName n a b c x y) chocolatelist)

filterX : Point -> List Point -> List Point
filterX a b =
    let
        isEqual : Point -> Point -> Maybe Point
        isEqual t z =
            if z.x == t.x && z.y == t.y then 
                Just z

            else
                Nothing
    in
    List.filterMap (isEqual a) b  



sumX : Point -> List Point ->  Int
sumX e f=
    List.length (filterX e f) 

type alias Point =
  { pointName : String, x : Float, y : Float }

pointName : Chocolate -> (Chocolate -> String) -> (Chocolate -> Float) -> (Chocolate -> Float) -> String -> String -> Point
pointName choco a b c d e  =
    Point (a choco ++ ", " ++ d ++ ": " ++ String.fromFloat (b choco) ++ "," ++ e ++ ": " ++ String.fromFloat (c choco)) (b choco) (c choco)



view : Model -> Html Msg
view model =
    case model of
        Failure ->
            Html.text "Die Daten konnten nicht geladen werden."

        Loading  ->
            Html.text "Die Daten werden geladen"


        Success l ->
            let 
                choco =
                    filterAndReduceChocolate l.data .index l.xAAFunction l.yAAFunction l.xName l.yName
            in
            Html.div []
                [
                    Html.text <| "Schokolade mit " ++ l.xName ++ " und " ++ l.yName ++ ":" 
                 ,ul []
                    [ li [] [
                            Html.text <| "X-Achse ändern"
                            , Html.button [ onClick (ChangeX (.counts_of_ingedients, "Ingredients")) ] [ Html.text "Ingredients" ]
                            , Html.button [ onClick (ChangeX (.cocoa_percent, "% Cacoa")) ] [ Html.text "% Cacoa" ]
                            , Html.button [ onClick (ChangeX (.review_date, "Review Year")) ] [ Html.text "Review Year" ]
                            , Html.button [ onClick (ChangeX (.ref, "ref")) ] [ Html.text "ref" ]
                            ]
                    ]
                , ul []
                    [ li [] [
                            Html.text <| "Set Y Value"
                            , Html.button [ onClick (ChangeY (.counts_of_ingedients, "Ingredients")) ] [ Html.text "Ingredients" ]
                            , Html.button [ onClick (ChangeY (.cocoa_percent, "% Cacoa")) ] [ Html.text "% Cacoa" ]
                            , Html.button [ onClick (ChangeY (.review_date, "Review Year")) ] [ Html.text "Review Year" ]
                            , Html.button [ onClick (ChangeY (.ref, "ref")) ] [ Html.text "ref" ]
                            ]
                    ]            
                --  , Html.input [ Html.Attributes.placeholder "Age", Html.Attributes.value model.content, onInput Change ]
                , scatterplot choco
                ]