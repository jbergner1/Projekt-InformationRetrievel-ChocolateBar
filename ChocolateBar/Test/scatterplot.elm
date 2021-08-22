-- Für Elli
module Main exposing (main)
-- Für lokale Elm-Installation: 
-- module Scatterplot1_4_public exposing (main)

import Axis
import Browser
import Csv
import Csv.Decoder
import Http
import Html exposing (Html)
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Transform(..))


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

wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        extent: (Float, Float)
        extent =
            Maybe.withDefault defaultExtent (Statistics.extent values)
        distance : Float
        distance =
            Tuple.second extent - Tuple.first extent
        offset : Float
        offset = 
            distance / (toFloat(2*tickCount))
    in
        (max (Tuple.first extent - offset) 0, Tuple.second extent + offset)

xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )

yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )

xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)

point : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
point scaleX scaleY xyPoint =
    let
        xPosition = (Scale.convert scaleX xyPoint.x)
        yPosition = (Scale.convert scaleY xyPoint.y)
    in
        g 
            [ class [ "point" ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ] 
            ]
        [ circle 
            [ cx xPosition
            , cy yPosition
            , r radius 
            ] []
        , text_ 
            [ textAnchor AnchorMiddle
            , transform [ Translate xPosition (yPosition - (1.5 * radius)) ] ] [ text xyPoint.pointName ]
        ]

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
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g
            [ transform [ Translate padding (h - padding) ] ]
            [ xAxis xValues,
                TypedSvg.text_
                    [ transform [ Translate (Scale.convert xScaleLocal labelPositions.x) 0, Translate 0 (padding / 2) ]
                    , fontSize <| Px 10.0
                    , fontFamily [ "sans-serif" ]
                    , textAnchor AnchorMiddle]
                    [ TypedSvg.Core.text model.xDescription ]
            ]
        , g
            [ transform [ Translate padding padding ] ]
            [ yAxis yValues,
                TypedSvg.text_
                    [ transform [ Translate 0 (Scale.convert yScaleLocal labelPositions.y), Translate 0 (-padding / 2) ]
                    , fontSize <| Px 10.0
                    , fontFamily [ "sans-serif" ]
                    , textAnchor AnchorMiddle ]
                    [ TypedSvg.Core.text model.yDescription ]
            ]
        , g 
            [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        ]        
 

type alias Point =
    { pointName : String, x : Float, y : Float }


type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }


filterAndReduceCars : List Car -> XyData
filterAndReduceCars my_cars =
    XyData "cityMPG" "retailPrice" (List.filterMap carToMaybePoints cars)

carName : String -> Int -> Int -> Point
carName vehicleName cityMPG retailPrice =
    Point (vehicleName ++ " (" ++ String.fromInt cityMPG ++ ", " ++ String.fromInt retailPrice ++ ")") (toFloat cityMPG) (toFloat retailPrice)

carToMaybePoints : Car -> Maybe Point
carToMaybePoints car =
    Maybe.map3 carName (Just car.vehicleName) car.cityMPG car.retailPrice

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
                    { url = "https://raw.githubusercontent.com/jbergner1/Projekt-InformationRetrievel-ChocolateBar/main/chocolate.csv"
                    , expect = Http.expectString x
                    }

csvString_to_data : String -> List Weine
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeWeine
        |> Result.toMaybe
        |> Maybe.withDefault []

type alias Chocolate =
    { company : String
    , company_location : String
    , review_date : Float
    , country_of_bean_origin : String
    , specific_bean_origin_or_bar_name : String
    , cocoa_percent : Float
    , rating : Float
    , counts_of_ingedients : Float
    , beans : String
    , cocoa_butter : String
    , vanilla : String
    , lecithin : String
    , salt : String
    , sugar : String
    , sweetener_without_sugar : String
    , first_taste : String
    , second_taste : String
    , third_taste : String
    , fourth_taste : String
    }

decodingChocolate : Csv.Decode.Decoder (Chocolate -> a) a
decodingChocolate =
    Csv.Decode.map Chocolate
        (Csv.Decode.field "name" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "company"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "company_location"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "review_date"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "country_of_bean_origin"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "specific_bean_origin_or_bar_name"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "cocoa_percent"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "rating"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "counts_of_ingedients"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "beans"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "cocoa_butter"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "vanilla"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "lecithin"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "salt"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "sugar"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "sweetener_without_sugar"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "first_taste"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "second_taste"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "third_taste"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "fourth_taste"(String.toFloat >> Result.fromMaybe "error parsing string"))
        )