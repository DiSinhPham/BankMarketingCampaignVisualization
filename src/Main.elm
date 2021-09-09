module Main exposing (..)

import Axis
import Browser
import Color
import Csv
import Csv.Decode
import Html exposing (Html, button, div, pre)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Http
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import TypedSvg exposing (circle, g, line, polygon, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, color, d, fill, fontFamily, fontSize, points, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure ErrorDetailed
    | Loading
    | Success ( Http.Metadata, String )


type ErrorDetailed
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String
    | BadBody String


type alias LoadedCSV =
    { url : String
    , contents : String
    }


init : () -> ( ( Model, LoadedCSV, List DemographicDataType ), Cmd Msg )
init _ =
    ( ( Loading, LoadedCSV "default" "default", [ DemoJob, DemoEducation, DemoBalance, DemoAge, DemoMarital ] )
    , Http.get
        { url = "https://raw.githubusercontent.com/DiSinhPham/BankMarketingCampaignVisualization/main/data/bank-full.csv"
        , expect = expectStringDetailed GotText
        }
    )



-- UPDATE


update : Msg -> ( Model, LoadedCSV, List DemographicDataType ) -> ( ( Model, LoadedCSV, List DemographicDataType ), Cmd Msg )
update msg ( model, loadedFile, axisOrder ) =
    let
        newModel =
            Debug.log "model"
                (case msg of
                    GotText result ->
                        case result of
                            Ok detailedResponse ->
                                Success detailedResponse

                            Err detailedError ->
                                Failure detailedError

                    _ ->
                        model
                )

        newAxisOrder =
            Debug.log "test"
                (case msg of
                    Swap_1_2 ->
                        reorderAxis axisOrder 1 False

                    Swap_2_3 ->
                        reorderAxis axisOrder 2 False

                    Swap_3_4 ->
                        reorderAxis axisOrder 3 False

                    Swap_4_5 ->
                        reorderAxis axisOrder 4 False

                    _ ->
                        axisOrder
                )

        updatedFile =
            case newModel of
                Failure error ->
                    loadedFile

                Loading ->
                    loadedFile

                Success ( metadata, body ) ->
                    LoadedCSV metadata.url body
    in
    ( ( newModel, updatedFile, newAxisOrder ), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : ( Model, LoadedCSV, List DemographicDataType ) -> Sub Msg
subscriptions ( model, loadedFile, axisOrder ) =
    Sub.none



-- VIEW


view : ( Model, LoadedCSV, List DemographicDataType ) -> Html Msg
view ( model, loadedFile, axisOrder ) =
    let
        csvStringToHtml : LoadedCSV -> List (Html Msg)
        csvStringToHtml csv =
            Html.p [] [ text csv.url ] :: [ pre [] [ text csv.contents ] ]

        htmlList =
            csvStringToHtml loadedFile

        ------------ Decode CSV
        --Decode Person
        csvStringToPerson : String -> List Person
        csvStringToPerson csvRaw =
            Csv.parseWith ";" loadedFile.contents
                |> Csv.Decode.decodeCsv decodePerson
                |> Result.toMaybe
                |> Maybe.withDefault []

        decodePerson : Csv.Decode.Decoder (Person -> a) a
        decodePerson =
            Csv.Decode.map Person
                (Csv.Decode.field "age" (String.toInt >> Result.fromMaybe "error parsing string")
                    |> Csv.Decode.andMap (Csv.Decode.field "job" (stringToJob >> Result.fromMaybe "error parsing string"))
                    |> Csv.Decode.andMap (Csv.Decode.field "marital" (stringToMarital >> Result.fromMaybe "error parsing string"))
                    |> Csv.Decode.andMap (Csv.Decode.field "education" (stringToEducation >> Result.fromMaybe "error parsing string"))
                    |> Csv.Decode.andMap (Csv.Decode.field "default" (stringYesNoToBool >> Result.fromMaybe "error parsing string"))
                    |> Csv.Decode.andMap (Csv.Decode.field "balance" (String.toInt >> Result.fromMaybe "error parsing string"))
                    |> Csv.Decode.andMap (Csv.Decode.field "housing" (stringYesNoToBool >> Result.fromMaybe "error parsing string"))
                    |> Csv.Decode.andMap (Csv.Decode.field "loan" (stringYesNoToBool >> Result.fromMaybe "error parsing string"))
                    |> Csv.Decode.andMap (Csv.Decode.field "y" (stringToOutcome >> Result.fromMaybe "error parsing string"))
                )

        personList =
            csvStringToPerson loadedFile.contents

        --Decode Campaign Info
        csvStringToCampaignInfo : String -> List CampaignInfo
        csvStringToCampaignInfo csvRaw =
            Csv.parseWith ";" loadedFile.contents
                |> Csv.Decode.decodeCsv decodeCampaignInfo
                |> Result.toMaybe
                |> Maybe.withDefault []

        decodeCampaignInfo : Csv.Decode.Decoder (CampaignInfo -> a) a
        decodeCampaignInfo =
            Csv.Decode.map CampaignInfo
                (Csv.Decode.field "contact" (stringToContact >> Result.fromMaybe "error parsing string")
                    |> Csv.Decode.andMap (Csv.Decode.field "day" (String.toInt >> Result.fromMaybe "error parsing string"))
                    |> Csv.Decode.andMap (Csv.Decode.field "month" (stringMonthToInt >> Result.fromMaybe "error parsing string"))
                    |> Csv.Decode.andMap (Csv.Decode.field "duration" (String.toInt >> Result.fromMaybe "error parsing string"))
                    |> Csv.Decode.andMap (Csv.Decode.field "campaign" (String.toInt >> Result.fromMaybe "error parsing string"))
                    |> Csv.Decode.andMap (Csv.Decode.field "pdays" (String.toInt >> Result.fromMaybe "error parsing string"))
                    |> Csv.Decode.andMap (Csv.Decode.field "previous" (String.toInt >> Result.fromMaybe "error parsing string"))
                    |> Csv.Decode.andMap (Csv.Decode.field "poutcome" (stringToOutcome >> Result.fromMaybe "error parsing string"))
                    |> Csv.Decode.andMap (Csv.Decode.field "y" (stringToOutcome >> Result.fromMaybe "error parsing string"))
                )

        campaignInfoList =
            csvStringToCampaignInfo loadedFile.contents
    in
    case model of
        Failure error ->
            pre [] [ text "Uh oh, there was an error! We can access the metadata and body if the error was \"BadStatus\"" ]

        Loading ->
            text "Loading..."

        Success ( metadata, body ) ->
            div []
                ([ Html.p [] [ Html.text "Bank Marketing Campaign Analysis" ]
                 , button [] [ text "Demographics Analysis" ]
                 , button [] [ text "Campaign Timeline" ]
                 , button [] [ text "Brand Familiarity" ]
                 , Html.p [] []
                 ]
                    ++ demographicPersonalView personList axisOrder
                )



-- Demographic View


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


demographicPersonalView : List Person -> List DemographicDataType -> List (Html Msg)
demographicPersonalView clientList axisOrder =
    let
        -- Full List for Scales
        multiDimClientList =
            createMultiDimData clientList axisOrder

        -- List separated by Outcome
        multiDimAcceptedClientList =
            createMultiDimData (List.filter (\n -> n.output == Accepted) clientList) axisOrder

        multiDimDeclinedClientList =
            createMultiDimData (List.filter (\n -> n.output == Declined) clientList) axisOrder

        xValues : List Float
        xValues =
            List.map (\n -> toFloat n) (List.range 1 (List.length multiDimClientList.dimDescription))

        xScale : ContinuousScale Float
        xScale =
            Scale.linear ( 0, w - 2 * padding ) (wideExtent xValues 5)

        lineGenerator : ( Float, Float ) -> Maybe ( Float, Float )
        lineGenerator ( x, y ) =
            Just ( x, y )

        lineFromFloats : List Float -> List (ContinuousScale Float) -> List Float -> Path
        lineFromFloats x s y =
            List.map3 (\i j k -> ( Scale.convert xScale i, Scale.convert j k )) x s y |> List.map lineGenerator |> Shape.line Shape.linearCurve

        acceptedLineList : List Path
        acceptedLineList =
            List.map (lineFromFloats xValues multiDimClientList.dimScales) (List.map (\n -> n.value) multiDimAcceptedClientList.data)

        declinedLineList : List Path
        declinedLineList =
            List.map (lineFromFloats xValues multiDimClientList.dimScales) (List.map (\n -> n.value) multiDimDeclinedClientList.data)
    in
    [ div [] []
    , Html.p [] [ Html.text "Job: (0 - Unknown)  (1 - Unemployed)  (2 - Retired)  (3 - Student)  (4 - Self-Employed)  (5 - Housemaid)  (6 - Blue-Collar)  (7 - Technician)  (8 - Services)  (9 - Management)  (10 - Admin)  (11 - Entepreneur)" ]
    , Html.p [] [ Html.text "Martial: (0 - Single)  (1 - Divorced)  (2 - Married)" ]
    , Html.p [] [ Html.text "Education: (0 - Unknown)  (1 - Primary)  (2 - Secondary)  (3 - Tertiary)" ]
    , svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        ([ style [] [ TypedSvg.Core.text """
            .frame rect { stroke: rgba(0, 0, 0,1); fill: rgba(0, 0, 0,1.0);}
              """ ]
         , g
            [ class [ "frame" ]
            ]
            [ rect [ x 0, y 0, width w, height h ] []
            ]
         , g
            [ class [ "diagramlabel" ]
            , transform [ Translate (2 * padding) (h - (padding / 4)) ]
            , stroke (Paint Color.white)
            , strokeWidth (Px 0.5)
            , fill (Paint Color.white)
            , color Color.white
            ]
            [ text_ [ x 0, y -10, fontFamily [ "sans-serif" ], fontSize (TypedSvg.Types.px 20), textAnchor AnchorMiddle ] [ text "Successful Contract" ] ]
         ]
            ++ List.map (drawLine True) acceptedLineList
            ++ drawAxisFromOrder multiDimClientList xScale axisOrder
        )
    , div []
        [ Html.text (" #1: " ++ (Maybe.withDefault "" <| Maybe.andThen demoDataTypeToString <| List.head axisOrder) ++ " ")
        , button [ onClick Swap_1_2 ] [ text "◄►" ]
        , Html.text (" #2: " ++ (Maybe.withDefault "" <| Maybe.andThen demoDataTypeToString <| List.head (List.drop 1 axisOrder)) ++ " ")
        , button [ onClick Swap_2_3 ] [ text "◄►" ]
        , Html.text (" #3: " ++ (Maybe.withDefault "" <| Maybe.andThen demoDataTypeToString <| List.head (List.drop 2 axisOrder)) ++ " ")
        , button [ onClick Swap_3_4 ] [ text "◄►" ]
        , Html.text (" #4: " ++ (Maybe.withDefault "" <| Maybe.andThen demoDataTypeToString <| List.head (List.drop 3 axisOrder)) ++ " ")
        , button [ onClick Swap_4_5 ] [ text "◄►" ]
        , Html.text (" #5: " ++ (Maybe.withDefault "" <| Maybe.andThen demoDataTypeToString <| List.head (List.drop 4 axisOrder)) ++ " ")
        ]
    , svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        ([ style [] [ TypedSvg.Core.text """
            .frame rect { stroke: rgba(0, 0, 0,1); fill: rgba(0, 0, 0,1.0);}
              """ ]
         , g
            [ class [ "frame" ]
            ]
            [ rect [ x 0, y 0, width w, height h ] []
            ]
         , g
            [ class [ "diagramlabel" ]
            , transform [ Translate (2 * padding) (h - (padding / 4)) ]
            , stroke (Paint Color.white)
            , strokeWidth (Px 0.5)
            , fill (Paint Color.white)
            , color Color.white
            ]
            [ text_ [ x 0, y -10, fontFamily [ "sans-serif" ], fontSize (TypedSvg.Types.px 20), textAnchor AnchorMiddle ] [ text "Failed Contract" ] ]
         ]
            ++ List.map (drawLine False) declinedLineList
            ++ drawAxisFromOrder multiDimClientList xScale axisOrder
        )
    ]


type DemographicDataType
    = DemoAge
    | DemoJob
    | DemoMarital
    | DemoEducation
    | DemoBalance


type alias MultiDimPoint =
    { pointName : String, value : List Float }


type alias MultiDimData =
    { dimDescription : List String
    , dimScales : List (ContinuousScale Float)
    , data : List MultiDimPoint
    }



--Create the Multidim Data in the given Axis Order


createMultiDimData : List Person -> List DemographicDataType -> MultiDimData
createMultiDimData clientList axisOrder =
    let
        firstFloatList =
            getFloatListFromDemoDataType (Maybe.withDefault DemoAge (List.head axisOrder)) clientList

        secondFloatList =
            getFloatListFromDemoDataType (Maybe.withDefault DemoAge (List.head (List.drop 1 axisOrder))) clientList

        thirdFloatList =
            getFloatListFromDemoDataType (Maybe.withDefault DemoAge (List.head (List.drop 2 axisOrder))) clientList

        fourthFloatList =
            getFloatListFromDemoDataType (Maybe.withDefault DemoAge (List.head (List.drop 3 axisOrder))) clientList

        fifthFloatList =
            getFloatListFromDemoDataType (Maybe.withDefault DemoAge (List.head (List.drop 4 axisOrder))) clientList

        firstScale =
            Scale.linear ( h - 2 * padding, 0 ) (wideExtent firstFloatList 11)

        secondScale =
            Scale.linear ( h - 2 * padding, 0 ) (wideExtent secondFloatList 4)

        thirdScale =
            Scale.linear ( h - 2 * padding, 0 ) (wideExtent thirdFloatList 9)

        fourthScale =
            Scale.linear ( h - 2 * padding, 0 ) (wideExtent fourthFloatList 10)

        fifthScale =
            Scale.linear ( h - 2 * padding, 0 ) (wideExtent fifthFloatList 3)

        scaleList =
            [ firstScale, secondScale, thirdScale, fourthScale, fifthScale ]
    in
    MultiDimData (List.filterMap demoDataTypeToString axisOrder) scaleList (List.map5 convertFloatsToMultiDimPoint firstFloatList secondFloatList thirdFloatList fourthFloatList fifthFloatList)



-- Creates Multidim Point from 5 Floats


convertFloatsToMultiDimPoint : Float -> Float -> Float -> Float -> Float -> MultiDimPoint
convertFloatsToMultiDimPoint vValue wValue xValue yValue zValue =
    MultiDimPoint "" [ vValue, wValue, xValue, yValue, zValue ]



--Returns a Float list depending on the provided DataType


getFloatListFromDemoDataType : DemographicDataType -> List Person -> List Float
getFloatListFromDemoDataType demographicDataType clientList =
    case demographicDataType of
        DemoAge ->
            List.map (\n -> toFloat n.age) clientList

        DemoJob ->
            List.map (\n -> toFloat (jobToInt n.job)) clientList

        DemoMarital ->
            List.map (\n -> toFloat (maritalToInt n.marital)) clientList

        DemoEducation ->
            List.map (\n -> toFloat (educationToInt n.education)) clientList

        DemoBalance ->
            List.map (\n -> toFloat n.balance) clientList



--Turns DemoDataType into string for axis labels and other output.


demoDataTypeToString : DemographicDataType -> Maybe String
demoDataTypeToString demoDataType =
    case demoDataType of
        DemoAge ->
            Just "Age"

        DemoJob ->
            Just "Job"

        DemoMarital ->
            Just "Marital"

        DemoEducation ->
            Just "Education"

        DemoBalance ->
            Just "Balance"



-- Draw Axist from Axis Order


drawAxisFromOrder : MultiDimData -> ContinuousScale Float -> List DemographicDataType -> List (Svg msg)
drawAxisFromOrder clientList xScale axisOrder =
    let
        firstAxis =
            axisFromDemoDataType clientList xScale (Maybe.withDefault DemoAge (List.head axisOrder)) 1

        secondAxis =
            axisFromDemoDataType clientList xScale (Maybe.withDefault DemoAge (List.head (List.drop 1 axisOrder))) 2

        thirdAxis =
            axisFromDemoDataType clientList xScale (Maybe.withDefault DemoAge (List.head (List.drop 2 axisOrder))) 3

        fourthAxis =
            axisFromDemoDataType clientList xScale (Maybe.withDefault DemoAge (List.head (List.drop 3 axisOrder))) 4

        fifthAxis =
            axisFromDemoDataType clientList xScale (Maybe.withDefault DemoAge (List.head (List.drop 4 axisOrder))) 5
    in
    [ firstAxis, secondAxis, thirdAxis, fourthAxis, fifthAxis ]


axisFromDemoDataType : MultiDimData -> ContinuousScale Float -> DemographicDataType -> Int -> Svg msg
axisFromDemoDataType clientList xScale dataType axisNumber =
    case dataType of
        DemoAge ->
            g
                [ class [ "axis" ]
                , transform [ Translate (Scale.convert xScale (toFloat axisNumber) + padding) padding ]
                , stroke (Paint Color.white)
                , strokeWidth (Px 0.5)
                , fill (Paint Color.white)
                , color Color.white
                ]
                [ Axis.left [ Axis.tickCount 10 ] (Maybe.withDefault xScale (List.head (List.drop (axisNumber - 1) clientList.dimScales))), text_ [ x 0, y -10, fontFamily [ "sans-serif" ], fontSize (TypedSvg.Types.px 20), textAnchor AnchorMiddle ] [ text (Maybe.withDefault "" (List.head (List.drop (axisNumber - 1) clientList.dimDescription))) ] ]

        DemoJob ->
            g
                [ class [ "axis" ]
                , transform [ Translate (Scale.convert xScale (toFloat axisNumber) + padding) padding ]
                , stroke (Paint Color.white)
                , strokeWidth (Px 0.5)
                , fill (Paint Color.white)
                , color Color.white
                ]
                [ Axis.left [ Axis.tickCount 11 ] (Maybe.withDefault xScale (List.head (List.drop (axisNumber - 1) clientList.dimScales))), text_ [ x 0, y -10, fontFamily [ "sans-serif" ], fontSize (TypedSvg.Types.px 20), textAnchor AnchorMiddle ] [ text (Maybe.withDefault "" (List.head (List.drop (axisNumber - 1) clientList.dimDescription))) ] ]

        DemoMarital ->
            g
                [ class [ "axis" ]
                , transform [ Translate (Scale.convert xScale (toFloat axisNumber) + padding) padding ]
                , stroke (Paint Color.white)
                , strokeWidth (Px 0.5)
                , fill (Paint Color.white)
                , color Color.white
                ]
                [ Axis.left [ Axis.tickCount 3 ] (Maybe.withDefault xScale (List.head (List.drop (axisNumber - 1) clientList.dimScales))), text_ [ x 0, y -10, fontFamily [ "sans-serif" ], fontSize (TypedSvg.Types.px 20), textAnchor AnchorMiddle ] [ text (Maybe.withDefault "" (List.head (List.drop (axisNumber - 1) clientList.dimDescription))) ] ]

        DemoEducation ->
            g
                [ class [ "axis" ]
                , transform [ Translate (Scale.convert xScale (toFloat axisNumber) + padding) padding ]
                , stroke (Paint Color.white)
                , strokeWidth (Px 0.5)
                , fill (Paint Color.white)
                , color Color.white
                ]
                [ Axis.left [ Axis.tickCount 4 ] (Maybe.withDefault xScale (List.head (List.drop (axisNumber - 1) clientList.dimScales))), text_ [ x 0, y -10, fontFamily [ "sans-serif" ], fontSize (TypedSvg.Types.px 20), textAnchor AnchorMiddle ] [ text (Maybe.withDefault "" (List.head (List.drop (axisNumber - 1) clientList.dimDescription))) ] ]

        DemoBalance ->
            g
                [ class [ "axis" ]
                , transform [ Translate (Scale.convert xScale (toFloat axisNumber) + padding) padding ]
                , stroke (Paint Color.white)
                , strokeWidth (Px 0.5)
                , fill (Paint Color.white)
                , color Color.white
                ]
                [ Axis.left [ Axis.tickCount 9 ] (Maybe.withDefault xScale (List.head (List.drop (axisNumber - 1) clientList.dimScales))), text_ [ x 0, y -10, fontFamily [ "sans-serif" ], fontSize (TypedSvg.Types.px 20), textAnchor AnchorMiddle ] [ text (Maybe.withDefault "" (List.head (List.drop (axisNumber - 1) clientList.dimDescription))) ] ]



-- Reorder Axis


reorderAxis : List DemographicDataType -> Int -> Bool -> List DemographicDataType
reorderAxis baseList axis up =
    let
        firstPart =
            if up then
                List.take (axis - 2) baseList

            else
                List.take (axis - 1) baseList

        secondPart =
            if up then
                List.take 1 <| List.drop (axis - 1) baseList

            else
                List.take 1 <| List.drop axis baseList

        thirdPart =
            if up then
                List.take 1 <| List.drop (axis - 2) baseList

            else
                List.take 1 <| List.drop (axis - 1) baseList

        -- a b c d e
        -- a b c e d
        fourthPart =
            if up then
                List.drop axis baseList

            else
                List.drop (axis + 1) baseList
    in
    List.concat [ firstPart, secondPart, thirdPart, fourthPart ]



-- Draw Line


drawLine : Bool -> Path -> Svg msg
drawLine accepted line =
    case accepted of
        True ->
            g [ transform [ Translate padding padding ], class [ "series" ] ]
                [ Path.element line [ stroke (Paint (Color.rgba 0 0 1 0.8)), strokeWidth (Px 0.1), fill PaintNone ]
                ]

        False ->
            g [ transform [ Translate padding padding ], class [ "series2" ] ]
                [ Path.element line [ stroke (Paint (Color.rgba 1 0 0 0.8)), strokeWidth (Px 0.1), fill PaintNone ]
                ]



-- Calculate Extents for Axis


wideExtent : List Float -> Int -> ( Float, Float )
wideExtent values tickCount =
    let
        minimum =
            Maybe.withDefault 0 (List.minimum values)

        maximum =
            Maybe.withDefault 100 (List.maximum values)

        extraRange =
            (maximum - minimum) / toFloat (2 * tickCount)
    in
    ( minimum, maximum )



-- HTTP Responses


convertResponseString : Http.Response String -> Result ErrorDetailed ( Http.Metadata, String )
convertResponseString httpResponse =
    case httpResponse of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err (BadStatus metadata body)

        Http.GoodStatus_ metadata body ->
            Ok ( metadata, body )


expectStringDetailed : (Result ErrorDetailed ( Http.Metadata, String ) -> msg) -> Http.Expect msg
expectStringDetailed msg =
    Http.expectStringResponse msg convertResponseString


type Msg
    = GotText (Result ErrorDetailed ( Http.Metadata, String ))
    | Swap_1_2
    | Swap_2_3
    | Swap_3_4
    | Swap_4_5



-- DATA STRUCTURE


type alias Person =
    { age : Int
    , job : Job
    , marital : Marital
    , education : Education
    , default : Bool
    , balance : Int
    , housing : Bool
    , loan : Bool
    , output : Outcome
    }


type alias CampaignInfo =
    { contactType : Contact
    , day : Int
    , month : Int
    , duration : Int
    , contactCount : Int
    , pdays : Int
    , pcontactCount : Int
    , poutcome : Outcome
    , output : Outcome
    }


type alias CampaignFull =
    { person : Person
    , campaignInfo : CampaignInfo
    }


type Job
    = Admin
    | Unemployed
    | Management
    | Housemaid
    | Entrepreneur
    | Student
    | BlueCollar
    | SelfEmployed
    | Retired
    | Technician
    | Services
    | UnknownJob


type Marital
    = Married
    | Divorced
    | Single


type Education
    = Primary
    | Secondary
    | Tertiary
    | UnknownEd


type Contact
    = Telephone
    | Cellular
    | UnknownContact


type Outcome
    = Accepted
    | Declined
    | Other
    | UnknownOutcome



-- DECODER FUNCTIONS


stringToJob : String -> Maybe Job
stringToJob jobname =
    case jobname of
        "admin." ->
            Just Admin

        "unemployed" ->
            Just Unemployed

        "management" ->
            Just Management

        "housemaid" ->
            Just Housemaid

        "entrepreneur" ->
            Just Entrepreneur

        "student" ->
            Just Student

        "blue-collar" ->
            Just BlueCollar

        "self-employed" ->
            Just SelfEmployed

        "retired" ->
            Just Retired

        "technician" ->
            Just Technician

        "services" ->
            Just Services

        "unknown" ->
            Just UnknownJob

        _ ->
            Nothing


stringToMarital : String -> Maybe Marital
stringToMarital marital =
    case marital of
        "married" ->
            Just Married

        "divorced" ->
            Just Divorced

        "single" ->
            Just Single

        _ ->
            Nothing


stringToOutcome : String -> Maybe Outcome
stringToOutcome outcome =
    case outcome of
        "yes" ->
            Just Accepted

        "success" ->
            Just Accepted

        "no" ->
            Just Declined

        "failure" ->
            Just Declined

        "other" ->
            Just Other

        "unknown" ->
            Just UnknownOutcome

        _ ->
            Nothing


stringToEducation : String -> Maybe Education
stringToEducation education =
    case education of
        "primary" ->
            Just Primary

        "secondary" ->
            Just Secondary

        "tertiary" ->
            Just Tertiary

        "unknown" ->
            Just UnknownEd

        _ ->
            Nothing


stringToContact : String -> Maybe Contact
stringToContact contact =
    case contact of
        "telephone" ->
            Just Telephone

        "cellular" ->
            Just Cellular

        "unknown" ->
            Just UnknownContact

        _ ->
            Nothing


stringYesNoToBool : String -> Maybe Bool
stringYesNoToBool string =
    case string of
        "yes" ->
            Just True

        "no" ->
            Just False

        _ ->
            Nothing


stringMonthToInt : String -> Maybe Int
stringMonthToInt month =
    case month of
        "jan" ->
            Just 1

        "feb" ->
            Just 2

        "mar" ->
            Just 3

        "apr" ->
            Just 4

        "may" ->
            Just 5

        "jun" ->
            Just 6

        "jul" ->
            Just 7

        "aug" ->
            Just 8

        "sep" ->
            Just 9

        "oct" ->
            Just 10

        "nov" ->
            Just 11

        "dec" ->
            Just 12

        _ ->
            Nothing



-- CONVERSION FUNCTIONS


jobToInt : Job -> Int
jobToInt job =
    case job of
        UnknownJob ->
            0

        Unemployed ->
            1

        Retired ->
            2

        Student ->
            3

        SelfEmployed ->
            4

        Housemaid ->
            5

        BlueCollar ->
            6

        Technician ->
            7

        Services ->
            8

        Management ->
            9

        Admin ->
            10

        Entrepreneur ->
            11


maritalToInt : Marital -> Int
maritalToInt marital =
    case marital of
        Single ->
            0

        Divorced ->
            1

        Married ->
            2


educationToInt : Education -> Int
educationToInt education =
    case education of
        UnknownEd ->
            0

        Primary ->
            1

        Secondary ->
            2

        Tertiary ->
            3
