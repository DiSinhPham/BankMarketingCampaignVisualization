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
import String exposing (fromFloat)
import Task exposing (fail)
import Tree exposing (Tree)
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
            case msg of
                GotText result ->
                    case result of
                        Ok detailedResponse ->
                            Success detailedResponse

                        Err detailedError ->
                            Failure detailedError

                _ ->
                    model

        newAxisOrder =
            case msg of
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
                    ++ demographicFinanceView personList
                )



-- Financial Tree View


demographicFinanceView : List Person -> List (Html Msg)
demographicFinanceView clientList =
    let
        --Split Success/Fail
        ( s, f ) =
            List.partition (\n -> outcomeToBool n.output) clientList

        --Split Default
        ( sD, sd ) =
            List.partition (\n -> n.default) s

        ( fD, fd ) =
            List.partition (\n -> n.default) f

        --Split Housing
        ( sDH, sDh ) =
            List.partition (\n -> n.housing) sD

        ( sdH, sdh ) =
            List.partition (\n -> n.housing) sd

        ( fDH, fDh ) =
            List.partition (\n -> n.housing) fD

        ( fdH, fdh ) =
            List.partition (\n -> n.housing) fd

        --Split Loan
        ( sDHL, sDHl ) =
            List.partition (\n -> n.loan) sDH

        ( sDhL, sDhl ) =
            List.partition (\n -> n.loan) sDh

        ( sdHL, sdHl ) =
            List.partition (\n -> n.loan) sdH

        ( sdhL, sdhl ) =
            List.partition (\n -> n.loan) sdh

        ( fDHL, fDHl ) =
            List.partition (\n -> n.loan) fDH

        ( fDhL, fDhl ) =
            List.partition (\n -> n.loan) fDh

        ( fdHL, fdHl ) =
            List.partition (\n -> n.loan) fdH

        ( fdhL, fdhl ) =
            List.partition (\n -> n.loan) fdh

        --Tree
        clientTree =
            Tree.tree ( "all", List.length clientList )
                [ Tree.tree ( "s", List.length s )
                    [ Tree.tree ( "sD", List.length sD )
                        [ Tree.tree ( "sDH", List.length sDH )
                            [ Tree.singleton ( "sDHL", List.length sDHL )
                            , Tree.singleton ( "sDHl", List.length sDHl )
                            ]
                        , Tree.tree ( "sDh", List.length sDh )
                            [ Tree.singleton ( "sDhL", List.length sDhL )
                            , Tree.singleton ( "sDhl", List.length sDhl )
                            ]
                        ]
                    , Tree.tree ( "sd", List.length sd )
                        [ Tree.tree ( "sdH", List.length sdH )
                            [ Tree.singleton ( "sdHL", List.length sdHL )
                            , Tree.singleton ( "sdHl", List.length sdHl )
                            ]
                        , Tree.tree ( "sdh", List.length sdh )
                            [ Tree.singleton ( "sdhL", List.length sdhL )
                            , Tree.singleton ( "sdhl", List.length sdhl )
                            ]
                        ]
                    ]
                , Tree.tree ( "f", List.length f )
                    [ Tree.tree ( "fD", List.length fD )
                        [ Tree.tree ( "fDH", List.length fDH )
                            [ Tree.singleton ( "fDHL", List.length fDHL )
                            , Tree.singleton ( "fDHl", List.length fDHl )
                            ]
                        , Tree.tree ( "fDh", List.length fDh )
                            [ Tree.singleton ( "fDhL", List.length fDhL )
                            , Tree.singleton ( "fDhl", List.length fDhl )
                            ]
                        ]
                    , Tree.tree ( "fd", List.length fd )
                        [ Tree.tree ( "fdH", List.length fdH )
                            [ Tree.singleton ( "fdHL", List.length fdHL )
                            , Tree.singleton ( "fdHl", List.length fdHl )
                            ]
                        , Tree.tree ( "fdh", List.length fdh )
                            [ Tree.singleton ( "fdhL", List.length fdhL )
                            , Tree.singleton ( "fdhl", List.length fdhl )
                            ]
                        ]
                    ]
                ]

        successTree =
            Tree.tree ( "s", List.length s )
                [ Tree.tree ( "sD", List.length sD )
                    [ Tree.tree ( "sDH", List.length sDH )
                        [ Tree.singleton ( "sDHL", List.length sDHL )
                        , Tree.singleton ( "sDHl", List.length sDHl )
                        ]
                    , Tree.tree ( "sDh", List.length sDh )
                        [ Tree.singleton ( "sDhL", List.length sDhL )
                        , Tree.singleton ( "sDhl", List.length sDhl )
                        ]
                    ]
                , Tree.tree ( "sd", List.length sd )
                    [ Tree.tree ( "sdH", List.length sdH )
                        [ Tree.singleton ( "sdHL", List.length sdHL )
                        , Tree.singleton ( "sdHl", List.length sdHl )
                        ]
                    , Tree.tree ( "sdh", List.length sdh )
                        [ Tree.singleton ( "sdhL", List.length sdhL )
                        , Tree.singleton ( "sdhl", List.length sdhl )
                        ]
                    ]
                ]

        failTree =
            Tree.tree ( "f", List.length f )
                [ Tree.tree ( "fD", List.length fD )
                    [ Tree.tree ( "fDH", List.length fDH )
                        [ Tree.singleton ( "fDHL", List.length fDHL )
                        , Tree.singleton ( "fDHl", List.length fDHl )
                        ]
                    , Tree.tree ( "fDh", List.length fDh )
                        [ Tree.singleton ( "fDhL", List.length fDhL )
                        , Tree.singleton ( "fDhl", List.length fDhl )
                        ]
                    ]
                , Tree.tree ( "fd", List.length fd )
                    [ Tree.tree ( "fdH", List.length fdH )
                        [ Tree.singleton ( "fdHL", List.length fdHL )
                        , Tree.singleton ( "fdHl", List.length fdHl )
                        ]
                    , Tree.tree ( "fdh", List.length fdh )
                        [ Tree.singleton ( "fdhL", List.length fdhL )
                        , Tree.singleton ( "fdhl", List.length fdhl )
                        ]
                    ]
                ]
    in
    [ div []
        [ Html.p [] [ Html.text "Distribution of Loan Situation" ]
        , Html.p [] [ Html.text "Complete Tree" ]
        , drawTreemapSimple clientTree True
        , Html.p [] [ Html.text "Separated Trees" ]
        , div []
            [ drawTreemapSimple successTree False
            , drawTreemapSimple failTree False
            ]
        ]
    ]


drawTreemapSimple : Tree ( String, Int ) -> Bool -> Html Msg
drawTreemapSimple t splitX =
    let
        w =
            1000

        h =
            1000

        padding =
            100
    in
    svg
        [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 50
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 50
        , TypedSvg.Attributes.preserveAspectRatio (TypedSvg.Types.Align TypedSvg.Types.ScaleMin TypedSvg.Types.ScaleMin) TypedSvg.Types.Slice
        ]
        [ style [] [ TypedSvg.Core.text """
                .rect1:hover rect {stroke: rgba(0, 0, 0,1.0); fill: rgba(255, 255, 255, 1);  }
                .rect1 text { display: none; }
                .rect1:hover text { display: inline; font: bold 30px sans-serif;}
              """ ]
        , TypedSvg.g [ TypedSvg.Attributes.transform [ Translate padding padding ] ] <| drawTreeNode splitX 0 0 w h t
        ]


drawTreeNode : Bool -> Float -> Float -> Float -> Float -> Tree ( String, Int ) -> List (Svg Msg)
drawTreeNode splitX x y w h t =
    let
        -- Aufbereiten von Kind-Listen um diese iterativ abzuarbeiten
        --Liste der Kinder
        children =
            Tree.children t

        --Summe der Kindervalues. Theoretisch das Wert Labels des ausgewählten Elternknoten.
        sumOfChildren =
            toFloat (List.sum (List.map (\( n, v ) -> v) (List.map Tree.label children)))

        --Anteile der Kindknoten für die aktuelle Split-Achse (für Skalierung)
        childSpaceAllocation =
            List.map (\( n, v ) -> toFloat v / sumOfChildren) (List.map Tree.label children)

        --Breite der Kindknoten basierend auf deren Prozentualen Anteile
        childW =
            if splitX then
                List.map (\n -> (w * 0.9) * n) childSpaceAllocation

            else
                List.repeat (List.length children) (w * 0.9)

        --Höhe der Kindknoten basierend auf deren Prozentualen Anteile
        childH =
            if splitX then
                List.repeat (List.length children) (h * 0.9)

            else
                List.map (\n -> (h * 0.9) * n) childSpaceAllocation

        --X-Position der Kindknoten berechnet mithilfe der Breiten
        childX =
            if splitX then
                List.indexedMap (\i n -> (x + (w * 0.025)) + List.sum (List.take i childW) + (toFloat i * (w * 0.025))) childW

            else
                List.repeat (List.length children) (x + (0.05 * w))

        --Y-Position der Kindknoten berechnet mithilfe der Höhen
        childY =
            if splitX then
                List.repeat (List.length children) (y + (0.05 * h))

            else
                List.indexedMap (\i n -> (y + (h * 0.025)) + List.sum (List.take i childH) + (toFloat i * (h * 0.025))) childH

        --Kinder werden iterativ abgearbeitet. Für jedes Kind wird die Funktion rekursiv aufgerufen.
        result =
            if List.length children > 0 then
                [ g
                    [ class [ "rect1" ]
                    , fontSize <| Px 30.0
                    , fontFamily [ "sans-serif" ]
                    ]
                    [ TypedSvg.rect
                        [ TypedSvg.Attributes.x <| TypedSvg.Types.px x
                        , TypedSvg.Attributes.y <| TypedSvg.Types.px y
                        , TypedSvg.Attributes.width <| TypedSvg.Types.px w
                        , TypedSvg.Attributes.height <| TypedSvg.Types.px h
                        , TypedSvg.Attributes.stroke <| TypedSvg.Types.Paint Color.black
                        , TypedSvg.Attributes.fill <| TypedSvg.Types.Paint (getColorFromStringCode (Tuple.first (Tree.label t)))
                        ]
                        []
                    , text_
                        [ TypedSvg.Attributes.x <| TypedSvg.Types.px 0
                        , TypedSvg.Attributes.y <| TypedSvg.Types.px -10
                        , textAnchor TypedSvg.Types.AnchorStart
                        ]
                        [ text (getStringCodeToLabel (Tuple.first (Tree.label t))) ]
                    ]
                ]
                    ++ List.concat (List.map5 (drawTreeNode (not splitX)) childX childY childW childH children)

            else
                [ g
                    [ class [ "rect1" ]
                    , fontSize <| Px 30.0
                    , fontFamily [ "sans-serif" ]
                    ]
                    [ TypedSvg.rect
                        [ TypedSvg.Attributes.x <| TypedSvg.Types.px x
                        , TypedSvg.Attributes.y <| TypedSvg.Types.px y
                        , TypedSvg.Attributes.width <| TypedSvg.Types.px w
                        , TypedSvg.Attributes.height <| TypedSvg.Types.px h
                        , TypedSvg.Attributes.stroke <| TypedSvg.Types.Paint Color.black
                        , TypedSvg.Attributes.fill <| TypedSvg.Types.Paint (getColorFromStringCode (Tuple.first (Tree.label t)))
                        ]
                        []
                    , text_
                        [ TypedSvg.Attributes.x <| TypedSvg.Types.px 0
                        , TypedSvg.Attributes.y <| TypedSvg.Types.px -10
                        , textAnchor TypedSvg.Types.AnchorStart
                        ]
                        [ text (getStringCodeToLabel (Tuple.first (Tree.label t))) ]
                    ]
                ]
    in
    result


getColorFromStringCode : String -> Color.Color
getColorFromStringCode code =
    case String.length code of
        1 ->
            if String.contains "s" code then
                Color.rgba 0 0 1 1

            else
                Color.rgba 0 0 0.5 1

        2 ->
            if String.contains "D" code then
                Color.rgba 1 0 0 1

            else
                Color.rgba 0.5 0 0 1

        3 ->
            if String.contains "H" code then
                Color.rgba 0 1 1 1

            else
                Color.rgba 0 0.5 0.5 1

        4 ->
            if String.contains "L" code then
                Color.rgba 1 1 0 1

            else
                Color.rgba 0.5 0.5 0 1

        _ ->
            Color.rgba 0.5 0.5 0.5 1


getStringCodeToLabel : String -> String
getStringCodeToLabel code =
    case code of
        "all" ->
            "Full Data Set"

        "s" ->
            "Success"

        "sD" ->
            "Success - Default: Yes"

        "sd" ->
            "Success - Default: No"

        "sDH" ->
            "Success - Default: Yes - House Loan: Yes"

        "sDh" ->
            "Success - Default: Yes - House Loan: No"

        "sdH" ->
            "Success - Default: No - House Loan: Yes"

        "sdh" ->
            "Success - Default: No - House Loan: No"

        "sDHL" ->
            "Success - Default: Yes - House Loan: Yes - Other Loan: Yes"

        "sDHl" ->
            "Success - Default: Yes - House Loan: Yes - Other Loan: No"

        "sDhL" ->
            "Success - Default: Yes - House Loan: No - Other Loan: Yes"

        "sDhl" ->
            "Success - Default: Yes - House Loan: No - Other Loan: No"

        "sdHL" ->
            "Success - Default: No - House Loan: Yes - Other Loan: Yes"

        "sdHl" ->
            "Success - Default: No - House Loan: Yes - Other Loan: No"

        "sdhL" ->
            "Success - Default: No - House Loan: No - Other Loan: Yes"

        "sdhl" ->
            "Success - Default: No - House Loan: No - Other Loan: No"

        "f" ->
            "Faillure"

        "fD" ->
            "Faillure - Default: Yes"

        "fd" ->
            "Faillure - Default: No"

        "fDH" ->
            "Faillure - Default: Yes - House Loan: Yes"

        "fDh" ->
            "Faillure - Default: Yes - House Loan: No"

        "fdH" ->
            "Faillure - Default: No - House Loan: Yes"

        "fdh" ->
            "Faillure - Default: No - House Loan: No"

        "fDHL" ->
            "Faillure - Default: Yes - House Loan: Yes - Other Loan: Yes"

        "fDHl" ->
            "Faillure - Default: Yes - House Loan: Yes - Other Loan: No"

        "fDhL" ->
            "Faillure - Default: Yes - House Loan: No - Other Loan: Yes"

        "fDhl" ->
            "Faillure - Default: Yes - House Loan: No - Other Loan: No"

        "fdHL" ->
            "Faillure - Default: No - House Loan: Yes - Other Loan: Yes"

        "fdHl" ->
            "Faillure - Default: No - House Loan: Yes - Other Loan: No"

        "fdhL" ->
            "Faillure - Default: No - House Loan: No - Other Loan: Yes"

        "fdhl" ->
            "Faillure - Default: No - House Loan: No - Other Loan: No"

        _ ->
            "Error"



-- Demographic Personal View


demoW : Float
demoW =
    900


demoH : Float
demoH =
    450


demoPadding : Float
demoPadding =
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
            Scale.linear ( 0, demoW - 2 * demoPadding ) (wideExtent xValues 5)

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
    , Html.p [] [ Html.text "Marital: (0 - Single)  (1 - Divorced)  (2 - Married)" ]
    , Html.p [] [ Html.text "Education: (0 - Unknown)  (1 - Primary)  (2 - Secondary)  (3 - Tertiary)" ]
    , svg [ viewBox 0 0 demoW demoH, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        ([ style [] [ TypedSvg.Core.text """
            .frame rect { stroke: rgba(0, 0, 0,1); fill: rgba(0, 0, 0,1.0);}
              """ ]
         , g
            [ class [ "frame" ]
            ]
            [ rect [ x 0, y 0, width demoW, height demoH ] []
            ]
         , g
            [ class [ "diagramlabel" ]
            , transform [ Translate (2 * demoPadding) (demoH - (demoPadding / 4)) ]
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
    , svg [ viewBox 0 0 demoW demoH, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        ([ style [] [ TypedSvg.Core.text """
            .frame rect { stroke: rgba(0, 0, 0,1); fill: rgba(0, 0, 0,1.0);}
              """ ]
         , g
            [ class [ "frame" ]
            ]
            [ rect [ x 0, y 0, width demoW, height demoH ] []
            ]
         , g
            [ class [ "diagramlabel" ]
            , transform [ Translate (2 * demoPadding) (demoH - (demoPadding / 4)) ]
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
            Scale.linear ( demoH - 2 * demoPadding, 0 ) (wideExtent firstFloatList 11)

        secondScale =
            Scale.linear ( demoH - 2 * demoPadding, 0 ) (wideExtent secondFloatList 4)

        thirdScale =
            Scale.linear ( demoH - 2 * demoPadding, 0 ) (wideExtent thirdFloatList 9)

        fourthScale =
            Scale.linear ( demoH - 2 * demoPadding, 0 ) (wideExtent fourthFloatList 10)

        fifthScale =
            Scale.linear ( demoH - 2 * demoPadding, 0 ) (wideExtent fifthFloatList 3)

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
                , transform [ Translate (Scale.convert xScale (toFloat axisNumber) + demoPadding) demoPadding ]
                , stroke (Paint Color.white)
                , strokeWidth (Px 0.5)
                , fill (Paint Color.white)
                , color Color.white
                ]
                [ Axis.left [ Axis.tickCount 10 ] (Maybe.withDefault xScale (List.head (List.drop (axisNumber - 1) clientList.dimScales))), text_ [ x 0, y -10, fontFamily [ "sans-serif" ], fontSize (TypedSvg.Types.px 20), textAnchor AnchorMiddle ] [ text (Maybe.withDefault "" (List.head (List.drop (axisNumber - 1) clientList.dimDescription))) ] ]

        DemoJob ->
            g
                [ class [ "axis" ]
                , transform [ Translate (Scale.convert xScale (toFloat axisNumber) + demoPadding) demoPadding ]
                , stroke (Paint Color.white)
                , strokeWidth (Px 0.5)
                , fill (Paint Color.white)
                , color Color.white
                ]
                [ Axis.left [ Axis.tickCount 11 ] (Maybe.withDefault xScale (List.head (List.drop (axisNumber - 1) clientList.dimScales))), text_ [ x 0, y -10, fontFamily [ "sans-serif" ], fontSize (TypedSvg.Types.px 20), textAnchor AnchorMiddle ] [ text (Maybe.withDefault "" (List.head (List.drop (axisNumber - 1) clientList.dimDescription))) ] ]

        DemoMarital ->
            g
                [ class [ "axis" ]
                , transform [ Translate (Scale.convert xScale (toFloat axisNumber) + demoPadding) demoPadding ]
                , stroke (Paint Color.white)
                , strokeWidth (Px 0.5)
                , fill (Paint Color.white)
                , color Color.white
                ]
                [ Axis.left [ Axis.tickCount 3 ] (Maybe.withDefault xScale (List.head (List.drop (axisNumber - 1) clientList.dimScales))), text_ [ x 0, y -10, fontFamily [ "sans-serif" ], fontSize (TypedSvg.Types.px 20), textAnchor AnchorMiddle ] [ text (Maybe.withDefault "" (List.head (List.drop (axisNumber - 1) clientList.dimDescription))) ] ]

        DemoEducation ->
            g
                [ class [ "axis" ]
                , transform [ Translate (Scale.convert xScale (toFloat axisNumber) + demoPadding) demoPadding ]
                , stroke (Paint Color.white)
                , strokeWidth (Px 0.5)
                , fill (Paint Color.white)
                , color Color.white
                ]
                [ Axis.left [ Axis.tickCount 4 ] (Maybe.withDefault xScale (List.head (List.drop (axisNumber - 1) clientList.dimScales))), text_ [ x 0, y -10, fontFamily [ "sans-serif" ], fontSize (TypedSvg.Types.px 20), textAnchor AnchorMiddle ] [ text (Maybe.withDefault "" (List.head (List.drop (axisNumber - 1) clientList.dimDescription))) ] ]

        DemoBalance ->
            g
                [ class [ "axis" ]
                , transform [ Translate (Scale.convert xScale (toFloat axisNumber) + demoPadding) demoPadding ]
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
            g [ transform [ Translate demoPadding demoPadding ], class [ "series" ] ]
                [ Path.element line [ stroke (Paint (Color.rgba 0 0 1 0.8)), strokeWidth (Px 0.1), fill PaintNone ]
                ]

        False ->
            g [ transform [ Translate demoPadding demoPadding ], class [ "series2" ] ]
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


outcomeToBool : Outcome -> Bool
outcomeToBool outcome =
    case outcome of
        Accepted ->
            True

        Declined ->
            False

        _ ->
            False
