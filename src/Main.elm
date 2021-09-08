module Main exposing (..)

import Browser
import Csv
import Csv.Decode
import Html exposing (Html, button, div, pre, text)
import Http



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


init : () -> ( ( Model, LoadedCSV ), Cmd Msg )
init _ =
    ( ( Loading, LoadedCSV "default" "default" )
    , Http.get
        { url = "https://raw.githubusercontent.com/DiSinhPham/BankMarketingCampaignVisualization/main/data/bank-full.csv"
        , expect = expectStringDetailed GotText
        }
    )



-- UPDATE


update : Msg -> ( Model, LoadedCSV ) -> ( ( Model, LoadedCSV ), Cmd Msg )
update msg ( model, loadedFile ) =
    let
        newModel =
            case msg of
                GotText result ->
                    case result of
                        Ok detailedResponse ->
                            Success detailedResponse

                        Err detailedError ->
                            Failure detailedError

        updatedFile =
            case newModel of
                Failure error ->
                    loadedFile

                Loading ->
                    loadedFile

                Success ( metadata, body ) ->
                    LoadedCSV metadata.url body
    in
    ( ( newModel, updatedFile ), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : ( Model, LoadedCSV ) -> Sub Msg
subscriptions ( model, loadedFile ) =
    Sub.none



-- VIEW


view : ( Model, LoadedCSV ) -> Html Msg
view ( model, loadedFile ) =
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
                 ]
                    ++ htmlList
                )



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
