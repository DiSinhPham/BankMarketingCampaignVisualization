module Main exposing (..)

import Browser
import Html exposing (Html, div, pre, text)
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


type alias LoadedCVS =
    { url : String
    , contents : String
    }


init : () -> ( ( Model, LoadedCVS ), Cmd Msg )
init _ =
    ( ( Loading, LoadedCVS "default" "default" )
    , Http.get
        { url = "https://raw.githubusercontent.com/DiSinhPham/BankMarketingCampaignVisualization/main/data/bank-full.csv"
        , expect = expectStringDetailed GotText
        }
    )


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

-- UPDATE


type Msg
    = GotText (Result ErrorDetailed ( Http.Metadata, String ))


update : Msg -> ( Model, LoadedCVS ) -> ( ( Model, LoadedCVS ), Cmd Msg )
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
                    LoadedCVS metadata.url body
    in
    ( ( newModel, updatedFile ), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : ( Model, LoadedCVS ) -> Sub Msg
subscriptions ( model, loadedFile ) =
    Sub.none



-- VIEW


view : ( Model, LoadedCVS ) -> Html Msg
view ( model, loadedFile ) =
    let
        cvsStringToHtml : LoadedCVS -> List (Html Msg)
        cvsStringToHtml cvs =
             Html.p [] [ text cvs.url ] :: [pre [] [ text (cvs.contents) ]]
        
        htmlList = cvsStringToHtml loadedFile
    in
    case model of
        Failure error ->
            pre [] [ text "Uh oh, there was an error! We can access the metadata and body if the error was \"BadStatus\"" ]

        Loading ->
            text "Loading..."

        Success ( metadata, body ) ->
            div []
                (htmlList)
                
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
    { person : Person
    , contactType : Contact
    , day : Int
    , month : Int
    , duration : Int
    , contactCount : Int
    , outcome : Bool
    , pdays : Int
    , pcontactCount : Int
    , poutcome : Bool
    }
    
type Job 
    = Admin 
    | UnknownJob
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

type Marital
    = Married
    | Divorced
    | Single

type Education 
    = UnknownEd
    | Primary
    | Secondary
    | Tertiary

type Contact
    = UnknownContact
    | Telephone
    | Cellular