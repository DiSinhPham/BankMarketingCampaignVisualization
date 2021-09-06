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


init : () -> ( ( Model, List LoadedCVS ), Cmd Msg )
init _ =
    ( ( Loading, [] )
    , Cmd.batch (List.map sendHttpRequest urls)
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


sendHttpRequest : String -> Cmd Msg
sendHttpRequest givenUrl =
    Http.get
        { url = givenUrl
        , expect = expectStringDetailed GotText
        }



-- UPDATE


type Msg
    = GotText (Result ErrorDetailed ( Http.Metadata, String ))


update : Msg -> ( Model, List LoadedCVS ) -> ( ( Model, List LoadedCVS ), Cmd Msg )
update msg ( model, loadedFiles ) =
    let
        newModel =
            case msg of
                GotText result ->
                    case result of
                        Ok detailedResponse ->
                            Success detailedResponse

                        Err detailedError ->
                            Failure detailedError

        updatedFileList =
            case newModel of
                Failure error ->
                    loadedFiles

                Loading ->
                    loadedFiles

                Success ( metadata, body ) ->
                    loadedFiles ++ [ LoadedCVS metadata.url body ]
    in
    ( ( newModel, updatedFileList ), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : ( Model, List LoadedCVS ) -> Sub Msg
subscriptions ( model, loadedFiles ) =
    Sub.none



-- VIEW


view : ( Model, List LoadedCVS ) -> Html Msg
view ( model, loadedFiles ) =
    let
        cvsStringToHtml : LoadedCVS -> List (Html Msg)
        cvsStringToHtml cvs =
             Html.p [] [ text cvs.url ] :: [pre [] [ text (String.left 1000 cvs.contents) ]]
        
        htmlList = List.concat (List.map cvsStringToHtml loadedFiles)
    in
    case model of
        Failure error ->
            pre [] [ text "Uh oh, there was an error! We can access the metadata and body if the error was \"BadStatus\"" ]

        Loading ->
            text "Loading..."

        Success ( metadata, body ) ->
            div []
                (htmlList)
                


urls : List String
urls =
    [
    "https://raw.githubusercontent.com/DiSinhPham/BankMarketingCampaignVisualization/main/data/bank-full.csv"
    ]
