port module Main exposing (..)
import String
import Html exposing (..)
import Html.App as App
import Html.Attributes as Attr
import Html.Events exposing (..)
import Http
import Maybe exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgAttr exposing (..)
import Json.Decode as Json exposing (..)
import Json.Encode
import Task
import Date exposing (Date, Day(..), day, dayOfWeek, month, year)
import DatePicker exposing (defaultSettings)
--import Date.Extra.Format as Format exposing (format, isoDateFormat,formatUtc, isoStringNoOffset)
import Date.Extra.Core exposing (monthToInt)
import Dict exposing (..)

main : Program Flags
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias PathRecord = {id : String, path : String}

type alias DataRecord = { id: String, value : Float }

type alias Model =
    {file : String
    ,records : Maybe (List PathRecord)
    ,data : Maybe (Dict String String)
    ,dataUrl : String
    ,fetchingColors : Bool
    ,showingDate : Maybe String
    ,datePicker : DatePicker.DatePicker
    ,date : Maybe Date
    ,hour : Int
    ,plotvars : List String
    ,colorData : Dict String (Dict String (Dict String Float))
    }


type alias Flags =
    {mapfile : String
    ,dataUrl : String
    ,year : Int
    ,month : Int
    ,day : Int}


init: Flags -> (Model, Cmd Msg)
init fl =
    let
        initdate =  (mkDate fl.year fl.month fl.day)
        ( datePicker, datePickerFx ) =
            DatePicker.init
                { defaultSettings
                    | inputClassList = [ ( "form-control", True ) ]
                    , inputName = Just "date"
                    , pickedDate = Just initdate
                }
    in
        { file = fl.mapfile
         , dataUrl = fl.dataUrl
         , records = Nothing
         , data = Nothing
        , plotvars = ["sum_vmt","n_mt"]
         , datePicker = datePicker
         , date = Just initdate
         , hour = 8
                  , colorData = Dict.empty
         , fetchingColors = False
         , showingDate = Nothing}
         ! [ Cmd.batch([getIt2 fl.mapfile
                       , Cmd.map ToDatePicker datePickerFx
                       ])]


-- UPDATE

port d3Update : (List String) -> Cmd msg
port getTopoJson : Json.Value -> Cmd msg
port getColorJson2 : List (String, Float) -> Cmd msg

-- port for listening for translated features from JavaScript
port features : (List PathRecord -> msg) -> Sub msg
port colors   : (Json.Value -> msg) -> Sub msg

type Msg
  = MorePlease
  | FetchSucceed2 Json.Value
  | FetchDataSucceed (Dict String (Dict String (Dict String Float)))
  | IdPath (List PathRecord)
  | ColorMap Json.Value
  | FetchFail Http.Error
  | ToDatePicker DatePicker.Msg
  | Hour String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ToDatePicker rec ->
            let
                ( datePicker, datePickerFx, mDate ) =
                    DatePicker.update rec model.datePicker

                date =
                    case mDate of
                        Nothing ->
                            model.date

                        dd ->
                            dd
            in
             { model
                  | date = date
                  , datePicker = datePicker
              }
             ! [ Cmd.map ToDatePicker datePickerFx]

    Hour rec ->
        ({model | hour = Result.withDefault model.hour (String.toInt rec)}
        ,Cmd.none)

    MorePlease ->
        ({model | fetchingColors = True} , getData model)


    FetchSucceed2 rec ->
        (model, getTopoJson rec)

    FetchDataSucceed rec ->
        case model.date of
            Just date ->
                let
                    y = (toString (Date.year date))
                    m = (pad (monthToInt (Date.month date)))
                    d  = (pad (Date.day date))
                    h = (pad model.hour)
                    newDateFetched =  y++"-"++m++"-"++d++" "++h++":00"
                in
                    -- let the UI know the data is back
                    ({model |
                      fetchingColors = False
                     ,showingDate = Just newDateFetched
                     ,hour = model.hour + 1
                     ,colorData = rec}
                    -- and now go get the right colors for the retrieved data
                    , getColorJson rec model.plotvars)
            Nothing ->
                (model , Cmd.none)

    IdPath newFeatures ->
        ({model | records = Just newFeatures}, Cmd.none)

    ColorMap newData ->
        ({model | data = (Result.toMaybe(Json.decodeValue colorDictionary newData))}, Cmd.none)

    FetchFail _ ->
        (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ features IdPath
        , colors ColorMap
        ]


-- VIEW

svgpaths : List PathRecord -> List (Svg msg)
svgpaths paths =
    List.map svgpath paths

--pathRender : PathRecord -> Html
svgpath : PathRecord -> Svg msg
svgpath entry =
    Svg.path [SvgAttr.class "grid", Attr.id entry.id, SvgAttr.d entry.path][]

svgpaths2 : List PathRecord ->  Maybe (Dict String String) -> List (Svg msg)
svgpaths2 paths colordata =
    List.map (svgpath2 colordata) paths

--pathRender : PathRecord -> Html
svgpath2 :  Maybe (Dict String String) -> PathRecord -> Svg msg
svgpath2 colordata entry =
    let

        colorString =
            case colordata of
                Nothing ->
                    "inherit"
                Just colordata ->
                    Maybe.withDefault "inherit" (get entry.id colordata)
        gridstyle = Attr.style
                       [("fill", colorString),("stroke","white")]

    in
        case colorString of
            "inherit" ->
                Svg.path [SvgAttr.class "grid"
                         , Attr.id entry.id
                         , SvgAttr.d entry.path][]
            _ ->
                Svg.path [SvgAttr.class "grid colored"
                         , Attr.id entry.id
                         , gridstyle
                         , SvgAttr.d entry.path][]



mapcontrol : Model -> Html Msg
mapcontrol model =
            let
                currday = case model.date of
                              Just date ->
                                  let
                                      y = (toString (Date.year date))
                                      m= (pad (monthToInt (Date.month date)))
                                      d  = (pad (Date.day date))
                                      h = (pad model.hour)
                                  in
                                      y++"-"++m++"-"++d++" "++h++":00"
                              Nothing ->
                                  "No date selected"
            in
                div [Attr.class "mapcontrol col"]
                    [h2 []
                         [Html.text "Pick date and hour to display on map"]
                    ,h2 [] [ Html.text <| currday ]
                    ,label[][Html.text "Date: "
                                  ,DatePicker.view model.datePicker
                                  |> App.map ToDatePicker
                                     ]
                    ,label [] [Html.text "Hour: "
                                    , input [ Attr.type' "number"
                                            , Attr.value (pad model.hour)
                                            , Attr.min "0"
                                            , Attr.max "23"
                                            , Attr.step "1"
                                            , onInput Hour][]
                              ]
                    ,button [ Attr.disabled model.fetchingColors, onClick MorePlease ] [ Html.text ("get date")]
                    ]



view : Model -> Html Msg
view model =

        case model.records of
            Nothing ->
                (div [Attr.class "container"]
                     [div [Attr.class "row"][
                           div [Attr.class "mapapp col"][
                                Svg.svg [  width "500", height "536"][
                                     Svg.g [ overflow "hidden", width "500", height "500"][]
                                    ]],
                           (mapcontrol model)
                               ]])
            Just records ->
                ( div [Attr.class "container"]
                      [div [Attr.class "row"][
                            div [Attr.class "mapapp col"][
                                 Svg.svg [  width "500", height "536"][
                                      Svg.g [ width "500", height "500"] (svgpaths2 records model.data)
                                     ,Svg.rect[ x "0"
                                              , y "500"
                                              , width "500"
                                              , height "36"][]

                                     ,Svg.text'
                                          [x "250"
                                          , y "524"
                                          , fontSize "24"
                                          , alignmentBaseline "middle"
                                          , textAnchor "middle"
                                          , class "maplabel"]
                                          [Svg.text (Maybe.withDefault "No date selected" model.showingDate)]
                                     ]],
                                (mapcontrol model)
                           ]]
                )




-- HTTP

getData : Model -> Cmd Msg
getData model =
    case model.date of
        Nothing -> Cmd.none
        Just date ->
            let
                y = (toString (Date.year date))
                m= (pad (monthToInt (Date.month date)))
                d  = (pad (Date.day date))
                h = (pad model.hour)
                filePath = y++"_"++m++"_"++d++"_"++h++"00.json"
                url = model.dataUrl ++ "/" ++ filePath
            in
                Task.perform FetchFail FetchDataSucceed (Http.get gridDataDictionary url)


getter : (Dict String Float) -> String -> Float -> Float
getter  myd a start = start + (Maybe.withDefault 0.0 (Dict.get a myd))

sumValues : List String -> Dict String Float -> Float -> Float
sumValues mykeys mydict start = List.foldl (getter mydict) start mykeys


gridReduce : List String -> String ->  (Dict String (Dict String Float) ) -> Float
gridReduce mykeys gridid griddata =  List.foldl (sumValues mykeys) 0.0 (Dict.values griddata)

sumDataValues : Dict String (Dict String (Dict String Float)) -> List String -> Dict String Float
sumDataValues griddata plotvars =
    Dict.map (gridReduce plotvars) griddata


getColorJson : Dict String (Dict String (Dict String Float)) -> List String -> Cmd Msg
getColorJson griddata plotvars  =
   getColorJson2 ( Dict.toList (sumDataValues griddata plotvars))



getIt2 : String -> Cmd Msg
getIt2 f =
  let
    url = f
  in
    Task.perform FetchFail FetchSucceed2 (Http.get decodeResult2 url)


decodeResult2 : Json.Decoder Json.Value
decodeResult2 = Json.value


gridDataDictionary : Json.Decoder (Dict String (Dict String (Dict String Float)))
gridDataDictionary = dict (dict ( dict float))


colorDictionary : Json.Decoder (Dict String String)
colorDictionary = dict Json.string




-- this mkDate/unsafeDate  hack idiom copied from one of the date libraries I looked at

mkDate : Int -> Int -> Int -> Date
mkDate year month day =
    toString year
        ++ "/"
        ++ pad month
        ++ "/"
        ++ pad day
        |> unsafeDate


unsafeDate : String -> Date
unsafeDate date =
    case Date.fromString date of
        Err e ->
            Debug.crash ("unsafeDate: failed to parse date:" ++ e)

        Ok date -> date




pad :  Int -> String
pad int =
    if int < 10 then
        "0" ++ toString int
    else
        toString int
