port module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes as Attr
import Html.Events exposing (..)
import Http
import Maybe exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgAttr exposing (..)
import Json.Decode as Json exposing (..)
import Task
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


type alias Model =
    {file : String
    ,records : Maybe (List PathRecord)
    ,data : Maybe (Dict String String)
    ,year : Int
    ,month : Int
    ,day : Int
    ,hour : Int
    ,dataUrl : String
    ,fetchingColors : Bool
    ,showingDate : Maybe String
    }

-- type alias Properties =
--     { gid : Int
--     ,id : String}

-- type alias Coordinate =
--     (Float, Float)

-- type alias PolyLine =
--      List Coordinate

-- type alias PolyCoordinates =
--      List ( List  PolyLine  )


-- type alias Geometry =
--     { type' : String
--       ,coordinates : PolyCoordinates }

-- type alias Feature =
--     {type' : String
--     ,properties : Properties
--     ,geometry : Geometry }

-- {"type":"Feature",
--    "properties":{
--            "gid":25083,
--            "id":"120_278"},
--    "geometry":{
--            "type":"Polygon",
--            "coordinates":[[[-123.08153279727972,42.0060393009301],[-123.10209111311131,42.0060393009301],[-123.10209111311131,42.007934650465046],[-123.08153279727972,42.0060393009301]]]}
--   }

type alias Flags =
    {mapfile : String
    ,dataUrl : String}


init: Flags -> (Model, Cmd Msg)
init fl =
  ({ file = fl.mapfile
   , dataUrl = fl.dataUrl
   , records = Nothing
   , data = Nothing
   , year = 2012
   , month = 1
   , day = 11
   , hour = 8
   , fetchingColors = False
   , showingDate = Nothing} , getIt2 fl.mapfile)


-- UPDATE

port d3Update : (List String) -> Cmd msg
port getTopoJson : Json.Value -> Cmd msg
port getColorJson : Json.Value -> Cmd msg

-- port for listening for translated features from JavaScript
port features : (List PathRecord -> msg) -> Sub msg
port colors   : (Json.Value -> msg) -> Sub msg

type Msg
  = MorePlease
  | FetchSucceed2 Json.Value
  | FetchDataSucceed Json.Value
  | IdPath (List PathRecord)
  | ColorMap Json.Value
  | FetchFail Http.Error




update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
        let
            year = if(model.year < 10) then  "0"++toString(model.year) else toString(model.year)
            month= if(model.month < 10)then "0"++toString(model.month) else toString(model.month)
            day  = if(model.day < 10)  then "0"++toString(model.day)  else  toString(model.day)
            hour = if(model.hour < 10) then "0"++toString(model.hour) else  toString(model.hour)
            fetchingDate = year++"-"++month++"-"++day++" "++hour++":00"
        in

            ({model | fetchingColors = True , showingDate = Just fetchingDate}
            , getData model)

    FetchSucceed2 rec ->
        (model, getTopoJson rec)

    FetchDataSucceed rec ->
        ({model | fetchingColors = False, hour = model.hour + 1}
        , getColorJson rec)

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
        year = if(model.year < 10) then  "0"++toString(model.year) else toString(model.year)
        month= if(model.month < 10)then "0"++toString(model.month) else toString(model.month)
        day  = if(model.day < 10)  then "0"++toString(model.day)  else  toString(model.day)
        hour = if(model.hour < 10) then "0"++toString(model.hour) else  toString(model.hour)
        currday = year++"-"++month++"-"++day
        filePath = year++"_"++month++"_"++day++"_"++hour++"00.json"
    in
        div [Attr.class "mapcontrol col"]
            [h2 []
                 [Html.text "Map of estimated hourly VMT"]
            , label[][Html.text "Date: "
                          , input [ Attr.type' "date"
                                  , Attr.value currday ][]
                     ]
            , label[][Html.text "Hour: "
                          , input [ Attr.type' "number"
                                  , Attr.value hour
                                  , Attr.min "0"
                                  , Attr.max "23"
                                  , Attr.step "1" ][]
                     ]
            , button [ onClick MorePlease ] [ Html.text ("get "++filePath) ]]


view : Model -> Html Msg
view model =

        case model.records of
            Nothing ->
                (div [Attr.class "container"]
                     [div [Attr.class "row"][
                           div [Attr.class "mapapp col"][
                                Svg.svg [  width "500", height "500"][
                                     Svg.g [][]
                                    ]],
                           (mapcontrol model)
                               ]])
            Just records ->
                ( div [Attr.class "container"]
                      [div [Attr.class "row"][
                            div [Attr.class "mapapp col"][
                                 Svg.svg [  width "500", height "500"][
                                      Svg.g [] (svgpaths2 records model.data)
                                     ,Svg.text'
                                          [x "250"
                                          , y "500"
                                          , fontSize "24"
                                          , fill "#233"
                                          , textAnchor "middle" ]
                                          [Svg.text (Maybe.withDefault "" model.showingDate)]
                                     ]],
                                (mapcontrol model)
                           ]]
                )




-- HTTP

getData : Model -> Cmd Msg
getData model =
    let
        year = if(model.year < 10) then  "0"++toString(model.year) else toString(model.year)
        month= if(model.month < 10)then "0"++toString(model.month) else toString(model.month)
        day  = if(model.day < 10)  then "0"++toString(model.day)  else  toString(model.day)
        hour = if(model.hour < 10) then "0"++toString(model.hour) else  toString(model.hour)
        filePath = year++"_"++month++"_"++day++"_"++hour++"00.json"
        url = model.dataUrl ++ "/" ++ filePath
    in
        Task.perform FetchFail FetchDataSucceed (Http.get decodeResult2 url)

getIt2 : String -> Cmd Msg
getIt2 f =
  let
    url = f
  in
    Task.perform FetchFail FetchSucceed2 (Http.get decodeResult2 url)


decodeResult2 : Json.Decoder Json.Value
decodeResult2 = Json.value

--decodeResult : List decodeIdPath

-- decodeIdPath : Json.Decoder PathRecord
-- decodeIdPath = Json.object2 PathRecord
--                (  ("id"   := Json.string)
--                       ("path" := Json.string))


-- decodeDataResult : Json.Decoder Json.Value
-- decodeDataResult = Json.value



--                    type DataShape
--   = HpmsEntry {sum_lane_miles : Float
--               ,sum_single_unit_mt : Float
--               ,sum_vmt : Float
--               ,sum_combination_mt : Float}
--   | DetectorEntry {n_mt : Float
--                   ,hh_mt : Float
--                   ,nhh_mt : Float
--                   ,lane_miles : Float
--                   ,miles : Float}

-- dataInfo : String -> Decoder DataShape
-- dataInfo tag =
--   case tag of
--     "detector_based" ->
--         object5 DetectorEntry
--             ("n_mt" := Float)
--             ("hh_mt" := Float)
--             ("nhh_mt" := Float)
--             ("lane_miles" := Float)
--             ("miles" := Float)
--     _ ->
--         object4 HpmsEntry
--           ("sum_lane_miles" := Float)
--           ("sum_single_unit_mt" := Float)
--           ("sum_vmt" := Float)
--           ("sum_combination_mt" := Float)


-- -- now combine these (but how?)

gridDictionary : Json.Decoder (Dict String (Dict String (Dict String Float)))
gridDictionary = dict (dict ( dict float))


colorDictionary : Json.Decoder (Dict String String)
colorDictionary = dict Json.string
