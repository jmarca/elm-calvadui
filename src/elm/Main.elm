port module Main exposing (..)
import String
import Html exposing (..)
import Html.App as App
import Html.Attributes as Attr
import Html.Events exposing (..)
import Cmd.Extra
import Http
import Maybe exposing (..)
import Svg exposing (..)
import Svg.Attributes as SvgAttr exposing (..)
import Json.Decode as Json exposing (..)
-- import Json.Encode
import Task
import Date exposing (Date, Day(..), day, dayOfWeek, month, year)
import DatePicker exposing (defaultSettings)
--import Date.Extra.Format as Format exposing (format, isoDateFormat,formatUtc, isoStringNoOffset)
import Date.Extra.Core exposing (monthToInt)
import Date.Extra.Period as Period exposing (Period (..))
import Dict exposing (..)
import Regex exposing (..)
import Array exposing (..)

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


type alias PlottingButton =
    { entry    : String -- sometimes relevant, what to extract from dict
    , value    : String -- what to say to the Update fn
    , class    : String -- help with styling
    , text     : String -- what to show the UI
    , on       : Bool   -- is button currently selected
    , disabled : Bool   -- is button currently selected
    , relevant : Bool   -- is this button relevant to current year
    , matchFn : String -> Bool -- does the incoming key match this
    }

-- -- PlotVars.  Problem with this is able to make mistakes, like
-- -- plotting at the same time trucks and VMT
-- type alias PlotVars =
--     { detectorbased : Bool
--     , hpmsBased : Bool
--     , hpmsHwys : Bool
--     , hpmsCounty : Bool
--     , hpmsCity : Bool
--     , hpmsVMT : Bool
--     , hpmsCombo : Bool
--     , hpmsSingle : Bool
--     , detectorVMT : Bool
--     , detectorHH : Bool
--     , detectorNHH : Bool
--     }


type alias Model =
    {file : String
    ,records : Maybe (List PathRecord)
    ,data : Maybe (Dict String String)
    ,dataUrl : String
    ,fetchingColors : Bool
    ,showingDate : Maybe String
    ,datePicker : DatePicker.DatePicker
    ,baddate : Maybe String
    ,date : Date
    ,hour : Int
    ,hpmsBased : PlottingButton
    ,detectorBased : PlottingButton
    ,hpmsPlotVars : Array PlottingButton
    ,hpmsRoadTypes : Array PlottingButton
    ,detectorPlotVars : Array PlottingButton
    ,colorData : Dict String (Dict String (Dict String Float))
    ,scaleDomain : Int
    ,scaleExponent : Float
    }

type alias ColorMessages =
    {maxdomain : Int
    ,exponent : Float
    ,data : List (String, Float)}

type alias Flags =
    {mapfile : String
    ,dataUrl : String
    ,year : Int
    ,month : Int
    ,day : Int
    ,hour: Int}


init: Flags -> (Model, Cmd Msg)
init fl =
    let
        initdate =  (mkDate fl.year fl.month fl.day fl.hour)
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
        , detectorBased = { entry = ""
                          , value = "detectorbased"
                          , class = "detector btn"
                          , text = "detector based data"
                          , on = True
                          , disabled = False
                          , relevant = True
                          , matchFn = \k -> (k == "detector_based")
                          }
        , hpmsBased = { entry = ""
                      , value = "hpmsbased"
                      , class = "hpms btn"
                      , text = "HPMS based data"
                      , on = True, disabled = False
                      , relevant = True
                      , matchFn = \_ -> True
                      }

        , hpmsRoadTypes = Array.fromList [ { entry = ""
                            , value = "shs"
                            , class = "hpms btn"
                            , text = "State Highways"
                            , on = True, disabled = False
                            , relevant = True
                            , matchFn = \key ->
                                        List.any (\rx -> Regex.contains (regex rx) key) ["^SHS","^RAMP"]
                            }
                          , { entry = ""
                            , value = "city"
                            , class = "hpms btn"
                            , text = "City Streets"
                            , on = True, disabled = False
                            , relevant = True
                            , matchFn = \key ->
                                        let
                                            types = ["^SHS","^CO","^RAMP","^detector"]
                                            check = List.any (\rx -> Regex.contains (regex rx) key ) types
                                        in
                                            (not check)
                            }
                          , { entry = ""
                            , value = "co"
                            , class = "hpms btn"
                            , text = "County Streets"
                            , on = True, disabled = False
                            , relevant = True
                            , matchFn = Regex.contains (regex "^CO")
                            }
                          ]

        ,hpmsPlotVars = Array.fromList [ { entry = "sum_vmt"
                          , value = "hpms_vmt"
                          , class = "hpms btn"
                          , text = "HPMS VMT"
                          , on = True, disabled = False
                          , relevant = True
                          , matchFn = (==) "sum_vmt"
                          }
                        , { entry = "sum_combination_mt"
                          , value = "hpms_combovmt"
                          , class = "hpms btn"
                          , text = "Combination Truck VMT"
                          , on = False, disabled = True
                          , relevant = True
                          , matchFn = (==) "sum_combination_vmt"
                          }
                        , { entry = "sum_single_unit_mt"
                          , value = "hpms_singlevmt"
                          , class = "hpms btn"
                          , text = "Single Unit Truck VMT"
                          , on = False, disabled = True
                          , relevant = True
                          , matchFn = (==) "sum_single_unit_vmt"
                          }
                        ]

        ,detectorPlotVars = Array.fromList [ { entry = "n_mt"
                              , value = "n_mt"
                              , class = "detector btn"
                              , text = "detector-based VMT"
                              , on = True, disabled = False
                              , relevant = True
                              , matchFn = (==) "n_mt"
                              }
                            , { entry = "hh_mt"
                              , value = "hh_mt"
                              , class = "detector btn"
                              , text = "Heavy Heavy-Duty Truck VMT"
                              , on = False, disabled = True
                              , relevant = True
                              , matchFn = (==) "hh_mt"
                              }
                            , { entry = "nhh_mt"
                              , value = "nhh_mt"
                              , class = "detector btn"
                              , text = "Not Heavy Heavy-Duty Truck VMT"
                              , on = False, disabled = True
                              , relevant = True
                              , matchFn = (==) "nhh_mt"
                              }
                            ]

        -- { detectorbased = True  --  True
        -- , hpmsBased     = True  --  True
        -- , hpmsHwys      = True  --  True
        -- , hpmsCounty    = True  --  True
        -- , hpmsCity      = True  --  True
        -- , hpmsVMT       = True  --  True
        -- , hpmsCombo     = False --  False
        -- , hpmsSingle    = False --  False
        -- , detectorVMT   = True  --  True
        -- , detectorHH    = False --  False
        -- , detectorNHH   = False --  False
        -- }
        , datePicker = datePicker
        , baddate = Nothing
        , date = initdate
        , hour = (Date.hour initdate) -- should be same as fl.hour
        , colorData = Dict.empty
        , fetchingColors = False
        , showingDate = Nothing
        , scaleDomain = 190000
        , scaleExponent = 0.3}
         ! [ Cmd.batch([getIt2 fl.mapfile
                       , Cmd.map ToDatePicker datePickerFx
                       ])]





-- UPDATE

port d3Update : (List String) -> Cmd msg
port getTopoJson : Json.Value -> Cmd msg
port getColorJson2 : ColorMessages  -> Cmd msg

-- port for listening for translated features from JavaScript
port features : (List PathRecord -> msg) -> Sub msg
port colors   : (Json.Value -> msg) -> Sub msg

type BtnMsg
    = DetectorBased
    | HpmsBased
    | HpmsRoadTypes
    | HpmsPlotVars
    | DetectorPlotVars

type Msg
  = MorePlease
  | FetchSucceed2 Json.Value
  | FetchDataSucceed (Dict String (Dict String (Dict String Float)))
  | IdPath (List PathRecord)
  | ColorMap Json.Value
  | FetchFail Http.Error
  | FetchDataFail Http.Error
  | ToDatePicker DatePicker.Msg
  | NewHour String
  | HandleButton BtnMsg Int String
  -- | DetectorBased
  -- | HpmsBased
  -- | HpmsHwys
  -- | HpmsCounty
  -- | HpmsCity
  -- | HpmsVMT
  -- | HpmsCombo
  -- | HpmsSingle
  -- | DetectorVMT
  -- | DetectorHH
  -- | DetectorNHH
  | ScaleDomain String
  | ScaleExponent String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewHour rec ->
        let
            newhour = Result.withDefault model.hour (String.toInt rec)
            diffhour = newhour - model.hour
            newdate = (Period.add Period.Hour diffhour model.date)
            ( datePicker, datePickerFx ) =
                DatePicker.init
                    { defaultSettings
                        | inputClassList = [ ( "form-control", True ) ]
                        , inputName = Just "date"
                        , pickedDate = Just newdate
                    }

        in
        ({model | hour = Date.hour newdate
                 ,date = newdate
                 ,datePicker = datePicker
         }
        , Cmd.map ToDatePicker datePickerFx)

    ToDatePicker rec ->
            let
                ( datePicker, datePickerFx, mDate ) =
                    DatePicker.update rec model.datePicker

                date =
                    case mDate of
                        Nothing ->
                            model.date

                        Just dd -> dd
            in
             { model
                  | date = date
                  , datePicker = datePicker
              }
             ! [ Cmd.map ToDatePicker datePickerFx]


    MorePlease ->
        ({model | fetchingColors = True} , getData model)

    HandleButton msg index rec ->
        case msg of
            DetectorBased ->
                let pvars =  model.detectorBased
                    toggle =  {pvars | on = not pvars.on}
                in
                    ({model | detectorBased = toggle}
                    , getColorJson {model | detectorBased = toggle})
            HpmsBased ->
                let pvars = model.hpmsBased
                    toggle = {pvars | on = not pvars.on}

                in
                    ({model | hpmsBased = toggle}
                    , getColorJson {model | hpmsBased = toggle})
            HpmsRoadTypes ->
                let pvars = Array.get index model.hpmsRoadTypes
                in
                    case pvars of
                        Just pvars ->
                            let
                                toggle = {pvars | on = not pvars.on
                                         }
                                newArr = Array.set index toggle model.hpmsRoadTypes
                            in
                                ({model | hpmsRoadTypes = newArr}
                                , getColorJson {model | hpmsRoadTypes = newArr})
                        _ -> (model , Cmd.none)
            HpmsPlotVars ->
                let pvars = Array.get index model.hpmsPlotVars
                in
                    case pvars of
                        Just pvars ->
                            let
                                toggle = {pvars | on = not pvars.on}
                                newArr =
                                    case index of
                                        0 -> -- toggling VMT case
                                        let
                                            len = Array.length model.hpmsPlotVars
                                            otherArr = Array.map (setDisabled (not pvars.on)) (Array.slice 1 len model.hpmsPlotVars)
                                        in
                                            Array.append
                                                (Array.fromList [toggle])
                                                otherArr
                                        _ -> -- not the VMT case, so simpler
                                          Array.set index toggle model.hpmsPlotVars
                            in
                                ({model | hpmsPlotVars = newArr}
                                , getColorJson {model | hpmsPlotVars = newArr})
                        _ -> (model, Cmd.none)
            DetectorPlotVars ->
                let pvars = Array.get index model.detectorPlotVars
                in
                    case pvars of
                        Just pvars ->
                            let
                                toggle = {pvars | on = not pvars.on}
                                newArr =
                                    case index of
                                        0 -> -- toggling VMT case
                                        let
                                            len = Array.length model.detectorPlotVars
                                            otherArr = Array.map (setDisabled  (not pvars.on)) (Array.slice 1 len model.detectorPlotVars)
                                        in
                                            Array.append
                                                (Array.fromList [toggle])
                                                otherArr
                                        _ -> -- not the VMT case, so simpler
                                           Array.set index toggle model.detectorPlotVars

                            in
                                ({model | detectorPlotVars = newArr}
                                , getColorJson {model | detectorPlotVars = newArr})
                        _ -> (model, Cmd.none)

    -- HpmsHwys      ->
    --     let pvars = model.plotvars
    --         dvars = {pvars | hpmsHwys = (not pvars.hpmsHwys)}
    --     in
    --     ({model | plotvars = dvars} , getColorJson model.colorData dvars model.scaleDomain model.scaleExponent)
    -- HpmsCounty    ->
    --     let pvars = model.plotvars
    --         dvars = {pvars | hpmsCounty = (not pvars.hpmsCounty)}
    --     in
    --     ({model | plotvars = dvars} , getColorJson model.colorData dvars model.scaleDomain model.scaleExponent)
    -- HpmsCity      ->
    --     let pvars = model.plotvars
    --         dvars = {pvars | hpmsCity = (not pvars.hpmsCity)}
    --     in
    --     ({model | plotvars = dvars} , getColorJson model.colorData dvars model.scaleDomain model.scaleExponent)
    -- HpmsVMT       ->
    --     let pvars = model.plotvars
    --         hpmsvmtNewState = (not pvars.hpmsVMT)
    --         dvars = if hpmsvmtNewState
    --                 then {pvars | hpmsVMT=True, hpmsCombo=False, hpmsSingle=False}
    --                 else {pvars | hpmsVMT=False, hpmsCombo=True, hpmsSingle=True}
    --     in
    --     ({model | plotvars = dvars} , getColorJson model.colorData dvars model.scaleDomain model.scaleExponent)
    -- HpmsCombo     ->
    --     let pvars = model.plotvars
    --         dvars = {pvars | hpmsCombo = (not pvars.hpmsCombo)}
    --     in
    --     ({model | plotvars = dvars} , getColorJson model.colorData dvars model.scaleDomain model.scaleExponent)
    -- HpmsSingle    ->
    --     let pvars = model.plotvars
    --         dvars = {pvars | hpmsSingle = (not pvars.hpmsSingle)}
    --     in
    --     ({model | plotvars = dvars} , getColorJson model.colorData dvars model.scaleDomain model.scaleExponent)
    -- DetectorVMT   ->
    --     let pvars = model.plotvars
    --         dvmtNewState = (not pvars.detectorVMT)
    --         dvars = if dvmtNewState
    --                 then {pvars | detectorVMT=True, detectorHH=False, detectorNHH=False}
    --                 else {pvars | detectorVMT=False, detectorHH=True, detectorNHH=True}
    --     in
    --     ({model | plotvars = dvars} , getColorJson model.colorData dvars model.scaleDomain model.scaleExponent)
    -- DetectorHH    ->
    --     let pvars = model.plotvars
    --         dvars = {pvars | detectorHH = (not pvars.detectorHH)}
    --     in
    --     ({model | plotvars = dvars} , getColorJson model.colorData dvars model.scaleDomain model.scaleExponent)
    -- DetectorNHH   ->
    --     let pvars = model.plotvars
    --         dvars = {pvars | detectorNHH = (not pvars.detectorNHH)}
    --     in
    --     ({model | plotvars = dvars} , getColorJson model.colorData dvars model.scaleDomain model.scaleExponent)

    ScaleDomain rec ->
        let
            newmax = Result.withDefault model.scaleDomain (String.toInt rec)
        in
            ({model | scaleDomain = newmax}
            , getColorJson {model | scaleDomain = newmax})


    ScaleExponent rec ->
        let
            newExponent = Result.withDefault model.scaleExponent (String.toFloat rec)
        in
            ({model | scaleExponent = newExponent}
            , getColorJson {model | scaleExponent = newExponent})


    IdPath newFeatures ->
        ({model | records = Just newFeatures}, Cmd.none)

    ColorMap newData ->
        ({model | data = (Result.toMaybe(Json.decodeValue colorDictionary newData))}, Cmd.none)

    FetchSucceed2 rec ->
        (model, getTopoJson rec)

    FetchDataSucceed rec ->
        let
            y = (toString (Date.year model.date))
            m = (pad (monthToInt (Date.month model.date)))
            d  = (pad (Date.day model.date))
            h = (pad model.hour)
            newDateFetched =  y++"-"++m++"-"++d++" "++h++":00"
        in
            -- let the UI know the data is back
            ({model |
                  fetchingColors = False
             ,showingDate = Just newDateFetched
             ,baddate=Nothing
             ,colorData = rec}
            -- and now go get the right colors for the retrieved data
            ! [ getColorJson {model | fetchingColors = False
                             ,showingDate = Just newDateFetched
                             ,baddate=Nothing
                             ,colorData = rec}
              ,Cmd.Extra.message (NewHour (toString (model.hour + 1) ))])

    FetchDataFail e ->
        let
            y = (toString (Date.year model.date))
            m = (pad (monthToInt (Date.month model.date)))
            d  = (pad (Date.day model.date))
            h = (pad model.hour)
            newDateFailed =  y++"-"++m++"-"++d++" "++h++":00"
        in
            -- let the UI know the date has no data
            ({model| baddate=Just newDateFailed
             ,fetchingColors = False}, Cmd.none)

    FetchFail e ->
        let
            f = Debug.log "HTTP Error: " e
        in
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
                    Maybe.withDefault "inherit" (Dict.get entry.id colordata)
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

makebutton : Maybe PlottingButton -> BtnMsg -> (Int, PlottingButton) ->  Maybe (Html Msg)
makebutton ablebutton msg (idx, btn) =
     if btn.relevant
     then
        let
            myclass = if btn.on
                      then (btn.class ++ " active")
                      else btn.class
            mydisabled = case ablebutton of
                             Just b ->
                                 btn.disabled || (not b.on)
                             _ -> btn.disabled
            mybutton =
                Html.button
                  [Attr.disabled mydisabled
                  ,Attr.class myclass
                  ,Attr.value btn.value
                  ,onClick (HandleButton msg idx btn.value) ] -- fix later
                  [ Html.text btn.text]
        in
            Just mybutton
     else
         Nothing

pickbuttons : Model ->  Html Msg
pickbuttons model =
    let
        -- what follows is a classic James E. Marca ball of logic
        -- default button
        defbtn = button [Attr.class "Error"][Html.text "Error"]
        -- road types are always enabled, so diabled list is all false
        hpmsRoadTypes    = Array.toIndexedList model.hpmsRoadTypes
        hpmsPlotVars     = Array.toIndexedList model.hpmsPlotVars
        detectorPlotVars = Array.toIndexedList model.detectorPlotVars

        detectorButton =  makebutton Nothing DetectorBased (0, model.detectorBased)
        detectorDataButtons =  List.filterMap (makebutton (Just model.detectorBased) DetectorPlotVars) detectorPlotVars

        hpmsButton =  makebutton Nothing HpmsBased (0, model.hpmsBased)
        hpmsTypeButtons = List.filterMap (makebutton (Just model.hpmsBased) HpmsRoadTypes) hpmsRoadTypes
        hpmsDataButtons = List.filterMap (makebutton (Just model.hpmsBased) HpmsPlotVars) hpmsPlotVars

    in

        div [Attr.class "plotbuttons"]
            [div [Attr.class "btn-container"]
                 [div [Attr.class "toplevel hpms row"] [Maybe.withDefault defbtn hpmsButton]
                 ,div [Attr.class "hpmsbuttons row"] hpmsTypeButtons
                 ,div [Attr.class "hpmsdata row"] hpmsDataButtons
                 ]
            ,div [Attr.class "btn-container"]
                 [div [Attr.class "toplevel detector row"] [Maybe.withDefault  defbtn  detectorButton]
                 ,div [Attr.class "detectordata row"] detectorDataButtons
                 ]]

-- whatToPlot : Model -> Html Msg
-- whatToPlot model =
--         let
--             detectorActive = True -- placeholder for other things
--         in
--             div [Attr.class "plotbuttons"]
--                 [div [Attr.class "btn-container"]
--                      [div [Attr.class "toplevel hpms row"]
--                           [button
--                                [ Attr.class (if model.plotvars.hpmsBased
--                                then "btn hpmsbased active"
--                                else "btn hpmsbased off")
--                        , onClick HpmsBased]
--                        [ Html.text ("hpms based data")]
--                   ]
--              ,div [Attr.class "hpmsbuttons row"]
--                  (hpmsbuttons model)
--              ,div [Attr.class "hpmsdata row"]
--                  [ button
--                        [Attr.disabled (not model.plotvars.hpmsBased)
--                        ,Attr.class (if model.plotvars.hpmsBased && model.plotvars.hpmsVMT
--                                     then "btn hpmsVMT active"
--                                     else "btn hpmsVMT off")
--                        , onClick HpmsVMT]
--                        [ Html.text ("HPMS-based VMT")]
--                  , button
--                        [Attr.disabled  (model.plotvars.hpmsVMT || (not model.plotvars.hpmsBased))
--                        ,Attr.class (if model.plotvars.hpmsBased && model.plotvars.hpmsCombo
--                                     then "btn hpmscombo active"
--                                     else "btn hpmscombo off")
--                        , onClick HpmsCombo]
--                        [ Html.text ("Combination Truck VMT")]
--                  , button
--                        [Attr.disabled  (model.plotvars.hpmsVMT || (not model.plotvars.hpmsBased))
--                        ,Attr.class (if model.plotvars.hpmsBased && model.plotvars.hpmsSingle
--                                     then "btn hpmssingle active"
--                                     else "btn hpmssingle off")
--                        , onClick HpmsSingle]
--                        [ Html.text ("Single Unit Truck VMT")]
--                  ]
--                  ]
--         ,div [Attr.class "btn-container"]
--             [div [Attr.class "toplevel detector row"]
--                  [button
--                       [ Attr.class (if model.plotvars.detectorbased
--                                     then "btn detectorbased active"
--                                     else "btn detectorbased off")
--                       , onClick DetectorBased]
--                       [ Html.text ("detector based data")]
--                  ]
--             ,div [Attr.class "detectordata row"]
--                 [ button
--                       [Attr.disabled (not model.plotvars.detectorbased)
--                       ,Attr.class (if model.plotvars.detectorbased && model.plotvars.detectorVMT
--                                    then "btn detectorVMT active"
--                                    else "btn detectorVMT off")
--                       , onClick DetectorVMT]
--                       [ Html.text ("detector-based VMT")]
--                 , button
--                       [Attr.disabled (model.plotvars.detectorVMT || (not  model.plotvars.detectorbased))
--                       ,Attr.class (if model.plotvars.detectorHH
--                                    then "btn detectorhh active"
--                                    else "btn detectorhh off")
--                       , onClick DetectorHH]
--                       [ Html.text ("Heavy Heavy-Duty Truck VMT")]
--                 , button
--                       [Attr.disabled (model.plotvars.detectorVMT || (not model.plotvars.detectorbased))
--                       ,Attr.class (if model.plotvars.detectorNHH
--                                    then "btn detectorNHH active"
--                                    else "btn detectorNHH off")
--                       , onClick DetectorNHH]
--                       [ Html.text ("Not Heavy Heavy-Duty Truck VMT")]
--                 ]
--             ]]

datepickbuilder : Model -> Html Msg
datepickbuilder model =
    let  y = (toString (Date.year model.date))
         m = (pad (monthToInt (Date.month model.date)))
         d  = (pad (Date.day model.date))
    in
        case model.fetchingColors of
            False -> label [] [Html.text "Date: "
                              ,DatePicker.view model.datePicker
                              |> App.map ToDatePicker]
            True -> label [class "datepicker-placeholder"] [Html.text ("Date: "++ y++"-"++m++"-"++d )]


mapcontrol : Model -> Html Msg
mapcontrol model =
    let  y = (toString (Date.year model.date))
         m = (pad (monthToInt (Date.month model.date)))
         d  = (pad (Date.day model.date))
         h = (pad model.hour)
         currday =   y++"-"++m++"-"++d++" "++h++":00"
         baddate = case  model.baddate of
                      Nothing -> ""
                      Just bd -> "No data for " ++ bd
         badclass = case model.baddate of
                       Nothing -> "goodday"
                       _       -> "badday"
    in
        div [Attr.class "mapcontrol col"]
            [div [Attr.class "row"]
                 [h2 []
                      [Html.text "Pick date and hour to display on map"]]
            ,div [Attr.class "row"]
                  [h2 [] [ Html.text <| currday ]]
            ,div [Attr.class "row"]
                [datepickbuilder model
                ,label [] [Html.text "Hour: "
                       , input [ Attr.type' "number"
                               , Attr.value (pad model.hour)
                               , Attr.min "-1"
                               , Attr.max "24"
                               , Attr.step "1"
                               , Attr.disabled model.fetchingColors
                               , onInput NewHour][]
                       ]
            ,button [ class "btn" ,Attr.disabled model.fetchingColors, onClick MorePlease ] [ Html.text ("get date")]
                  ]
            ,div [Attr.class ("row "++badclass)][Html.text baddate]
            ,pickbuttons model
            ,div [Attr.class "row"]
                [label [class "slider"] [Html.text ("Color Scale max: "++ (toString model.scaleDomain))
                                        ,input [ Attr.type' "range"
                                               , id "volrange"
                                               , Attr.min "1"
                                               , Attr.max "300000"
                                               , Attr.step "100"
                                               , Attr.value (toString model.scaleDomain)
                                               , Attr.name "volrange"
                                               --, Attr.list "volranges"
                                               , onInput ScaleDomain][]
                                        ]
                     ]
            ,div [Attr.class "row"]
                [label [class "slider"] [Html.text ("Color scale exponent: "++ (toString model.scaleExponent))
                      ,input [ Attr.type' "range"
                             , id "volrange"
                             , Attr.min "0.01"
                             , Attr.max "1"
                             , Attr.step "0.01"
                             , Attr.value (toString model.scaleExponent)
                             , Attr.name "exponentrange"
                             , onInput ScaleExponent][]
                          ]
                ]
            ]



view : Model -> Html Msg
view model =

        case model.records of
            Nothing ->
                (div [Attr.class "container"]
                     [div [Attr.class "row"][
                           div [Attr.class "mapapp col"][
                                Svg.svg [  width "500", height "536"][
                                     Svg.g [ class "tile", overflow "hidden", width "500", height "500"][]
                                    ,Svg.g [ class "grid", overflow "hidden", width "500", height "500"][]
                                    ]],
                           (mapcontrol model)
                               ]])
            Just records ->
                ( div [Attr.class "container"]
                      [div [Attr.class "row"][
                            div [Attr.class "mapapp col"][
                                 Svg.svg [  width "500", height "536"][
                                      Svg.g [ class "tile", width "500", height "500"][]
                                     ,Svg.g [ class "grid", width "500", height "500"] (svgpaths2 records model.data)
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
    let
                y = (toString (Date.year model.date))
                m= (pad (monthToInt (Date.month model.date)))
                d  = (pad (Date.day model.date))
                h = (pad model.hour)
                filePath = y++"/"++m++"/"++d++"/"++h++".json"
                url = model.dataUrl ++ "/" ++ filePath
            in
                Task.perform FetchDataFail FetchDataSucceed (Http.get gridDataDictionary url)


getter : (Dict String Float) -> String -> Float -> Float
getter  myd a start = start + (Maybe.withDefault 0.0 (Dict.get a myd))

-- set up the checkOn function
setDisabled : Bool -> PlottingButton -> PlottingButton
setDisabled setting button =
    {button | disabled=setting, on=(not setting) }

-- set up the checkOn function
checkOn : PlottingButton -> PlottingButton -> Bool
checkOn category button = category.on && button.on

-- set up the matchOn function
matchOn : String -> PlottingButton -> Bool
matchOn key button =  button.matchFn key

sumValues : Model -> (String -> Dict String Float -> Float -> Float)
sumValues model =
    -- check here whether to bother with dictkey
    let
        hpmsBased =  model.hpmsBased
        detectorBased = model.detectorBased

        relevantRoadTypes = Array.filter .relevant model.hpmsRoadTypes
        -- filter out irrelevant buttons (those not on)
        hpmsRoadTypes = List.filter (checkOn hpmsBased) (Array.toList relevantRoadTypes)
        hpmsPlotVars =  List.filter (checkOn hpmsBased) (Array.toList model.hpmsPlotVars)
        detectorPlotVars = List.filter (checkOn detectorBased) (Array.toList model.detectorPlotVars)

        -- now decide about this key.  look through each button
        -- it is either hpms based, or detector based entry.

        -- condense the tests.  If ANY pass, then keep the entry for summing
        -- need to check for degenerate cases
        buttonTests =  if detectorBased.on
                       then detectorBased :: hpmsRoadTypes
                       else hpmsRoadTypes

        summer : String -> Dict String Float -> Float -> Float
        summer dictkey mydict start =
            let
                isWorthy = List.any (matchOn dictkey) buttonTests
                -- blblb = Debug.log (dictkey ++ " isWorthy: ") isWorthy
            in
                if (not isWorthy)
                -- then ( Debug.log (dictkey ++ " skipping case ") (start + 0.0))
                then  (start + 0.0)
                else
                    let
                    -- filterDict = Dict.filter (dataTypeFilter pv) mydict
                        dplotlist = if detectorBased.on
                                     -- if a detector based thing, then use those plot vars
                                    then List.map (\l -> l.entry ) detectorPlotVars
                                    else []
                                    -- now to hpms conditions
                        hplotlist = if hpmsBased.on
                                    -- if an hpms based thing, then use those plot vars
                                    then List.map (\l -> l.entry ) hpmsPlotVars
                                    else []
                        plotlist = List.concat [dplotlist, hplotlist]
                    in
                        List.foldl (getter mydict) start plotlist
    in
        summer


gridReduce : (String -> Dict String Float -> Float -> Float) -> String ->  (Dict String (Dict String Float) ) -> Float
gridReduce redfn gridid griddata =
    Dict.foldl redfn  0.0 griddata

sumDataValues :  Model -> Dict String Float
sumDataValues model =
    let
        redfn =  sumValues model
    in

    Dict.map (gridReduce redfn)  model.colorData

dropZeros : String -> Float -> Bool
dropZeros k v =
    (not (v == 0.0))


getColorJson : Model -> Cmd Msg
getColorJson model =
    let
        datablob = ( Dict.toList (Dict.filter dropZeros (sumDataValues model)))
    in
        getColorJson2 { maxdomain = model.scaleDomain
                      , exponent = model.scaleExponent
                      , data = datablob}



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

mkDate : Int -> Int -> Int -> Int -> Date
mkDate year month day hour=
    toString year
        ++ "/"
        ++ pad month
        ++ "/"
        ++ pad day
        ++ " "
        ++ pad hour
        ++ ":00"
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
