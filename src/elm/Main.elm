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
import Set exposing (..)

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

type alias ColorResponse =
    {data : Dict String String
    ,newmax : Int}

type AreaType
    = County
    | Airbasin
    | Airdistrict
    | Statewide


type alias AreaMembership = Dict String (List String)





type alias Model =
    {file : String
    ,records : Maybe (List PathRecord)
    ,data : Maybe (Dict String String)
    ,county_membership : Maybe AreaMembership
    ,airdistrict_membership : Maybe AreaMembership
    ,airbasin_membership : Maybe AreaMembership
    ,areaPickerExpanded : Bool
    ,areaTypePicked : AreaType
    ,areasPicked : Maybe (Set String)
    ,dataUrl : String
    ,fetchingColors : Bool
    ,showingDate : Maybe String
    ,datePicker : DatePicker.DatePicker
    ,baddate : Maybe String
    ,date : Date
    ,hour : Int
    ,animate : Bool
    ,doAnimate : Bool
    ,hpmsBased : PlottingButton
    ,detectorBased : PlottingButton
    ,hpmsPlotVars : Array PlottingButton
    ,hpmsRoadTypes : Array PlottingButton
    ,detectorPlotVars : Array PlottingButton
    ,colorData : Dict String (Dict String (Dict String Float))
    ,sumVMT : Maybe String
    ,scaleDomain : Int
    ,autoMax : Bool
    ,scaleExponent : Float
    ,opacity : Float
    }

type alias ColorMessages =
    {maxdomain : Int
    ,exponent : Float
    ,data : List (String, Float)}

type alias Flags =
    {mapfile : String
    ,countymembershipfile : String
    ,airbasinmembershipfile : String
    ,airdistrictmembershipfile : String
    ,dataUrl : String
    ,year : Int
    ,month : Int
    ,day : Int
    ,hour: Int}

makeHpmsEntry : String -> String -> PlottingButton
makeHpmsEntry val txt =
     { entry = ""
     , value = val
     , class = "hpms btn"
     , text = txt
     , on = True, disabled = False
     , relevant = False
     , matchFn = \k -> (k == txt)
     }

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
        , county_membership = Nothing
        , airdistrict_membership = Nothing
        , airbasin_membership = Nothing
        , areaPickerExpanded = False
        , areaTypePicked = Statewide
        , areasPicked = Nothing
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

        , hpmsRoadTypes = Array.fromList [
                           { entry = ""
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
                          -- older hpms functional types

                          , makeHpmsEntry "rpai" "Rural Principal Arterial Interstate (PAI)"
                          , makeHpmsEntry "ropa" "Rural Other Principal Arterial (OPA)"
                          , makeHpmsEntry "rma"  "Rural Minor Arterial (MA)"
                          , makeHpmsEntry "rmjc" "Rural Major Collector (MJC)"
                          , makeHpmsEntry "rmnc" "Rural Minor Collector (MNC)"
                          , makeHpmsEntry "rloc" "Rural Local (LOC)"
                          , makeHpmsEntry "upai" "Urban Principal Arterial Interstate (PAI)"
                          , makeHpmsEntry "uofe" "Urban Principal Arterial Other Fwys & Exp (OFE)"
                          , makeHpmsEntry "uopa" "Urban Other Principal Arterial (OPA)"
                          , makeHpmsEntry "uma"  "Urban Minor Arterial (MA)"
                          , makeHpmsEntry "ucol" "Urban Collector (COL)"
                          , makeHpmsEntry "uloc" "Urban Local (LOC)"
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

        , datePicker = datePicker
        , baddate = Nothing
        , date = initdate
        , hour = (Date.hour initdate) -- should be same as fl.hour
        , animate = False
        , doAnimate = False
        , colorData = Dict.empty
        , fetchingColors = False
        , showingDate = Nothing
        , sumVMT = Nothing
        , scaleDomain = 160000
        , autoMax = False
        , scaleExponent = 0.3
        , opacity = 0.5}
         ! [ Cmd.batch([ getIt2 fl.mapfile
                       , getIt3 fl.countymembershipfile
                       , getIt4 fl.airdistrictmembershipfile
                       , getIt5 fl.airbasinmembershipfile
                       , Cmd.map ToDatePicker datePickerFx
                       ])]





-- UPDATE

port d3Update : (List String) -> Cmd msg
port getTopoJson : Json.Value -> Cmd msg
port getColorJson2 : ColorMessages  -> Cmd msg
port getFormattedVMT : Float -> Cmd msg

-- port for listening for translated features from JavaScript
port features : (List PathRecord -> msg) -> Sub msg
port colors   : (Json.Value -> msg) -> Sub msg
port vmt : (String -> msg) -> Sub msg

type BtnMsg
    = DetectorBased
    | HpmsBased
    | HpmsRoadTypes
    | HpmsPlotVars
    | DetectorPlotVars

type Msg
  = MorePlease
  | FetchSucceed2 Json.Value
  | FetchSucceed3 AreaMembership
  | FetchSucceed4 AreaMembership
  | FetchSucceed5 AreaMembership
  | FetchDataSucceed (Dict String (Dict String (Dict String Float)))
  | IdPath (List PathRecord)
  | ColorMap Json.Value
  | FetchFail Http.Error
  | FetchDataFail Http.Error
  | ToDatePicker DatePicker.Msg
  | NewHour String
  | HandleButton BtnMsg Int String
  | AreaPickerToggle
  | HandleArea AreaType
  | ToggleArea String
  | ScaleDomain String
  | SumVMT String
  | AutoMax
  | ScaleExponent String
  | ScaleOpacity String
  | Animate

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
                        , inputName = Just "datepicker"
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
                -- adjust the hour correctly
                diffhour = model.hour - ( Date.hour date )
                newdate = (Period.add Period.Hour diffhour date)
                -- adjust the HPMS buttons based on year
                yr = Date.year newdate
            in
             { model
                  | date = newdate
                  , datePicker = datePicker
              }
             ! [ Cmd.map ToDatePicker datePickerFx]


    MorePlease ->
        ({model | fetchingColors = True, doAnimate=False} , getData model)

    Animate ->
        let
            newanimate = (not model.animate)
        in
            ({model | animate = newanimate, doAnimate = newanimate
             }
            , if newanimate
              then Cmd.Extra.message MorePlease
              else Cmd.none)

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

    AreaPickerToggle ->
        ({model | areaPickerExpanded = (not model.areaPickerExpanded)}
             ,Cmd.none)

    HandleArea msg ->
        let
            newareas = if msg == model.areaTypePicked
                       then model.areasPicked
                       else Nothing
            newmodel = {model | areaTypePicked = msg
                              , areasPicked = newareas
                       }
        in
        (newmodel, getColorJson newmodel)

    ToggleArea msg ->
        let
            newAreas = (case model.areasPicked of
                            Nothing ->
                                Set.singleton msg
                            Just a ->
                                if Set.member msg a
                                then Set.remove msg a
                                else Set.insert msg a
                       )
            newmodel = {model | areasPicked = Just newAreas}
        in
            (newmodel , getColorJson newmodel)


    SumVMT sumvmt ->
        ({model | sumVMT = Just sumvmt},Cmd.none)

    AutoMax ->
        ({model | autoMax = (not model.autoMax)}
        , getColorJson {model |  autoMax = (not model.autoMax)})

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

    ScaleOpacity rec ->
        let
            newOpacity = Result.withDefault model.opacity (String.toFloat rec)
        in
            ({model | opacity = newOpacity}
            , Cmd.none )


    IdPath newFeatures ->
        ({model | records = Just newFeatures}, Cmd.none)

    ColorMap incoming ->
        let
            cr = Maybe.withDefault {data = Dict.empty, newmax = model.scaleDomain}  (Result.toMaybe(Json.decodeValue colorResponse incoming))
        in
        ({model | scaleDomain = cr.newmax
                , data = Just cr.data}
        ,if (model.animate && model.doAnimate)
         then Cmd.Extra.message MorePlease
         else Cmd.none)

    FetchSucceed2 rec ->
        (model, getTopoJson rec)

    FetchSucceed3 rec ->
        ({model | county_membership = Just rec} , Cmd.none)

    FetchSucceed4 rec ->
        ({model | airdistrict_membership = Just rec} , Cmd.none)

    FetchSucceed5 rec ->
        ({model | airbasin_membership = Just rec} , Cmd.none)

    FetchDataSucceed rec ->
        let
            yr = Date.year model.date
            y = (toString yr)
            m = (pad (monthToInt (Date.month model.date)))
            d  = (pad (Date.day model.date))
            h = (pad model.hour)
            newDateFetched =  (toString (Date.dayOfWeek model.date)) ++", " ++ y++"-"++m++"-"++d++" "++h++":00"
        -- adjust the HPMS buttons based on year
            len = Array.length model.hpmsRoadTypes
            newtypes = if yr<2012
                       then
                           Array.map
                               (setRelevant False)
                               (Array.slice 0 3 model.hpmsRoadTypes)
                       else
                           Array.map
                               (setRelevant True)
                               (Array.slice 0 3 model.hpmsRoadTypes)
            oldtypes = if yr<2012
                       then
                           Array.map
                               (setRelevant True)
                               (Array.slice 3 len model.hpmsRoadTypes)
                       else
                           Array.map
                               (setRelevant False)
                               (Array.slice 3 len model.hpmsRoadTypes)
            hpmsRoadTypes = Array.append newtypes oldtypes
        in
            -- let the UI know the data is back
            ({model | fetchingColors = False
                    , doAnimate = True
                    , showingDate = Just newDateFetched
                    , baddate=Nothing
                    , colorData = rec
                    , hpmsRoadTypes = hpmsRoadTypes
             }
            -- and now go get the right colors for the retrieved data
            ! [ getColorJson {model | fetchingColors = False
                             ,showingDate = Just newDateFetched
                             ,baddate=Nothing
                             ,colorData = rec
                             , hpmsRoadTypes = hpmsRoadTypes
                             }
              ,Cmd.Extra.message (NewHour (toString (model.hour + 1) ))])

    FetchDataFail e ->
        let
            y = (toString (Date.year model.date))
            m = (pad (monthToInt (Date.month model.date)))
            d  = (pad (Date.day model.date))
            h = (pad model.hour)
            newDateFailed =  if model.animate
                             then y++"-"++m++"-"++d++" "++h++":00  (Animation Canceled)"
                             else y++"-"++m++"-"++d++" "++h++":00"

        in
            -- let the UI know the date has no data
            ({model| baddate=Just newDateFailed
             ,fetchingColors = False
             ,animate = False, doAnimate = False}, Cmd.none)

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
        , vmt SumVMT
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


datepickbuilder : Model -> Html Msg
datepickbuilder model =
    let  y = (toString (Date.year model.date))
         m = (pad (monthToInt (Date.month model.date)))
         d  = (pad (Date.day model.date))
    in
        case model.fetchingColors of
            False -> DatePicker.view model.datePicker
                         |> App.map ToDatePicker
            True -> input [Attr.class "disabled-date form-control"
                           , Attr.disabled True
                           , Attr.name "datepicker"
                           , Attr.value (y++"-"++m++"-"++d)][]



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
         animateclass = if model.animate
                        then "btn animate active"
                        else "btn animate"
         automaxclass = if model.autoMax
                        then "btn max active"
                        else "btn max"
         slidermaxclass = if model.autoMax
                          then "slider slider-disabled"
                          else "slider"
    in
        div [Attr.class "mapcontrol col"]
            [div [Attr.class "row"]
                 [h2 []
                      [Html.text "Display hourly estimates of California Vehicle Activity"]
                 ,p [][Html.text
                           """
                            This page allows you to view the
                            estimated vehicle travel activity for any
                            hour in the CalVAD database for the entire
                            state of California.  Choose a date using
                            the date picker (you can just type in the
                            year month and day if you want) and select
                            the hour and then press the \"get date\"
                            button.  The \"animate\" button automates
                            the incrementing of hours.  You can pan and
                            zoom the map, and all of the buttons work
                            to select different subsets of the
                            downloaded data.
                            """
                      ]]
            ,div [Attr.class "row"]
                  [h2 [] [ Html.text <| currday ]]
            ,div [Attr.class "daterow"]
                [ label [Attr.for "datepicker"]  [Html.text "Date: "]
                , datepickbuilder model
                , label [Attr.for "Hour"] [Html.text "Hour: "]
                , div [Attr.class "hours"][
                      input [ Attr.class "hours form-control"
                            , Attr.type' "number"
                            , Attr.value (pad model.hour)
                            , Attr.min "-1"
                            , Attr.max "24"
                            , Attr.step "1"
                            , Attr.name "Hour"
                            , Attr.disabled model.fetchingColors
                            , onInput NewHour][]
                     ]
            ,button [ class "btn" ,Attr.disabled model.fetchingColors, onClick MorePlease ] [ Html.text ("get date")]
            ,button [ class animateclass , onClick Animate ] [ Html.text ("Animate data")]
                  ]
            ,div [Attr.class ("row "++badclass)][Html.text baddate]
            ,areaPickerControl model
            ,areaSelectorControl model
            ,pickbuttons model
            ,div [Attr.class "row"]
                [label [Attr.for "volrange"
                       ,Attr.class slidermaxclass]
                     [Html.text ("Color Scale max: "++ (toString model.scaleDomain))]
                ,button [ class automaxclass , onClick AutoMax ] [ Html.text ("set max automatically")]
                ,input [ Attr.type' "range"
                       , id "volrange"
                       , Attr.min "100"
                       , Attr.max "200000"
                       , Attr.step "100"
                       , Attr.value (toString model.scaleDomain)
                       , Attr.name "volrange"
                       , Attr.disabled model.autoMax
                       , onInput ScaleDomain][]

                ,label [Attr.for "exponentrange"
                       ,Attr.class "slider"]
                     [Html.text ("Color scale exponent: "++ (toString model.scaleExponent))]
                ,input [ Attr.type' "range"
                       , id "exponentrange"
                       , Attr.min "0.01"
                       , Attr.max "1"
                       , Attr.step "0.01"
                       , Attr.value (toString model.scaleExponent)
                       , Attr.name "exponentrange"
                       , onInput ScaleExponent][]

                , label [Attr.for "opacity"
                        ,Attr.class "slider"]
                     [Html.text ("Data opacity: "++ (toString model.opacity))]
                ,input [ Attr.type' "range"
                       , id "opacity"
                       , Attr.min "0"
                       , Attr.max "1"
                       , Attr.step ".01"
                       , Attr.value (toString model.opacity)
                       , Attr.name "opacity"
                       , onInput ScaleOpacity][]
                     ]

                ]


areaPickerControl : Model -> Html Msg
areaPickerControl model =

    (div [Attr.class "areainputs row"]
     [div [Attr.class "btn-group"
          ,Attr.attribute "role" "group" ]
          [button [Attr.type' "button"
                  ,Attr.class (if model.areaTypePicked == Statewide
                               then "btn btn-default active"
                               else "btn btn-default")
                  ,onClick (HandleArea Statewide)]
               [ Html.text "Statewide" ]
          ,button [Attr.type' "button"
                  ,Attr.class (if model.areaTypePicked == County
                               then "btn btn-default active"
                               else "btn btn-default")
                  ,onClick (HandleArea County)]
               [ Html.text "County" ]
          ,button [Attr.type' "button"
                  ,Attr.class (if model.areaTypePicked == Airbasin
                               then "btn btn-default active"
                               else "btn btn-default")
                  ,onClick (HandleArea Airbasin)]
               [ Html.text "Air Basin" ]
          ,button [Attr.type' "button"
                  ,Attr.class (if model.areaTypePicked == Airdistrict
                               then "btn btn-default active"
                               else "btn btn-default")
                  ,onClick (HandleArea Airdistrict)]
               [ Html.text "Air District" ]
          ]
     ,areaDropdownControl model
     ])


areaToggleButton : Set String -> String -> Html Msg
areaToggleButton picked area =
    let
        cls = if (Set.member area picked)
              then "btn area active"
              else "btn area"
    in
    button [Attr.type' "button"
            ,Attr.class cls
            ,onClick (ToggleArea area)]
         [Html.text area]




areaDropdownControl : Model -> Html Msg
areaDropdownControl model =
    if model.areaTypePicked == Statewide
    then (div [Attr.class "hide"][])
    else -- do something
        (div [class "dropdown"]
             [button [Attr.type' "button"
                     ,id "areapickerdropdown"
                     ,Attr.class "btn btn-default dropdown-toggle"
                     ,Attr.attribute "data-toggle" "dropdown"
                     ,Attr.attribute "aria-haspopup" "true"
                     ,Attr.attribute "aria-expanded" (if model.areaPickerExpanded
                                                      then "true"
                                                      else "false")
                     ,onClick AreaPickerToggle
                     ][Html.text "Select", (span [class "caret"][])]
             ])

areaSelectorControl : Model -> Html Msg
areaSelectorControl model =
    if model.areaTypePicked == Statewide || (not model.areaPickerExpanded)
    then (div [Attr.class "hide"][])
    else -- do something
        let
            buttonlist = case model.areaTypePicked of
                             County -> Dict.keys (Maybe.withDefault Dict.empty model.county_membership)
                             Airbasin -> Dict.keys (Maybe.withDefault Dict.empty model.airbasin_membership)
                             Airdistrict -> Dict.keys (Maybe.withDefault Dict.empty model.airdistrict_membership)
                             _ -> [] -- todo
        in
            (div [Attr.class "areainputs row"]
                 (List.map (areaToggleButton
                                (Maybe.withDefault
                                     Set.empty model.areasPicked))
                      buttonlist)
            )


view : Model -> Html Msg
view model =

        case model.records of
            Nothing ->
                (div [Attr.class "container"]
                     [div [Attr.class "row"][
                           div [Attr.class "mapapp col"][
                                Svg.svg [  width "500", height "500"][
                                     Svg.g [ class "tile", overflow "hidden", width "500", height "500"][]
                                    ,Svg.g [ class "grid"
                                           , overflow "hidden"
                                           , width "500"
                                           , height "500"
                                           , Attr.style [("fill-opacity", (toString model.opacity))]]
                                         []
                                    ]],
                           (mapcontrol model)
                               ]])
            Just records ->
                ( div [Attr.class "container"]
                      [div [Attr.class "row"][
                            div [Attr.class "mapapp col"][
                                 Svg.svg [  width "500", height "500"]
                                     [Svg.g [ class "tile", width "500", height "500"][]
                                     ,Svg.g [ class "grid"
                                            , width "500"
                                            , height "500"
                                            , Attr.style [("fill-opacity", (toString model.opacity))]
                                            ] (svgpaths2 records model.data)
                                     ]
                                     ,Svg.svg [  width "500", height "400"]
                                     [Svg.text'
                                         [x "250"
                                         , y "24"
                                         , fontSize "24"
                                         , alignmentBaseline "middle"
                                         , textAnchor "middle"
                                         , class "maplabel"]
                                          [Svg.text (Maybe.withDefault "No date selected" model.showingDate)]

                                     ,Svg.g [ class "hist"
                                            , width "500"
                                            , height "200"
                                            , SvgAttr.transform "translate(10,60)"][]

                                     ,Svg.g [ class "sums"
                                            , width "500"
                                            , height "100"
                                            , SvgAttr.transform "translate(0,340)"]
                                         [Svg.text'[x "250"
                                                   , y "24"
                                                   , fontSize "24"
                                                   , alignmentBaseline "middle"
                                                   , textAnchor "middle"
                                                   , class "vmtsum"][Svg.text  (case model.sumVMT of
                                                                            Nothing -> ""
                                                                            Just vmt -> ("Total VMT ≈ " ++ vmt))]]
                                     ]
                                ]

                                ,(mapcontrol model)
                           ]
                      ]

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

-- set up the setDisabled function
setDisabled : Bool -> PlottingButton -> PlottingButton
setDisabled setting button =
    {button | disabled=setting, on=(not setting) }

-- set up the setRelevant function
setRelevant : Bool -> PlottingButton -> PlottingButton
setRelevant setting button =
    {button | relevant=setting }

-- set up the checkOn function
checkOn : PlottingButton -> PlottingButton -> Bool
checkOn category button = category.on && button.on

-- set up the matchOn function
matchOn : String -> PlottingButton -> Bool
matchOn key button =  button.matchFn key


makeSummer : Model -> String -> (String,Float)
makeSummer model  =
    let
        -- first, what are the keys I'm going to be summing up inside
        -- the grid's dictionary?
        hpmsBased =  model.hpmsBased
        detectorBased = model.detectorBased

        relevantRoadTypes = Array.filter .relevant model.hpmsRoadTypes
        -- filter out irrelevant buttons (those not on)
        hpmsRoadTypes = List.filter (checkOn hpmsBased) (Array.toList relevantRoadTypes)
        hpmsPlotVars =  List.filter (checkOn hpmsBased) (Array.toList model.hpmsPlotVars)
        detectorPlotVars = List.filter (checkOn detectorBased) (Array.toList model.detectorPlotVars)

        -- condense the tests.  If ANY pass, then keep the entry for summing
        -- need to check for degenerate cases
        buttonTests =  if detectorBased.on
                       then detectorBased :: hpmsRoadTypes
                       else hpmsRoadTypes

        -- if a detector based thing, then
        -- use those plot vars
        dplotlist = if detectorBased.on
                    then List.map (\l -> l.entry ) detectorPlotVars
                    else []
        -- now to hpms conditions
        hplotlist = if hpmsBased.on
                    then List.map (\l -> l.entry ) hpmsPlotVars
                    else []
        plotlist = List.concat [dplotlist, hplotlist]

        gridDictResult : String -> (String, Float)
        gridDictResult gridkey =
            let
                -- then, what is the grid?
                mydict = Dict.get gridkey model.colorData
                -- mydict is a Dict String (Dict String Float)
                         -- where the first string is road types, and the
                         -- second string is data types (vmt, etc)
            in
                case mydict of
                    Nothing -> (gridkey, 0)
                    Just d  ->
                        -- have a dictionary.  sum over road types
                        -- sum up the relevant keys
                        let
                            -- make a summation function
                            -- for dictonary pass
                            dictsum :  String -> (Dict String Float) -> Float -> Float
                            dictsum dictkey thisdict total =

                           -- decide about this key.  look through each button it
                           -- is either hpms based, or detector based entry.
                                let
                                    isWorthy = List.any (matchOn dictkey) buttonTests
                                in
                                    if (not isWorthy)
                                    -- then ( Debug.log (dictkey ++ " skipping case ") (start + 0.0))
                                    then
                                        (total + 0.0)
                                    else
                                        List.foldl (getter thisdict) total plotlist
                        in
                            (gridkey, Dict.foldl dictsum 0.0 d)

    in
        gridDictResult


subsetAreas : Set String -> String -> (List String) -> Bool
subsetAreas keeplist area _ =
    Set.member area keeplist



-- iterate over the colorData, summing up the appropriate values
-- inside each grid cell
sumDataValues :  Model -> List (String, Float)
sumDataValues model =
    let
        -- use gridkeys here, so that I can pick just county, etc later
        gridkeys = if model.areaTypePicked == Statewide
                   then Dict.keys model.colorData
                   else
                       let
                           -- for each of the picked areas,
                           -- concatenate the "county" grid
                           -- entries that match

                           aplist = Maybe.withDefault Set.empty model.areasPicked
                           membership = case model.areaTypePicked of
                                            County -> Maybe.withDefault Dict.empty model.county_membership
                                            Airbasin -> Maybe.withDefault Dict.empty model.airbasin_membership
                                            Airdistrict -> Maybe.withDefault Dict.empty model.airdistrict_membership
                                            _ -> Dict.empty

                           griddict = Dict.filter (subsetAreas aplist) membership
                           gridlists = Dict.values griddict
                       in
                           List.concat gridlists

        -- for each key, I need to sum up the appropriate values in
        -- the colorData dictionary of data.  So make the summer,
        -- which is based on the model, using the current state of the
        -- various button presses
        redfn =  makeSummer model

    in

        -- then that reduce function can be applied to the grid keys,
        -- and will generate a list of (String, Float)
        List.map redfn gridkeys


dropZeros : (String, Float) -> Bool
dropZeros (_, v) =
    (not (v == 0.0))

sumBlob : (String, Float) -> Float -> Float
sumBlob (_, value) sum =    value + sum


getColorJson : Model -> Cmd Msg
getColorJson model =
    let
        -- datablob can become a JS object of key,value pairs
        -- gridid : Float
        -- so that getColorJson2 can convert Float to colors
        --
        -- sumDataValues model does the summing, needs to generate a
        -- list of string,float

        datablob =  (List.filter dropZeros (sumDataValues model))
        sumvmt = List.foldr sumBlob 0 datablob
    in
        Cmd.batch([ getFormattedVMT sumvmt
                  , getColorJson2 { maxdomain = if model.autoMax
                                                then 0
                                                else model.scaleDomain
                                  , exponent = model.scaleExponent
                                  , data = datablob}
                      ])


getIt2 : String -> Cmd Msg
getIt2 f =
  let
    url = f
  in
    Task.perform FetchFail FetchSucceed2 (Http.get decodeResult2 url)


decodeResult2 : Json.Decoder Json.Value
decodeResult2 = Json.value


getIt3 : String -> Cmd Msg
getIt3 url =
    Task.perform FetchFail FetchSucceed3 (Http.get decodeResult3 url)

getIt4 : String -> Cmd Msg
getIt4 url =
    Task.perform FetchFail FetchSucceed4 (Http.get decodeResult3 url)

getIt5 : String -> Cmd Msg
getIt5 url =
    Task.perform FetchFail FetchSucceed5 (Http.get decodeResult3 url)

decodeResult3 : Json.Decoder AreaMembership
decodeResult3 =
     Json.dict (Json.list Json.string)


gridDataDictionary : Json.Decoder (Dict String (Dict String (Dict String Float)))
gridDataDictionary = dict (dict ( dict float))


colorDictionary : Json.Decoder (Dict String String)
colorDictionary = dict Json.string

colorResponse : Json.Decoder ColorResponse
colorResponse = object2 ColorResponse ("data" := colorDictionary) ("max" := Json.int)


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
