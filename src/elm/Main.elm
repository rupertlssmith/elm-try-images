module Main exposing (Model, Msg, main)

import Array
import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Color
import Css
import Css.Global
import Frame2d exposing (Frame2d)
import Geometry.Svg
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy
import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Html.Styled.Events as HSE
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as DE
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Unitless)
import Rectangle2d exposing (Rectangle2d)
import Sticky.Editor as Editor
import Task
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core as SvgCore exposing (Svg)
import TypedSvg.Types
    exposing
        ( Align(..)
        , AnchorAlignment(..)
        , MeetOrSlice(..)
        , Opacity(..)
        , Paint(..)
        , Scale(..)
        , ShapeRendering(..)
        , StrokeLinecap(..)
        , StrokeLinejoin(..)
        , TextRendering(..)
        , Transform(..)
        )
import Update2
import Vector2d exposing (Vector2d)


config =
    let
        fontSize =
            30

        lineHeightRatio =
            1.4
    in
    { fontSize = fontSize
    , lineHeight = (lineHeightRatio * fontSize) |> floor |> toFloat
    , zoomLevels =
        [ 0.25, 0.33, 0.5, 0.67, 0.75, 0.8, 0.9, 1, 1.1, 1.25, 1.5, 1.75, 2.0, 2.5, 3.0, 4.0, 5.0 ]
            |> Array.fromList
    , defaultZoomLevel = 7
    , maxZoomLevel = 16
    , minZoomLevel = 0
    , defaultSize = Vector2d.unitless 400 400
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Model
    = SizingWindow
    | Ready DrawingModel
    | ResizingPostIt PScreen VScene DrawingModel


type alias DrawingModel =
    { frame : RScreen
    , postIt : RScene
    , userSize : VScene
    , zoomLevel : Int
    , fontLevel : Int
    , editor : Editor.Model
    }


type Msg
    = EditorMsg Editor.Msg
    | WindowSize VScreen
    | EditorResize VScreen
    | Zoom WheelEvent
    | PostItResize PScreen
    | MouseMoved PScreen
    | EndGesture
    | FontBigger
    | FontSmaller


type Scene
    = Scene


type alias VScene =
    Vector2d Unitless Scene


type alias RScene =
    Rectangle2d Unitless Scene


type Screen
    = Screen


type alias VScreen =
    Vector2d Pixels Screen


type alias PScreen =
    Point2d Pixels Screen


type alias RScreen =
    Rectangle2d Pixels Screen


init : () -> ( Model, Cmd Msg )
init _ =
    ( SizingWindow
    , Task.perform (viewportToSize >> WindowSize) Browser.Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        ResizingPostIt _ _ _ ->
            [ Browser.Events.onResize coordsToSize |> Sub.map WindowSize
            , Browser.Events.onMouseUp (Decode.succeed EndGesture)
            , Browser.Events.onMouseMove (mousePosDecoder |> Decode.map MouseMoved)
            ]
                |> Sub.batch

        _ ->
            [ Browser.Events.onResize coordsToSize |> Sub.map WindowSize
            ]
                |> Sub.batch


noop model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, Debug.log "msg" msg ) of
        ( Ready drawingModel, EditorMsg editorMsg ) ->
            let
                editorUpdate =
                    Update2.lift
                        .editor
                        (\x m -> { m | editor = x })
                        EditorMsg
                        Editor.update
                        editorMsg
            in
            editorUpdate drawingModel
                |> Tuple.mapFirst Ready
                |> Tuple.mapSecond (always Cmd.none)

        ( SizingWindow, WindowSize windowSize ) ->
            noop
                (Ready
                    { frame = windowSizeToFrame windowSize
                    , postIt =
                        Rectangle2d.with
                            { x1 = Quantity.float 50
                            , y1 = Quantity.float 50
                            , x2 = Quantity.float 450
                            , y2 = Quantity.float 450
                            }
                    , userSize = config.defaultSize
                    , zoomLevel = config.defaultZoomLevel
                    , fontLevel = config.defaultZoomLevel
                    , editor = Editor.init
                    }
                )

        ( Ready readyModel, EditorResize editorSize ) ->
            adjustSize editorSize readyModel
                |> Tuple.mapFirst Ready

        ( Ready readyModel, Zoom wheelEvent ) ->
            let
                levelChange =
                    wheelEventToScale wheelEvent |> round

                newZoomLevel =
                    (readyModel.zoomLevel - levelChange)
                        |> clamp config.minZoomLevel config.maxZoomLevel
            in
            ( { readyModel | zoomLevel = newZoomLevel } |> Ready
            , Cmd.none
            )

        ( Ready drawingModel, PostItResize start ) ->
            ( ResizingPostIt start drawingModel.userSize drawingModel
            , Cmd.none
            )

        ( ResizingPostIt startPos startSize drawingModel, MouseMoved current ) ->
            resizePostIt startPos current startSize drawingModel
                |> Update2.andThen (adjustSize Vector2d.zero)
                |> Tuple.mapFirst (ResizingPostIt startPos startSize)

        ( ResizingPostIt _ _ drawingModel, EndGesture ) ->
            ( Ready drawingModel
            , Cmd.none
            )

        ( Ready drawingModel, FontBigger ) ->
            let
                newFontLevel =
                    (drawingModel.fontLevel + 1)
                        |> clamp config.minZoomLevel config.maxZoomLevel
            in
            ( { drawingModel | fontLevel = newFontLevel } |> Ready
            , Cmd.none
            )

        ( Ready drawingModel, FontSmaller ) ->
            let
                newFontLevel =
                    (drawingModel.fontLevel - 1)
                        |> clamp config.minZoomLevel config.maxZoomLevel
            in
            ( { drawingModel | fontLevel = newFontLevel } |> Ready
            , Cmd.none
            )

        _ ->
            noop model


adjustSize : VScreen -> DrawingModel -> ( DrawingModel, Cmd Msg )
adjustSize editorSize readyModel =
    let
        { x, y } =
            rectToXywh readyModel.postIt

        ( editorWidth, editorHeight ) =
            Vector2d.toTuple Pixels.toFloat editorSize

        defaultW =
            Vector2d.xComponent readyModel.userSize |> Quantity.toFloat

        defaultH =
            Vector2d.yComponent readyModel.userSize |> Quantity.toFloat

        newPostIt =
            Rectangle2d.with
                { x1 = Quantity.float x
                , y1 = Quantity.float y
                , x2 = Quantity.float (x + max defaultW editorWidth)
                , y2 = Quantity.float (y + max defaultH editorHeight)
                }
    in
    ( { readyModel | postIt = newPostIt }
    , Cmd.none
    )


resizePostIt : PScreen -> PScreen -> VScene -> DrawingModel -> ( DrawingModel, Cmd Msg )
resizePostIt start current startSize drawingModel =
    let
        scaleFactor =
            Array.get drawingModel.zoomLevel config.zoomLevels
                |> Maybe.withDefault 1.0

        rate =
            Pixels.pixels scaleFactor
                |> Quantity.per (Quantity.float 1)

        delta =
            Vector2d.from start current
                |> Vector2d.at_ rate
                |> Vector2d.toUnitless
                |> Vector2d.fromUnitless
    in
    ( { drawingModel
        | userSize =
            Vector2d.plus startSize delta
      }
    , Cmd.none
    )


coordsToSize : Int -> Int -> VScreen
coordsToSize x y =
    Vector2d.pixels (toFloat x) (toFloat y)


viewportToSize : Viewport -> VScreen
viewportToSize vport =
    Vector2d.pixels vport.viewport.width vport.viewport.height


windowSizeToFrame : VScreen -> RScreen
windowSizeToFrame size =
    Rectangle2d.from
        (Point2d.pixels 0 0)
        (Point2d.xy (Vector2d.xComponent size) (Vector2d.yComponent size))



-- Styling


offWhite =
    Color.rgb255 248 240 245


global : List Css.Global.Snippet
global =
    [ Css.Global.html
        [ Css.pct 100 |> Css.height ]
    , Css.Global.body
        [ Css.pct 100 |> Css.height ]
    , Css.Global.id "post-it-note"
        [ Css.pct 100 |> Css.width
        , Css.pct 100 |> Css.height
        , Css.backgroundColor (Css.rgb 225 225 20)
        , Css.alignItems Css.center -- Vertical alignment, center, flexStart, flexEnd all work.
        , Css.justifyContent Css.center -- Horizontal alignment, center and flexStart work.
        , Css.displayFlex
        ]
    , Css.Global.id "content-main"
        [ Css.position Css.absolute
        , Css.em 1 |> Css.paddingLeft
        , Css.em 1 |> Css.paddingRight
        , Css.em 0.5 |> Css.paddingTop
        , Css.em 0.5 |> Css.paddingBottom
        , Css.outline3 (Css.px 0) Css.solid Css.transparent
        , Css.property "user-select" "text"
        , Css.property "-moz-user-select" "text"
        , Css.property "-webkit-user-select" "text"
        , Css.property "-ms-user-select" "text"
        , Css.backgroundColor (Css.rgb 200 200 20)
        , Css.textAlign Css.center -- Use center when justify content above is center.
        ]
    , Css.Global.id "resize-control"
        [ Css.cursor Css.nwseResize
        ]
    , Css.Global.selector "::selection"
        [ Css.backgroundColor (Css.rgb 196 195 217)
        ]
    ]


css : String
css =
    """
.rte-main {
    outline: none;
}

.rte-hide-caret {
    caret-color: transparent;
}

/*
 * Workaround for https://github.com/mweiss/elm-rte-toolkit/issues/16, iOS has issues
 * changing caret color on elements that are already selected.
 */
@supports (-webkit-touch-callout: none) {
    .rte-hide-caret {
        caret-color: auto;
    }
}

.rte-main p {
    margin-bottom: 0.5em;
    margin-top: 0.5em;
}    
    """



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "SVG Text Editing Example"
    , body =
        [ H.node "style" [] [ H.text css ]
        , Css.Global.global global |> HS.toUnstyled
        , body model
        ]
    }


body : Model -> Html Msg
body model =
    H.div
        [ wheelDecoder
            |> Decode.map Zoom
            |> noPropagationOn "wheel"
        ]
        [ Html.Lazy.lazy fullBody model
        ]


fullBody : Model -> Html Msg
fullBody model =
    case model of
        Ready drawing ->
            let
                _ =
                    Editor.getControlContext drawing.editor
                        |> Debug.log "controlState"
            in
            H.div
                [ HA.style "width" "100%"
                , HA.style "height" "100%"
                , HA.style "overflow" "hidden"
                ]
                [ diagram drawing
                ]

        ResizingPostIt _ _ drawing ->
            H.div
                [ HA.style "width" "100%"
                , HA.style "height" "100%"
                , HA.style "overflow" "hidden"
                ]
                [ diagram drawing
                ]

        _ ->
            H.div [] []


diagram : DrawingModel -> Html Msg
diagram diag =
    let
        frame =
            diag.frame

        scaleFactor =
            Array.get diag.zoomLevel config.zoomLevels |> Maybe.withDefault 1.0

        pixelsPerUnit =
            Pixels.pixels scaleFactor |> Quantity.per (Quantity.float 1)

        { x, y, w, h } =
            rectToXywhPixels frame
    in
    Svg.svg
        [ SvgAttr.preserveAspectRatio (Align ScaleMid ScaleMid) Meet
        , SvgAttr.viewBox (round x |> toFloat)
            (round y |> toFloat)
            (round w |> toFloat)
            (round h |> toFloat)
        , SvgCore.svgNamespace
        , SvgAttr.shapeRendering RenderGeometricPrecision
        ]
        [ Svg.g
            []
            [ background diag
            , editableTextForeignObject diag
            , lowerControlBar diag
            ]
            |> Geometry.Svg.at pixelsPerUnit
        ]


background : DrawingModel -> Svg msg
background { frame } =
    let
        skirtScale =
            10

        ( w, h ) =
            Rectangle2d.dimensions frame
                |> Tuple.mapBoth Pixels.toFloat Pixels.toFloat

        bgArea =
            Rectangle2d.with
                { x1 = -(skirtScale * w) |> Quantity.float
                , y1 = -(skirtScale * h) |> Quantity.float
                , x2 = (2 * skirtScale) * w |> Quantity.float
                , y2 = (2 * skirtScale) * h |> Quantity.float
                }
    in
    Geometry.Svg.rectangle2d
        [ SvgAttr.fill <| Paint offWhite
        , SvgAttr.fillOpacity <| Opacity 0.8
        , InPx.strokeWidth 0
        ]
        bgArea


editableTextForeignObject : DrawingModel -> Svg Msg
editableTextForeignObject model =
    let
        { x, y, w, h } =
            rectToXywh model.postIt
    in
    SvgCore.foreignObject
        [ InPx.x x
        , InPx.y y
        , InPx.width w
        , InPx.height h
        ]
        [ H.div
            [ HA.id "post-it-note" ]
            [ editableContent model
            ]
        ]


lowerControlBar : DrawingModel -> Svg Msg
lowerControlBar { postIt } =
    let
        { x, y, w, h } =
            rectToXywh postIt

        control =
            Rectangle2d.with
                { x1 = x + w - 40 |> Quantity.float
                , y1 = y + h - 40 |> Quantity.float
                , x2 = x + w |> Quantity.float
                , y2 = y + h |> Quantity.float
                }
    in
    Svg.g
        [ HA.id "resize-control" ]
        [ Geometry.Svg.rectangle2d
            [ SvgAttr.fill <| Paint (Color.rgb255 200 200 20)
            , SvgAttr.fillOpacity <| Opacity 0.8
            , InPx.strokeWidth 0
            , HE.on "mousedown" (mousePosDecoder |> Decode.map PostItResize)
            ]
            control
        ]



-- contenteditable stuff


editableContent : DrawingModel -> Html Msg
editableContent model =
    let
        scaleFactor =
            Array.get model.fontLevel config.zoomLevels |> Maybe.withDefault 1.0
    in
    HS.node "elm-resize"
        [ HSA.css
            [ config.lineHeight * scaleFactor |> Css.px |> Css.lineHeight
            , config.fontSize * scaleFactor |> Css.px |> Css.fontSize
            ]
        , HSA.id "content-main"

        --, HSA.contenteditable True
        , resizeDecoder
            |> Decode.map EditorResize
            |> Decode.map (\val -> ( val, True ))
            |> HSE.stopPropagationOn "resize"
        ]
        [ H.map EditorMsg (Editor.view model.editor) |> HS.fromUnstyled

        --  HS.node "elm-editable"
        --     [ HSA.attribute "spellcheck" "false"
        --     , HSA.attribute "autocorrect" "off"
        --     , HSA.attribute "autocapitalize" "off"
        --     ]
        --     [ HS.div [] [ HS.text "Editable Post-It Note" ]
        --     , HS.node "selection-handler"
        --         []
        --         []
        --     ]
        ]
        |> HS.toUnstyled



-- Resize Events


resizeDecoder : Decoder VScreen
resizeDecoder =
    Decode.succeed Vector2d.pixels
        |> DE.andMap (Decode.at [ "detail", "w" ] Decode.float)
        |> DE.andMap (Decode.at [ "detail", "h" ] Decode.float)



-- Event Helpers


noPropagationOn : String -> Decoder msg -> H.Attribute msg
noPropagationOn name decoder =
    decoder
        |> Decode.map (\val -> ( val, True ))
        |> HE.stopPropagationOn name



-- Mouse Wheel Events


type WheelMode
    = Pixels
    | Lines
    | Pages


intToWheelMode : Int -> WheelMode
intToWheelMode val =
    case val of
        1 ->
            Lines

        2 ->
            Pages

        _ ->
            Pixels


wheelModeToScale : WheelMode -> Float
wheelModeToScale mode =
    case mode of
        Pixels ->
            1.0

        Lines ->
            40.0

        Pages ->
            800.0


wheelEventToScale : WheelEvent -> Float
wheelEventToScale event =
    wheelModeToScale event.deltaMode * event.deltaY / 50 |> clamp -1 1


type alias WheelEvent =
    { deltaY : Float
    , deltaMode : WheelMode
    }


wheelDecoder : Decoder WheelEvent
wheelDecoder =
    Decode.succeed WheelEvent
        |> DE.andMap (Decode.field "deltaY" Decode.float)
        |> DE.andMap (Decode.field "deltaMode" Decode.int |> Decode.map intToWheelMode)



-- Mouse Movement Events


mousePosDecoder : Decoder PScreen
mousePosDecoder =
    Decode.succeed Point2d.pixels
        |> DE.andMap (Decode.at [ "clientX" ] Decode.float)
        |> DE.andMap (Decode.at [ "clientY" ] Decode.float)
        |> Decode.map (Debug.log "mousePos")



-- Geometry


rectToXywh : Rectangle2d Unitless coord -> { x : Float, y : Float, w : Float, h : Float }
rectToXywh rect =
    let
        ( w, h ) =
            Rectangle2d.dimensions rect
                |> Tuple.mapBoth Quantity.toFloat Quantity.toFloat

        { x, y } =
            Rectangle2d.interpolate rect 0 0
                |> Point2d.toUnitless
    in
    { x = x
    , y = y
    , w = w
    , h = h
    }


rectToXywhPixels : Rectangle2d Pixels coord -> { x : Float, y : Float, w : Float, h : Float }
rectToXywhPixels rect =
    let
        ( w, h ) =
            Rectangle2d.dimensions rect
                |> Tuple.mapBoth Pixels.toFloat Pixels.toFloat

        { x, y } =
            Rectangle2d.interpolate rect 0 0
                |> Point2d.toPixels
    in
    { x = x
    , y = y
    , w = w
    , h = h
    }
