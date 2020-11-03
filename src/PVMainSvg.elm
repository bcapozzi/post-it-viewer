module PVMainSvg exposing (..)

import Browser exposing (..)
import Csv exposing (..)
import Html exposing (..)
import Html.Attributes exposing (checked, class, id, placeholder, src, style, title, type_, value, width)
import Html.Events exposing (..)
import Json.Decode as JD
import Ports exposing (CSVPortData, fileContentRead, fileSelected)
import Random
import Set exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = CSVSelected
    | CSVParse CSVPortData
    | RenderNotes
    | ColorIndexUpdate (List Int)
    | ZoomIn
    | ZoomOut
    | PanLeft
    | PanRight


type alias CSVFile =
    { contents : String
    , filename : String
    }


type alias Note =
    { author : String
    , recipient : String
    , message : String
    }


type alias Model =
    { csvFile : Maybe CSVFile
    , csvData : Csv
    , notes : List Note
    , colorIndices : List Int
    , viewWidth : Int
    , viewOrigin : ( Int, Int )
    }


type alias Role =
    { year : String
    , title : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing (Csv [] [ [] ]) [] [] 2000 (Tuple.pair 0 0), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead CSVParse


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CSVSelected ->
            ( model, fileSelected "CSVInput" )

        CSVParse data ->
            let
                newCSVFile =
                    { contents = data.contents
                    , filename = data.filename
                    }
            in
            case Csv.parse newCSVFile.contents of
                results ->
                    ( { model | csvFile = Just newCSVFile, csvData = results }, Cmd.none )

        RenderNotes ->
            ( { model | notes = parseCsvData model.csvData }, Random.generate ColorIndexUpdate (colorIndices (List.length model.csvData.records)) )

        ColorIndexUpdate indices ->
            ( { model | colorIndices = indices }, Cmd.none )

        ZoomIn ->
            ( { model | viewWidth = model.viewWidth - 500 }, Cmd.none )

        ZoomOut ->
            ( { model | viewWidth = model.viewWidth + 500 }, Cmd.none )

        PanRight ->
            ( { model | viewOrigin = panRight model.viewOrigin 100 }, Cmd.none )

        PanLeft ->
            ( { model | viewOrigin = panLeft model.viewOrigin 100 }, Cmd.none )



--colorIndices : Int -> Random.Generator (List Int)


panRight viewOrigin step =
    let
        prevX =
            Tuple.first viewOrigin

        prevY =
            Tuple.second viewOrigin
    in
    Tuple.pair (prevX + step) prevY


panLeft viewOrigin step =
    let
        prevX =
            Tuple.first viewOrigin

        prevY =
            Tuple.second viewOrigin
    in
    Tuple.pair (Basics.max 0 (prevX - step)) prevY


colorIndices n =
    Random.list n (Random.uniform 1 [ 2, 3, 4 ])



--(Random.int 1 4)


parseCsvData : Csv -> List Note
parseCsvData csvData =
    List.map (\r -> toNote r) csvData.records


toNote : List String -> Note
toNote fields =
    let
        pairs =
            List.indexedMap Tuple.pair fields
    in
    extractFields pairs (Note "" "" "")


extractFields pairs noteSoFar =
    case List.head pairs of
        Just aTuple ->
            case Tuple.first aTuple of
                0 ->
                    extractFields (List.drop 1 pairs) (Note (Tuple.second aTuple) noteSoFar.recipient noteSoFar.message)

                1 ->
                    extractFields (List.drop 1 pairs) (Note noteSoFar.author (Tuple.second aTuple) noteSoFar.message)

                2 ->
                    extractFields (List.drop 1 pairs) (Note noteSoFar.author noteSoFar.recipient (Tuple.second aTuple))

                _ ->
                    extractFields (List.drop 1 pairs) noteSoFar

        Nothing ->
            noteSoFar


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ Html.text "Post It Generator" ]
        , div [ Html.Attributes.class "FileWrapper" ]
            [ input
                [ Html.Attributes.type_ "file"
                , Html.Attributes.id "CSVInput"
                , on "change"
                    (JD.succeed CSVSelected)
                ]
                []
            ]
        , csvView model.csvFile model.csvData
        , br [] []
        , button [ onClick RenderNotes ] [ Html.text "Render Post-Its" ]
        , renderPostIts model.notes model.colorIndices
        , br [] []
        , div
            [ Html.Attributes.style "border" "3px solid green"
            , Html.Attributes.style "width" "1200px"
            , Html.Attributes.style "height" "800px"
            , Html.Attributes.style "overflow" "scroll"
            ]
            [ svg
                [ Svg.Attributes.width "100%"
                , Svg.Attributes.height "100%"
                , viewBox (String.fromInt (Tuple.first model.viewOrigin) ++ " " ++ String.fromInt (Tuple.second model.viewOrigin) ++ " " ++ String.fromInt model.viewWidth ++ " " ++ String.fromInt model.viewWidth)
                , Svg.Attributes.class "pshadow"

                --  , Svg.Attributes.overflow "auto"
                ]
                (renderPostIts2 model.notes model.colorIndices)
            ]
        , button [ onClick ZoomIn ] [ Html.text "Zoom In" ]
        , button [ onClick ZoomOut ] [ Html.text "Zoom Out" ]
        , button [ onClick PanLeft ] [ Html.text "Pan Left" ]
        , button [ onClick PanRight ] [ Html.text "Pan Right" ]

        -- [ rect
        --     [ x "10"
        --     , y "10"
        --     , Svg.Attributes.width "100"
        --     , Svg.Attributes.height "100"
        --     , rx "5"
        --     , ry "5"
        --     , fill "#ffff99"
        --     , Svg.Attributes.class "pshadow"
        --     ]
        --     []
        -- , circle
        --     [ cx "200"
        --     , cy "100"
        --     , r "50"
        --     , fill "red"
        --     ]
        --     []
        -- ]
        ]


renderPostIts2 notes indices =
    let
        pairs =
            List.map2 Tuple.pair notes indices

        xy =
            List.reverse (generateXY [] pairs)
    in
    renderPostIts3 [] pairs xy



--    List.map (\p -> renderPostIt2 p) pairs


renderPostIts3 renderedSoFar pairs xy =
    case pairs of
        firstPair :: restOfPairs ->
            let
                note =
                    Tuple.first firstPair

                colorIndex =
                    Tuple.second firstPair

                nextPostIt =
                    renderPostIt3 note colorIndex (List.head xy)
            in
            renderPostIts3 (List.append nextPostIt renderedSoFar) (List.drop 1 pairs) (List.drop 1 xy)

        [] ->
            renderedSoFar


renderPostIt3 note colorIndex xy =
    case xy of
        Just aPair ->
            [ rect
                [ x (String.fromInt (Tuple.first aPair))
                , y (String.fromInt (Tuple.second aPair))
                , Svg.Attributes.width "200"
                , Svg.Attributes.height "200"
                , rx "5"
                , ry "5"
                , fill (getColor colorIndex)
                , Svg.Attributes.class "pshadow"
                ]
                []
            , Svg.foreignObject
                [ x (String.fromInt (Tuple.first aPair + 10))
                , y (String.fromInt (Tuple.second aPair + 10))
                , Svg.Attributes.width "190"
                , Svg.Attributes.height "190"
                , Svg.Attributes.fontSize "20px"
                , Svg.Attributes.fontFamily "Permanent Marker"
                ]
                [ Html.text note.message ]
            ]

        Nothing ->
            [ rect
                [ x "0"
                , y "0"
                , Svg.Attributes.width "200"
                , Svg.Attributes.height "200"
                , rx "5"
                , ry "5"
                , fill (getColor colorIndex)
                , Svg.Attributes.class "pshadow"
                ]
                []
            , Svg.text_
                [ x "0"
                , y "0"
                , Svg.Attributes.fontSize "12"
                , Svg.Attributes.fontFamily "Permanent Marker"
                ]
                [ Svg.text "NONE" ]
            ]


generateXY xySoFar pairs =
    case pairs of
        first :: rest ->
            case xySoFar of
                [] ->
                    generateXY (Tuple.pair 0 0 :: xySoFar) (List.drop 1 pairs)

                firstXY :: restXY ->
                    let
                        xprev =
                            Tuple.first firstXY

                        yprev =
                            Tuple.second firstXY

                        ( xnext, ynext ) =
                            getNextXY firstXY 2000 2000 210 210
                    in
                    generateXY (Tuple.pair xnext ynext :: xySoFar) (List.drop 1 pairs)

        [] ->
            xySoFar


getNextXY prevPair widthX widthY stepX stepY =
    let
        xprev =
            Tuple.first prevPair

        yprev =
            Tuple.second prevPair

        xnext =
            xprev + stepX
    in
    if (xnext + stepX) > widthX then
        Tuple.pair 0 (yprev + stepY)

    else
        Tuple.pair xnext yprev


renderPostIt2 pair =
    let
        note =
            Tuple.first pair

        colorIndex =
            Tuple.second pair
    in
    rect
        [ x "10"
        , y "10"
        , Svg.Attributes.width "200"
        , Svg.Attributes.height "200"
        , rx "5"
        , ry "5"
        , fill (getColor colorIndex)
        , Svg.Attributes.class "pshadow"
        ]
        []


renderPostIts notes indices =
    let
        pairs =
            List.map2 Tuple.pair notes indices
    in
    div [] (List.map (\p -> renderPostIt p) pairs)


renderPostIt pair =
    let
        note =
            Tuple.first pair

        colorIndex =
            Tuple.second pair
    in
    div [ Html.Attributes.class "postIt", Html.Attributes.style "background-color" (getColor colorIndex) ]
        [ h2 [] [ Html.text ("TO: " ++ note.recipient) ]
        , h2 [] [ Html.text ("FROM: " ++ note.author) ]
        , h2 [] [ Html.text note.message ]
        ]


getColor : Int -> String
getColor index =
    case index of
        1 ->
            "#ffff99"

        2 ->
            "#7afcff"

        3 ->
            "#ff7eb9"

        4 ->
            "#35BEB7"

        _ ->
            "#333333"


csvView : Maybe CSVFile -> Csv -> Html Msg
csvView file csvData =
    case file of
        Just i ->
            csvTable csvData

        Nothing ->
            "NOTHING TO SHOW" |> Html.text


csvTable : Csv -> Html Msg
csvTable data =
    div []
        [ table []
            (tr []
                (List.map (\h -> th [] [ Html.text h ]) data.headers)
                :: List.map (\r -> tr [] (List.map (\c -> td [] [ Html.text c ]) r)) data.records
            )
        ]
