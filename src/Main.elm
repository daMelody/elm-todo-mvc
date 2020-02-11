module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { entries : List Entry
    , field : String
    , uid : Int
    , visibility : String
    }


type alias Entry =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }


emptyModel : Model
emptyModel =
    { entries = []
    , visibility = "All"
    , field = ""
    , uid = 0
    }


newEntry : String -> Int -> Entry
newEntry desc id =
    { description = desc
    , completed = False
    , editing = False
    , id = id
    }



-- INIT


init : Model
init =
    emptyModel



-- UPDATE


type Msg
    = UpdateField String
    | EditingEntry Int Bool
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            { model
                | uid = model.uid + 1
                , field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries

                    else
                        model.entries ++ [ newEntry model.field model.uid ]
            }

        UpdateField str ->
            { model | field = str }

        EditingEntry id isEditing ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | editing = isEditing }

                    else
                        t
            in
            { model | entries = List.map updateEntry model.entries }

        UpdateEntry id task ->
            let
                updateEntry t =
                    if t.id == id then
                        -- ?: check what task does here
                        { t | description = task }

                    else
                        t
            in
            { model | entries = List.map updateEntry model.entries }

        Delete id ->
            -- ?: i think this is a lambda function ??
            { model | entries = List.filter (\t -> t.id /= id) model.entries }

        DeleteComplete ->
            -- ?: check what << does here
            { model | entries = List.filter (not << .completed) model.entries }

        Check id isCompleted ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | completed = isCompleted }

                    else
                        t
            in
            { model | entries = List.map updateEntry model.entries }

        CheckAll isCompleted ->
            let
                updateEntry t =
                    { t | completed = isCompleted }
            in
            { model | entries = List.map updateEntry model.entries }

        ChangeVisibility visibility ->
            { model | visibility = visibility }



-- VIEW


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , div
            [ class "todomvc-wrapper" ]
            [ section
                [ class "todoapp" ]
                [ viewInput model.field
                , viewEntries model.visibility model.entries
                , viewControls model.visibility model.entries
                ]
            , infoFooter
            ]
        ]



-- INPUT section


viewInput : String -> Html Msg
viewInput task =
    header [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput UpdateField
            ]
            []
        , button [ onClick Add ] [ text "Add To List" ]
        ]



-- LISTVIEW section (view all)


viewEntries : String -> List Entry -> Html Msg
viewEntries visibility entries =
    let
        isVisible todo =
            case visibility of
                "Completed" ->
                    todo.completed

                "Active" ->
                    not todo.completed

                _ ->
                    True

        allCompleted =
            List.all .completed entries

        cssVisibility =
            if List.isEmpty entries then
                "hidden"

            else
                "visible"
    in
    section
        [ class "main"
        , style "visibility" cssVisibility
        ]
        [ input
            [ class "toggle-all"
            , type_ "checkbox"
            , name "toggle"
            , checked allCompleted
            , onClick (CheckAll (not allCompleted))
            ]
            []
        , label
            [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        , Keyed.ul [ class "todo-list" ] <|
            List.map viewKeyedEntry (List.filter isVisible entries)
        ]



-- VIEW individual entries


viewKeyedEntry : Entry -> ( String, Html Msg )
viewKeyedEntry todo =
    ( String.fromInt todo.id, viewEntry todo )


viewEntry : Entry -> Html Msg
viewEntry todo =
    li
        [ classList [ ( "completed", todo.completed ), ( "editing", todo.editing ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onClick (Check todo.id (not todo.completed))
                ]
                []
            , label [ onDoubleClick (EditingEntry todo.id True) ] [ text todo.description ]
            , button [ class "destroy", onClick (Delete todo.id) ] [ text "X" ]
            ]
        , input
            [ class "edit"
            , value todo.description
            , name "title"
            , id ("todo-" ++ String.fromInt todo.id)
            , onInput (UpdateEntry todo.id)
            , onBlur (EditingEntry todo.id False)
            ]
            []
        ]



-- CONTROLS section


viewControls : String -> List Entry -> Html Msg
viewControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter .completed entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
    footer [ class "footer", hidden (List.isEmpty entries) ]
        [ viewControlsCount entriesLeft, viewControlsFilters visibility, viewControlsClear entriesCompleted ]


viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
    let
        nCount =
            if entriesLeft == 1 then
                " item"

            else
                " items"
    in
    span [ class "todo-count" ] [ strong [] [ text (String.fromInt entriesLeft) ], text (nCount ++ " left") ]


viewControlsFilters : String -> Html Msg
viewControlsFilters visibility =
    ul [ class "filters" ] [ visibilitySwap "#/" "All" visibility, text " ", visibilitySwap "#/active" "Active" visibility, text " ", visibilitySwap "#/completed" "Completed" visibility ]


visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
    li [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ] [ text visibility ] ]


viewControlsClear : Int -> Html Msg
viewControlsClear entriesCompleted =
    button [ class "clear-completed", hidden (entriesCompleted == 0), onClick DeleteComplete ]
        [ text ("Clear completed (" ++ String.fromInt entriesCompleted ++ ")")
        ]


infoFooter : Html Msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Heavily inspired by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        ]
