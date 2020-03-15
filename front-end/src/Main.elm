module Main exposing (Model, Msg(..), init, initialModel, main, subscriptions, update, view)

import Browser
import Html exposing (Html, div, h1, h2, input, option, p, select, text)
import Html.Attributes exposing (class, style, type_, value)
import Html.Events exposing (onClick, onInput)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = ViewHome
    | ViewCreate
    | ViewPreferences
    | ViewCompose
    | ChangeMember String
    | AddMember


type ViewMode
    = Home
    | Create
    | Preferences
    | Compose


type alias Model =
    { viewMode : ViewMode
    , currentMember : String
    , members : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { viewMode = Home, currentMember = "", members = [] }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ViewHome ->
            ( { model | viewMode = Home }, Cmd.none )

        ViewCreate ->
            ( { model | viewMode = Create }, Cmd.none )

        ViewPreferences ->
            ( { model | viewMode = Preferences }, Cmd.none )

        ViewCompose ->
            ( { model | viewMode = Compose }, Cmd.none )

        ChangeMember name ->
            ( { model | currentMember = name }, Cmd.none )

        AddMember ->
            ( { model | members = model.currentMember :: model.members, currentMember = "" }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Team Composer" ]
        , p [] [ text "Compose teams from a group of people, based on their preferences about who they wish to work with." ]
        , viewMenuBar
        , viewBody model
        ]


viewMenuBar : Html Msg
viewMenuBar =
    div [ class "row", style "max-width" "fit-content" ]
        [ input [ type_ "button", value "Home", onClick ViewHome ] []
        , input [ type_ "button", value "Create", onClick ViewCreate ] []
        , input [ type_ "button", value "Preferences", onClick ViewPreferences ] []
        , input [ type_ "button", value "Compose", onClick ViewCompose ] []
        ]


viewBody : Model -> Html Msg
viewBody model =
    case model.viewMode of
        Home ->
            div [] []

        Create ->
            div []
                [ h2 [] [ text "Create the set up" ]
                , div []
                    [ text "Enter member name:"
                    , input [ value model.currentMember, onInput ChangeMember ] []
                    , input [ type_ "button", value "+", onClick AddMember ] []
                    ]
                , viewMembers model.members
                ]

        Preferences ->
            div []
                [ h2 [] [ text "Set preferences" ]
                , viewPreferences model
                ]

        Compose ->
            div []
                [ h2 [] [ text "Compose" ]
                , p [] [ text "The teams are composed given the preferences, if possible." ]
                ]


viewMembers : List String -> Html Msg
viewMembers members =
    div [ class "col" ] (List.map (\m -> div [] [ text m ]) members)


viewPreferences : Model -> Html Msg
viewPreferences model =
    div []
        [ select []
            (List.map (\m -> option [] [ text m ]) model.members)
        , div [ class "col" ]
            (List.map checkbox model.members)
        ]


checkbox m =
    input [ type_ "checkbox", value m ] [ text m ]



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
