module Main exposing (Model, Msg(..), init, initialModel, main, subscriptions, update, view)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as CheckBox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Browser
import Html exposing (Html, div, h1, label, p, text)
import Html.Attributes exposing (class, for, name)


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
    | ChangeSearch String
    | SearchForSetUp
    | SelectMemberChanged String


type ViewMode
    = Home
    | Create
    | Preferences
    | Compose


type alias Model =
    { host : String
    , viewMode : ViewMode
    , currentMember : String
    , currentSearch : String
    , members : List String
    }


init : String -> ( Model, Cmd Msg )
init locationHref =
    ( initialModel locationHref, Cmd.none )


initialModel : String -> Model
initialModel locationHref =
    { host = locationHref
    , viewMode = Home
    , currentMember = ""
    , currentSearch = ""
    , members = []
    }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ViewHome ->
            ( { model | viewMode = Home }, Cmd.none )

        ViewCreate ->
            ( { model | viewMode = Create }, Cmd.none )

        ViewPreferences ->
            let
                selected =
                    Maybe.withDefault "" <| List.head model.members
            in
            ( { model | viewMode = Preferences, currentMember = selected }, Cmd.none )

        ViewCompose ->
            ( { model | viewMode = Compose }, Cmd.none )

        ChangeMember name ->
            ( { model | currentMember = name }, Cmd.none )

        AddMember ->
            if (List.filter (\m -> m == model.currentMember) model.members |> List.length) > 0 then
                ( model, Cmd.none )

            else
                ( { model | members = model.currentMember :: model.members, currentMember = "" }, Cmd.none )

        ChangeSearch search ->
            ( { model | currentSearch = search }, Cmd.none )

        SearchForSetUp ->
            ( model, Cmd.none )

        SelectMemberChanged member ->
            ( { model | currentMember = member }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    Grid.container []
        [ h1 [] [ text "Team Composer" ]
        , p [] [ text "Compose teams from a group of people, based on their preferences about who they wish to work with." ]
        , viewMenuBar
        , viewBody model
        ]


viewMenuBar : Html Msg
viewMenuBar =
    ButtonGroup.buttonGroup []
        [ topButton ViewHome "Home"
        , topButton ViewCreate "Create"
        , topButton ViewPreferences "Preferences"
        , topButton ViewCompose "Compose"
        ]


topButton msg label =
    ButtonGroup.button [ Button.outlinePrimary, Button.onClick msg ] [ text label ]


viewBody : Model -> Html Msg
viewBody model =
    case model.viewMode of
        Home ->
            viewHomePage model

        Create ->
            viewCreatePage model

        Preferences ->
            viewPreferences model

        Compose ->
            viewComposePage model


viewHomePage : Model -> Html Msg
viewHomePage model =
    Card.config []
        |> Card.block []
            [ Block.titleH4 [] [ text "Load a set up" ]
            , Block.text [] [ p [] [ text "Load a previously entered set up." ] ]
            , Block.custom <| viewFormToFindSetUp model
            ]
        |> Card.view


viewFormToFindSetUp : Model -> Html Msg
viewFormToFindSetUp model =
    Form.form []
        [ Form.label [ for "search" ] [ text "Search" ]
        , Input.text [ Input.id "search", Input.value model.currentSearch ]
        , Button.button [ Button.primary, Button.onClick SearchForSetUp ] [ text "Search" ]
        ]


viewCreatePage : Model -> Html Msg
viewCreatePage model =
    Card.config []
        |> Card.block []
            [ Block.titleH4 [] [ text "Create the set up" ]
            , Block.text [] [ text "Enter the members of this set up. No duplicate names." ]
            , Block.custom <| viewFormToAddMember model
            , Block.text [] [ text "Current members:" ]
            , Block.custom <| viewMembers model.members
            ]
        |> Card.view


viewFormToAddMember : Model -> Html Msg
viewFormToAddMember model =
    Form.form []
        [ Form.group []
            [ Form.label [ for "name" ] [ text "Enter member name:" ]
            , Input.text [ Input.id "name", Input.onInput ChangeMember, Input.value model.currentMember ]
            , Form.help [] [ text "The name of a member" ]
            ]
        , Button.button [ Button.primary, Button.onClick AddMember ] [ text "Add member" ]
        ]


viewMembers : List String -> Html Msg
viewMembers members =
    div [ class "col" ] (List.map (\m -> div [] [ text m ]) members)


viewPreferences : Model -> Html Msg
viewPreferences model =
    Card.config []
        |> Card.block []
            [ Block.titleH4 [] [ text "Set preferences" ]
            , Block.text [] [ text "Select a member and mark with whom they can work with" ]
            , Block.custom <|
                viewFormToSetPreferences model
            ]
        |> Card.view


viewFormToSetPreferences : Model -> Html Msg
viewFormToSetPreferences model =
    let
        memberList =
            List.filter (\m -> m /= model.currentMember) model.members
    in
    Form.form []
        [ Form.group []
            [ Form.label [ for "select" ] [ text "Select a member" ]
            , Select.select [ Select.id "select", Select.onChange SelectMemberChanged ]
                (List.map (\m -> Select.item [] [ text m ]) model.members)
            , text "Can work with:"
            , div [ class "col" ]
                (List.map checkbox memberList)
            ]
        ]


checkbox : String -> Html Msg
checkbox m =
    CheckBox.checkbox [ CheckBox.id m ] m


viewComposePage : Model -> Html Msg
viewComposePage _ =
    Card.config []
        |> Card.block []
            [ Block.titleH4 [] [ text "Compose" ]
            , Block.custom <| p [] [ text "The teams are composed given the preferences, if possible." ]
            ]
        |> Card.view



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
