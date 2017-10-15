module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { history : List BoardState
    , currentBoard : List String
    , stepNumber : Int
    , xIsNext : Bool
    }


type alias BoardState =
    { squares : List String
    }



-- model


initialModel : Model
initialModel =
    { history = []
    , currentBoard = []
    , stepNumber = 0
    , xIsNext = True
    }



-- update


type Msg
    = AddBoardState
    | AddMove String
    | SwitchPlayer
    | JumpTo


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddMove move ->
            addMove
                model
                move
                |> addBoardState
                |> switchPlayer

        SwitchPlayer ->
            { model
                | xIsNext = not model.xIsNext
            }

        _ ->
            model


addMove : Model -> String -> Model
addMove model move =
    let
        newCurrentBoard =
            move :: model.currentBoard
    in
        Debug.log "MODEL"
            { model | currentBoard = newCurrentBoard }


addBoardState : Model -> Model
addBoardState model =
    let
        newHistory =
            (BoardState model.currentBoard) :: model.history
    in
        { model | history = newHistory }


switchPlayer : Model -> Model
switchPlayer model =
    { model | xIsNext = (not model.xIsNext) }



-- view


mark : Model -> String
mark model =
    if model.xIsNext then
        "X"
    else
        "O"


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "game" ]
            [ board model ]
        , div [ class "game-info" ]
            [ div [ class "next-player" ]
                [ text "Next Player: "
                , text (toString (mark model)) -- FIX - rendering as a string
                ]
            ]
        ]


board : Model -> Html Msg
board model =
    div [ class "board" ]
        [ div [ class "board-row" ]
            [ div [ class "square", onClick (AddMove (mark model)) ]
                [ text "SQUARE" ]
            , div [ class "square", onClick (AddMove (mark model)) ]
                [ text "SQUARE" ]
            , div [ class "square", onClick (AddMove (mark model)) ]
                [ text "SQUARE" ]
            ]
        , div [ class "board-row" ]
            [ div [ class "square" ]
                [ text "SQUARE" ]
            , div [ class "square" ]
                [ text "SQUARE" ]
            , div [ class "square" ]
                [ text "SQUARE" ]
            ]
        , div [ class "board-row" ]
            [ div [ class "square" ]
                [ text "SQUARE" ]
            , div [ class "square" ]
                [ text "SQUARE" ]
            , div [ class "square" ]
                [ text "SQUARE" ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
