module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (Maybe(Just))


-- model


type alias Model =
    { history : List BoardState
    , currentBoard : List String
    , stepNumber : Int
    , xIsNext : Bool
    }


type alias BoardState =
    { squares : List String
    }


initialModel : Model
initialModel =
    { history = []
    , currentBoard = List.repeat 9 "" -- FIX use a different value
    , stepNumber = 0
    , xIsNext = True
    }



-- update


type Msg
    = AddBoardState
    | AddMove Int
    | SwitchPlayer
    | JumpTo


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddMove index ->
            addMove
                model
                index
                |> addBoardState
                |> switchPlayer

        SwitchPlayer ->
            { model
                | xIsNext = not model.xIsNext
            }

        _ ->
            model


addMove : Model -> Int -> Model
addMove model index =
    let
        firstList =
            List.take index model.currentBoard

        secondList =
            List.drop index model.currentBoard

        newSecondList =
            (nextMark model) :: Maybe.withDefault [] (List.tail secondList)

        newCurrentBoard =
            List.append firstList newSecondList
    in
        { model | currentBoard = newCurrentBoard }


addBoardState : Model -> Model
addBoardState model =
    let
        newHistory =
            (BoardState model.currentBoard) :: model.history
    in
        Debug.log "MODEL"
            { model | history = newHistory }


switchPlayer : Model -> Model
switchPlayer model =
    { model | xIsNext = (not model.xIsNext) }



-- view


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "game" ]
            [ board model ]
        , div [ class "game-info" ]
            [ div [ class "next-player" ]
                [ text "Next Player: "
                , text (toString (nextMark model)) -- FIX - rendering as a string
                ]
            ]
        ]


squares =
    [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]


board : Model -> Html Msg
board model =
    List.map2 square model.currentBoard squares
        |> div [ class "board" ]


nextMark : Model -> String
nextMark model =
    if model.xIsNext then
        "X"
    else
        "O"


square :
    String
    -> Int
    -> Html Msg
square mark squareNumber =
    div [ class "square", onClick (AddMove squareNumber) ]
        [ text (toString mark) ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
