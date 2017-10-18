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
    , winner : Bool
    }


type alias BoardState =
    { squares : List String
    }


initialModel : Model
initialModel =
    { history = [ { squares = (List.repeat 9 "") } ]
    , currentBoard = List.repeat 9 ""
    , stepNumber = 0
    , xIsNext = True
    , winner = False
    }



-- update


type Msg
    = AddMove Int
    | GoToState (List String)
    | SwitchPlayer
    | StartNewGame


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddMove index ->
            addMove
                model
                index
                |> addBoardState
                |> checkForWinner
                |> switchPlayer

        GoToState state ->
            { model | currentBoard = state }

        SwitchPlayer ->
            { model
                | xIsNext = not model.xIsNext
            }

        StartNewGame ->
            { model
                | history = [ { squares = (List.repeat 9 "") } ]
                , currentBoard = List.repeat 9 ""
                , stepNumber = 0
                , xIsNext = True
                , winner = False
            }


checkForWinner : Model -> Model
checkForWinner model =
    let
        winner =
            List.indexedMap (,) model.currentBoard
                |> getPlayerSpaces model
                |> checkAllLines
    in
        { model | winner = winner }


getPlayerSpaces model currentBoard =
    Tuple.first
        (List.unzip
            (List.filter
                (\t -> (Tuple.second t) == (nextMark model))
                currentBoard
            )
        )


lines : List (List Int)
lines =
    [ [ 0, 1, 2 ]
    , [ 3, 4, 5 ]
    , [ 6, 7, 8 ]
    , [ 0, 3, 6 ]
    , [ 1, 4, 7 ]
    , [ 2, 5, 8 ]
    , [ 0, 4, 8 ]
    , [ 2, 4, 6 ]
    ]


checkAllLines : List Int -> Bool
checkAllLines playerSpaces =
    let
        checkedLines =
            List.map
                (\line ->
                    if List.length (List.filter (\space -> List.member space playerSpaces) line) == 3 then
                        True
                    else
                        False
                )
                lines
    in
        if List.member True checkedLines then
            True
        else
            False


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


filledSpaces : List String -> List String
filledSpaces currentBoard =
    List.filter (\space -> space /= "") currentBoard


addBoardState : Model -> Model
addBoardState model =
    let
        history =
            List.take (List.length (filledSpaces model.currentBoard)) model.history

        newHistory =
            (BoardState model.currentBoard)
                :: history
    in
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
                , text (nextMark model)
                ]
            , div [ class "winner" ]
                [ div []
                    [ text (winnerText model.winner) ]
                , div []
                    [ text (winnerMark model.winner (winningMark model)) ]
                , div []
                    [ newGame model.winner ]
                ]
            ]
        , div [ class "game-info" ]
            [ div [ class "moves" ]
                [ text "Moves" ]
            , div [ class "history" ]
                [ moveHistory model ]
            ]
        ]


winnerText : Bool -> String
winnerText winner =
    if winner then
        "WINNER!"
    else
        ""


winningMark : Model -> String
winningMark model =
    if model.xIsNext then
        "O"
    else
        "X"


winnerMark : Bool -> String -> String
winnerMark winner winningMark =
    if winner then
        winningMark
    else
        ""


newGame : Bool -> Html Msg
newGame winner =
    if winner then
        button [ class "new-game", onClick StartNewGame ]
            [ text "New Game" ]
    else
        div [] []


moveHistory : Model -> Html Msg
moveHistory model =
    List.map2 stateButtons (List.reverse model.history) squares
        |> div [ class "state-buttons" ]


moveNumber : Int -> String
moveNumber number =
    if number == 0 then
        "game start"
    else
        toString number


stateButtons : BoardState -> Int -> Html Msg
stateButtons boardState number =
    button [ onClick (GoToState boardState.squares) ]
        [ text ("Go to " ++ (moveNumber number)) ]


squares : List Int
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


square : String -> Int -> Html Msg
square mark squareNumber =
    div [ class "square", onClick (AddMove squareNumber) ]
        [ text mark ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
