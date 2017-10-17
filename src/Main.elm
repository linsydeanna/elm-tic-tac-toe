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
    { history = []
    , currentBoard = List.repeat 9 ""
    , stepNumber = 0
    , xIsNext = True
    , winner = False
    }



-- update


type Msg
    = AddBoardState
    | AddMove Int
    | SwitchPlayer
    | CheckWinner (List String)
    | JumpTo


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

        SwitchPlayer ->
            { model
                | xIsNext = not model.xIsNext
            }

        _ ->
            model


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
                , text (nextMark model)
                ]
            , div [ class "next-player" ]
                [ text "WINNER: "
                , text (toString model.winner)
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
        [ text mark ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
