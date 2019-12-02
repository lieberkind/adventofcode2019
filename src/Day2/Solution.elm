module Day2.Solution exposing (solve1, solve2)

import Array exposing (Array)
import Day2.Input exposing (input)


initialize : Int -> Int -> String -> Program
initialize noun verb str =
    str
        |> String.split ","
        |> List.map String.toInt
        |> List.filterMap identity
        |> Array.fromList
        |> Array.set 1 noun
        |> Array.set 2 verb
        |> State 0
        |> Running


type alias State =
    { pointer : Int
    , instrs : Array Int
    }


type Program
    = Running State
    | Terminated Int
    | Errored String


read : Int -> Array Int -> Maybe Int
read pointer instrs =
    let
        address =
            Array.get pointer instrs
    in
    Maybe.andThen (\addr -> Array.get addr instrs) address


step : State -> Program
step state =
    case Array.get state.pointer state.instrs of
        Just 1 ->
            Maybe.map3
                (\v1 v2 outAddr ->
                    Running
                        { instrs = Array.set outAddr (v1 + v2) state.instrs
                        , pointer = state.pointer + 4
                        }
                )
                (read (state.pointer + 1) state.instrs)
                (read (state.pointer + 2) state.instrs)
                (Array.get (state.pointer + 3) state.instrs)
                |> Maybe.withDefault (Errored "Couldn't add for some reason")

        Just 2 ->
            Maybe.map3
                (\v1 v2 outAddr ->
                    Running
                        { instrs = Array.set outAddr (v1 * v2) state.instrs
                        , pointer = state.pointer + 4
                        }
                )
                (read (state.pointer + 1) state.instrs)
                (read (state.pointer + 2) state.instrs)
                (Array.get (state.pointer + 3) state.instrs)
                |> Maybe.withDefault (Errored "Couldn't multiply for some reason")

        Just 99 ->
            case Array.get 0 state.instrs of
                Just val ->
                    Terminated val

                Nothing ->
                    Errored "Couldn't read termination value"

        _ ->
            Errored "Unknow instruction"


run : Program -> Result String Int
run program =
    case program of
        Errored str ->
            Err str

        Terminated val ->
            Ok val

        Running state ->
            step state |> run


solve1 =
    input
        |> initialize 12 2
        |> run


solve2 : Int -> Int -> Int
solve2 noun verb =
    let
        res =
            input |> initialize noun verb |> run
    in
    if res == Ok 19690720 then
        100 * noun + verb

    else
        case ( noun, verb ) of
            ( 99, 99 ) ->
                0

            ( n, v ) ->
                if v < 99 then
                    solve2 noun (verb + 1)

                else
                    solve2 (noun + 1) 0
