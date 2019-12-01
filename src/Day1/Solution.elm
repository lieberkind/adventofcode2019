module Day1.Solution exposing (..)

import Day1.Input exposing (input)


divideBy : Int -> Int -> Int
divideBy y x =
    x // y


subtract : Int -> Int -> Int
subtract x y =
    y - x


calculateFuelRequirement : Int -> Int
calculateFuelRequirement mass =
    mass |> divideBy 3 |> subtract 2


recursivelyCalculateFuelRequirement : Int -> Int
recursivelyCalculateFuelRequirement mass =
    let
        rec acc m =
            let
                fuelRequirements =
                    calculateFuelRequirement m
            in
            if fuelRequirements <= 0 then
                acc

            else
                rec (fuelRequirements + acc) fuelRequirements
    in
    rec 0 mass


parsedInput : List Int
parsedInput =
    input
        |> String.lines
        |> List.map String.toInt
        |> List.filterMap identity


solve1 : Int
solve1 =
    parsedInput
        |> List.map calculateFuelRequirement
        |> List.sum


solve2 : Int
solve2 =
    parsedInput
        |> List.map recursivelyCalculateFuelRequirement
        |> List.sum
