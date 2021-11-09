module ValvoMinimaxAI

open System.Collections.Generic
open Valvo

let reachableStates board initialState =
    let states = new HashSet<GameState>()
    states.Add(initialState) |> ignore
    let newStates = new List<GameState>(3)
    newStates.Add(initialState)
    while newStates.Count > 0 do
        let ns = newStates |> Seq.collect (nextStates board) |> Seq.concat |> Seq.toArray
        newStates.Clear()
        for st in ns do
            if states.Add(st) then
                newStates.Add(st)
    states

let minimaxScore board state =
    match isEndState board state with
    | Player1Wins, points1, points2 
    | Player2Wins, points1, points2 
    | Draw,        points1, points2 -> float (points1 - points2)
    | NotEnded,    _,       _       -> failwith "State has to be an end state"

let minimaxWinProbability board state =
    match isEndState board state with
    | Player1Wins, _, _ -> 1.0
    | Player2Wins, _, _ -> -1.0
    | Draw,        _, _ -> 0.0
    | NotEnded,    _, _ -> failwith "State has to be an end state"

type private Variable =
    | Constant of float
    | Var of int
    | Max3 of int * int * int
    | Min3 of int * int * int
    | Max2 of int * int
    | Min2 of int * int
    | Average of int * int

let private solve (equationSystem : Variable []) maxIterations =
    let solveStep (solution : float []) (result : float []) =
        for i = 0 to equationSystem.Length - 1 do
            result[i] <- 
                match equationSystem[i] with
                | Constant x     -> x
                | Var x          -> solution[x]
                | Max3 (x, y, z) -> (max solution[x] solution[y] + max solution[y] solution[z]) / 2.0
                | Min3 (x, y, z) -> (min solution[x] solution[y] + min solution[y] solution[z]) / 2.0
                | Max2 (x, y)    -> max solution[x] solution[y]
                | Min2 (x, y)    -> min solution[x] solution[y]
                | Average (x, y) -> (solution[x] + solution[y]) / 2.0
    let rec solveInner (solution1 : float []) (solution2 : float []) it =
        solveStep solution1 solution2
        if it = maxIterations || solution1 = solution2 then
            solution2
        else
            solveInner solution2 solution1 (it + 1)    
    let solution1 = Array.zeroCreate equationSystem.Length
    let solution2 = Array.zeroCreate equationSystem.Length
    solveInner solution1 solution2 0

let computeMinimaxScores scoreFunction board startingState maxIterations =
    let possibleStates = reachableStates board startingState |> Seq.toArray
    let states = possibleStates |> Seq.mapi (fun i st -> st, i) |> dict
    let stateToIndex state = states[state]
    let variable state = 
        match nextStates board state, state.Turn with
        | [[]],                 _                    -> Constant (scoreFunction board state)
        | [[s]],                _                    -> Var (stateToIndex s)
        | [[s1; s2]],           Player1              -> Max2 (stateToIndex s1, stateToIndex s2)
        | [[s1; s2]],           Player2              -> Min2 (stateToIndex s1, stateToIndex s2)
        | [[s1; s2]; [s3; s4]], Player1 when s1 = s3 -> Max3 (stateToIndex s2, stateToIndex s1, stateToIndex s4)
        | [[s1; s2]; [s3; s4]], Player2 when s1 = s3 -> Min3 (stateToIndex s2, stateToIndex s1, stateToIndex s4)
        | _,                    _                    -> failwith "Unexpected return value from nextStates"
    let equationSystem = possibleStates |> Array.map variable
    let scores = solve equationSystem maxIterations
    Seq.zip possibleStates scores |> dict

let computeExpectedValues scoreFunction board startingState mover1 mover2 maxIterations =
    let mover state possibleStates =
        match state.Turn with
        | Player1 -> mover1 state possibleStates
        | Player2 -> mover2 state possibleStates
    let possibleStates = reachableStates board startingState |> Seq.toArray
    let states = possibleStates |> Seq.mapi (fun i st -> st, i) |> dict
    let stateToIndex state = states[state]
    let variable state = 
        match nextStates board state with
        | [[]]             -> Constant (scoreFunction board state)
        | [group]          -> Var (stateToIndex (mover state group))
        | [group1; group2] -> Average (stateToIndex (mover state group1), stateToIndex (mover state group2))
        | _                -> failwith "Unexpected return value from nextStates"
    let equationSystem = possibleStates |> Array.map variable
    let scores = solve equationSystem maxIterations
    Seq.zip possibleStates scores |> dict

let computeExpectedScores board startingState mover1 mover2 maxIterations =
    let score player board state =
        let result, points1, points2 = isEndState board state
        match player with
        | Player1 -> float points1
        | Player2 -> float points2
    let p1Scores = computeExpectedValues (score Player1) board startingState mover1 mover2 maxIterations
    let p2Scores = computeExpectedValues (score Player2) board startingState mover1 mover2 maxIterations
    p1Scores.Keys
    |> Seq.map (fun state -> state, (p1Scores[state], p2Scores[state]))
    |> dict

let computeExpectedWinProbabilities board startingState mover1 mover2 maxIterations =
    let winProbability player board state =
        match isEndState board state with
        | Player1Wins, _, _ -> if player = Player1 then 1.0 else 0.0
        | Player2Wins, _, _ -> if player = Player2 then 1.0 else 0.0
        | Draw,        _, _ -> 0.0
        | NotEnded,    _, _ -> failwith "State has to be an end state"
    let p1WinProbabilities = computeExpectedValues (winProbability Player1) board startingState mover1 mover2 maxIterations
    let p2WinProbabilities = computeExpectedValues (winProbability Player2) board startingState mover1 mover2 maxIterations
    p1WinProbabilities.Keys
    |> Seq.map (fun state -> state, (p1WinProbabilities[state], p2WinProbabilities[state]))
    |> dict

let minimaxMover (scores : IDictionary<GameState,float>) state possibleStates =
    possibleStates |> (if state.Turn = Player1 then List.maxBy else List.minBy) (fun st -> scores[st])
