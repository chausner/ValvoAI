open System
open System.IO
open System.Linq
open System.Threading
open Valvo
open Utils
open ValvoMinimaxAI
open ValvoProcessReader
open FSharp.Collections.ParallelSeq

let randomMover state possibleStates =
    possibleStates |> List.chooseRandomly

let greedyMover (state : GameState) (possibleStates : GameState list) =
    match state.Turn with
    | Player1 -> possibleStates |> Seq.sortByDescending (fun st -> st.Player1Position) |> Seq.head
    | Player2 -> possibleStates |> Seq.sortBy (fun st -> st.Player2Position) |> Seq.head

// simulate a random board game
let simulateRandomBoardGame() = 
    let board = randomBoard 8 7
    let initialState = initState board Player1
    let scores = computeMinimaxScores minimaxScore board initialState 500
    let mover1 = minimaxMover scores
    let mover2 = minimaxMover scores
    let callback state = 
        drawState board state |> showBitmap
        Console.ReadKey() |> ignore
    simulateGame board initialState mover1 mover2 100000 callback |> ignore

// continuously read board state from running Valvo instance and display win probabilities and suggested next mvoe
let trainer() = 
    while true do
        let board, state, largeMove = readBoardAndState()
        if state.Turn = Player1 then
            let scores = computeMinimaxScores minimaxScore board state 1000
            let mover1 = minimaxMover scores
            let mover2 = minimaxMover scores
            let expScores = computeExpectedScores board state mover1 mover2 175 
            let p1Score, p2Score = expScores.[state]
            let expWinProb = computeExpectedWinProbabilities board state mover1 mover2 175 
            let p1WinProb, p2WinProb = expWinProb.[state]
            Console.Clear()
            printfn "Score: %f" scores.[state]
            printfn "Expected scores: Player 1: %.2f Player 2: %.2f" p1Score p2Score            
            printfn "Probabilities: Player 1 win: %.2f%% Player 2 win: %.2f%% Draw: %.2f%%" (p1WinProb * 100.0) (p2WinProb * 100.0) ((1.0 - p1WinProb - p2WinProb) * 100.0)
            let ns = nextStates board state
            if not ns.IsEmpty then
                let possibleStates = ns.[min (if largeMove then 1 else 0) (ns.Length - 1)]
                if not possibleStates.IsEmpty then
                    possibleStates |> Seq.iter (fun st -> printfn "Scores: %f" scores.[st])
                    let bestState = possibleStates |> Seq.maxBy (fun st -> scores.[st])
                    drawState board bestState |> showBitmap
        Thread.Sleep(100)

// draw scores and win probabilities for a random board
let drawScoresForRandomBoard() = 
    let board = randomBoard 8 7
    let state = initState board Player1
    let scores = computeMinimaxScores minimaxScore board state 500
    let score = scores.[state]
    let mover1 = minimaxMover scores
    let mover2 = minimaxMover scores
    let winLooseScores = computeExpectedWinProbabilities board state mover1 mover2 500
    let p1WinProb, p2WinProb = winLooseScores.[state]
    printfn "Score: %A, Player1 win probability: %.2f, Player2 win probability: %.2f" score (p1WinProb * 100.0) (p2WinProb * 100.0)
    drawState board state |> showBitmap
    Console.ReadKey() |> ignore
    drawScores board state scores |> showBitmap
    Console.ReadKey() |> ignore
    drawScores' board state scores |> showBitmap
    Console.ReadKey() |> ignore
    drawWinLooseScores board state winLooseScores |> showBitmap
    Console.ReadKey() |> ignore

// find and draw random boards with interesting properties
let findInterestingBoards() = 
    PSeq.init 10000 (fun _ -> randomBoard 8 7)
    |> PSeq.withMergeOptions ParallelMergeOptions.NotBuffered // avoids high memory usage, fine since order does not matter
    |> PSeq.map (fun board ->
        let state = initState board Player1
        let scores = computeMinimaxScores minimaxScore board state 500
        let score = scores.[state]
        let mover1 = minimaxMover scores
        let mover2 = minimaxMover scores
        let winLooseScores = computeExpectedWinProbabilities board state mover1 mover2 500
        let p1WinProb, p2WinProb = winLooseScores.[state]
        board, state, score, winLooseScores, p1WinProb, p2WinProb)
    |> selectLowest 10 (fun (_, _, _, _, p1WinProb, p2WinProb) -> -(min p1WinProb p2WinProb))
    |> Seq.iter (fun (board, state, score, winLooseScores, p1WinProb, p2WinProb) -> 
        printfn "Score: %A, Player1 win probability: %.2f, Player2 win probability: %.2f" score (p1WinProb * 100.0) (p2WinProb * 100.0)
        drawState board state |> showBitmap
        Console.ReadKey() |> ignore
        drawWinLooseScores board state winLooseScores |> showBitmap
        Console.ReadKey() |> ignore)

// generate scatter plot data of win probabilities
let generateScatterPlot() = 
    let output = 
        PSeq.init 10000 (fun _ -> randomBoard 8 7)
        |> PSeq.withMergeOptions ParallelMergeOptions.NotBuffered // avoids high memory usage, fine since order does not matter
        |> PSeq.map (fun board ->
            let state = initState board Player1
            let scores = computeMinimaxScores minimaxScore board state 500        
            let mover1 = minimaxMover scores
            let mover2 = minimaxMover scores
            let winLooseScores = computeExpectedWinProbabilities board state mover1 mover2 500
            let p1WinProb, p2WinProb = winLooseScores.[state]
            p1WinProb, p2WinProb)
        |> Seq.map (fun (p1WinProb, p2WinProb) -> sprintf "%f;%f" p1WinProb p2WinProb)
        |> Seq.append (Seq.singleton "p1WinProb;p2WinProb")

    File.WriteAllLines("plot.csv", output)

// simulate a large number of games and print statistics on game outcomes
let computeGameStatistics() =     
    let mutable numSimulations = 0
    let mutable numPlayer1Wins = 0
    let mutable numPlayer2Wins = 0
    let mutable numDraws = 0
    let mutable numTimeouts = 0
    let mutable player1Score = 0
    let mutable player2Score = 0

    let _, elapsed = stopwatch (fun _ ->
        PSeq.init 2000 (fun i -> 
            let board = randomBoard 8 7
            let initialState = initState board [Player1; Player2].[i % 2]
            let scores = computeMinimaxScores minimaxScore board initialState 500
            let mover1 = minimaxMover scores
            let mover2 = minimaxMover scores
            simulateGame board initialState mover1 mover2 100000 (fun _ -> ()))
        |> PSeq.withMergeOptions ParallelMergeOptions.NotBuffered
        |> Seq.iter (fun (result, p1score, p2score) ->
            player1Score <- player1Score + p1score
            player2Score <- player2Score + p2score
            match result with
            | Player1Wins -> numPlayer1Wins <- numPlayer1Wins + 1
            | Player2Wins -> numPlayer2Wins <- numPlayer2Wins + 1
            | Draw        -> numDraws <- numDraws + 1
            | NotEnded    -> numTimeouts <- numTimeouts + 1
            numSimulations <- numSimulations + 1
            if numSimulations % 1 = 0 then         
                if numSimulations > 1 then
                    Console.CursorTop <- Console.CursorTop - 6
                printfn "Player 1 wins: %d (%.2f%%)" numPlayer1Wins (float numPlayer1Wins / float numSimulations * 100.0)
                printfn "Player 2 wins: %d (%.2f%%)" numPlayer2Wins (float numPlayer2Wins / float numSimulations * 100.0)
                printfn "Draws:         %d (%.2f%%)" numDraws (float numDraws / float numSimulations * 100.0)
                printfn "Timeouts:      %d (%.2f%%)" numTimeouts (float numTimeouts / float numSimulations * 100.0)
                printfn "Player 1 score: %d" player1Score
                printfn "Player 2 score: %d" player2Score)            
    )
    printfn "Elapsed: %A" elapsed

[<EntryPoint>]
let main argv =   
    simulateRandomBoardGame()
    0