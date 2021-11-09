module Valvo

open System

type FieldColor = int voption

[<Struct>]
type Player = Player1 | Player2

[<Struct>]
type GameBoard = {
    Width : int;
    Height : int;
    Fields : FieldColor [];
    Valves : FieldColor []
}

let private globalRandom = new Random()

type ThreadSafeRandom = 
    [<ThreadStatic; DefaultValue>]
    static val mutable private localRandom : Option<Random>

    static member GetInstance() =
        match ThreadSafeRandom.localRandom with
        | None -> 
            lock globalRandom (fun _ ->
                match ThreadSafeRandom.localRandom with
                | None ->
                    let seed = globalRandom.Next()                
                    let random = new Random(seed)
                    ThreadSafeRandom.localRandom <- Some(random)
                    random
                | Some random -> random)            
        | Some random -> random

module List =
    let chooseRandomly list =
        let i = ThreadSafeRandom.GetInstance().Next(list |> List.length)
        list |> List.item i

module Array =
    let chooseRandomly array =
        let i = ThreadSafeRandom.GetInstance().Next(array |> Array.length)
        array |> Array.item i

let randomBoard width height numColors =
    let fields = Array.init (width * height) (fun i -> 
        if i = 0 || i = height - 1 || i = width * height - 1 || i = width * height - 1 - (height - 1) then
            ValueNone // starting positions and bottom left and bottom right corners
        else
            ValueSome (ThreadSafeRandom.GetInstance().Next(numColors))
    )
    let valves = Array.init (width * height) (fun i -> 
        if i = 0 || i = width * height - (2 * height) || i >= width * height - height - 1 || (i + 1) % height = 0 then
            ValueNone
        else
            ValueSome (ThreadSafeRandom.GetInstance().Next(numColors))
    )
    { Width = width; Height = height; Fields = fields; Valves = valves }

let randomSymmetricBoard width height numColors =
    let board = randomBoard width height numColors
    for i = width * height / 2 to width * height - 1 do
        board.Fields[i] <- board.Fields[width * height - 1 - i]
    for i = width * height / 2 to width * height - height - 2 do
        let mirroredPosition = width * height - 1 - i
        let leftPosition = mirroredPosition - (2 * (mirroredPosition % board.Height) + 1)        
        board.Valves[i] <- board.Valves[leftPosition]
    board

[<Struct>]
type GameState = {
    Player1Position : int;
    Player2Position : int;
    Turn : Player
}

let private startField (board : GameBoard) player =
    match player with
    | Player1 -> 0
    | Player2 -> board.Width * board.Height - 1

let initState board turn =
    { Player1Position = startField board Player1;
      Player2Position = startField board Player2;
      Turn = turn }

[<Struct>]
type EndState = NotEnded | Player1Wins | Player2Wins | Draw

let isEndState board state =
    let stepsRemaining player =
        match player with
        | Player1 -> (board.Width * board.Height - 1) - state.Player1Position
        | Player2 -> state.Player2Position

    if state.Player1Position = startField board Player2 then
        let steps = stepsRemaining Player2
        let bonus = steps * (steps - 1) / 2
        Player1Wins, (100 + bonus), 0
    elif state.Player2Position = startField board Player1 then
        let steps = stepsRemaining Player1
        let bonus = steps * (steps - 1) / 2
        Player2Wins, 0, (100 + bonus)
    elif state.Player2Position = state.Player1Position + 1 then
        let steps1 = stepsRemaining Player1
        let steps2 = stepsRemaining Player2
        let bonus = abs (steps1 - steps2) * (abs (steps1 - steps2) - 1) / 5
        Draw, (if steps1 < steps2 then bonus else 0), (if steps2 < steps1 then bonus else 0)
    else
        NotEnded, 0, 0

let nextStates board state =
    match isEndState board state with
    | Player1Wins, _, _ 
    | Player2Wins, _, _
    | Draw,        _, _ -> [[]]
    | NotEnded,    _, _ -> 
        let innerNextStates largeMove =
            let move state steps =
                match state.Turn with
                | Player1 -> { state with Player1Position = state.Player1Position + steps; Turn = Player2 }
                | Player2 -> { state with Player2Position = state.Player2Position - steps; Turn = Player1 }
            let canMoveTo (state : GameState) (destinationState : GameState) =
                let order1 = compare state.Player1Position state.Player2Position
                let order2 = compare destinationState.Player1Position destinationState.Player2Position
                destinationState.Player1Position < board.Width * board.Height && destinationState.Player2Position >= 0 && order1 = order2
            let moveThroughValves (state : GameState) player =
                let openValveColor = 
                    match player with
                    | Player1 -> board.Fields[state.Player2Position]
                    | Player2 -> board.Fields[state.Player1Position]
                if openValveColor = ValueNone then state else
                let position =
                    match player with
                    | Player1 -> state.Player1Position
                    | Player2 -> state.Player2Position
                let rec findLeftValvesEnd position =
                    let leftPosition = position - (2 * (position % board.Height) + 1)
                    if leftPosition >= 0 && board.Valves[leftPosition] = openValveColor && 
                        state.Player1Position <> leftPosition && state.Player2Position <> leftPosition then
                        findLeftValvesEnd leftPosition
                    else
                        position
                let rec findRightValvesEnd position =
                    let rightPosition = position + (2 * board.Height - 1 - 2 * (position % board.Height))
                    if rightPosition < board.Width * board.Height && board.Valves[position] = openValveColor && 
                        state.Player1Position <> rightPosition && state.Player2Position <> rightPosition then
                        findRightValvesEnd rightPosition
                    else
                        position
                let leftValvesEnd = findLeftValvesEnd position
                let rightValvesEnd = findRightValvesEnd position
                match player with
                | Player1 -> { state with Player1Position = if leftValvesEnd <> position then leftValvesEnd else rightValvesEnd }
                | Player2 -> { state with Player2Position = if rightValvesEnd <> position then rightValvesEnd else leftValvesEnd }
            let newState1 = move state 2
            let newState2 = move state (if largeMove then 3 else 1)
            let canMoveToNewState1 = canMoveTo state newState1
            let canMoveToNewState2 = canMoveTo state newState2
            seq {
                if canMoveToNewState1 && (not largeMove || canMoveToNewState2) then
                    yield moveThroughValves newState1 state.Turn
                if canMoveToNewState2 && (not largeMove || canMoveToNewState1) then
                    yield moveThroughValves newState2 state.Turn
            } |> Seq.toList
        [innerNextStates false; innerNextStates true] |> List.except [[]]

let simulateGame board initialState mover1 mover2 maxIterations callback =    
    let rec simulateMove state i =
        callback state
        let endState = isEndState board state
        match endState with
        | NotEnded, _, _ when i < maxIterations ->            
            let possibleStates = nextStates board state |> List.chooseRandomly
            let newState = 
                match state.Turn with
                | Player1 -> mover1 state possibleStates
                | Player2 -> mover2 state possibleStates
            simulateMove newState (i + 1)
        | _, _, _ -> endState

    simulateMove initialState 0