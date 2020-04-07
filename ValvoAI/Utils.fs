module Utils

open System
open System.Collections.Generic
open System.Drawing
open System.Drawing.Drawing2D
open System.Windows.Forms
open System.Threading
open Valvo

let stopwatch f =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let result = f()
    result, sw.Elapsed

let mutable form = null
let mutable pictureBox = null

let thread = new Thread(new ParameterizedThreadStart(fun param ->
    let bitmap = param :?> Bitmap
    form <- new Form()
    pictureBox <- new PictureBox()
    pictureBox.Dock <- DockStyle.Fill
    pictureBox.SizeMode <- PictureBoxSizeMode.StretchImage
    pictureBox.Image <- bitmap
    form.Controls.Add(pictureBox)
    form.ClientSize <- new Size(bitmap.Width, bitmap.Height)
    form.ShowDialog() |> ignore))

thread.SetApartmentState(ApartmentState.STA);

let showBitmap bitmap =
    match thread.ThreadState with
    | ThreadState.Unstarted -> 
        thread.Start(bitmap)
        Thread.Sleep(500)
    | ThreadState.Running   ->         
        form.Invoke(new Action(fun _ -> pictureBox.Image <- bitmap)) |> ignore
        form.Invoke(new Action(fun _ -> form.ClientSize <- new Size(bitmap.Width, bitmap.Height))) |> ignore
    | _                     -> ()

let drawScores (board : GameBoard) state (scores : IDictionary<GameState,float>) =
    let stateToIndices (state : GameState) =
        state.Player1Position + (if state.Turn = Player1 then 0 else board.Width * board.Height), state.Player2Position
    let matrix = Array2D.zeroCreate (board.Width * board.Height * 2) (board.Width * board.Height)
    let bitmap = new Bitmap(matrix |> Array2D.length1, matrix |> Array2D.length2)
    use graphics = Graphics.FromImage(bitmap)
    graphics.Clear(Color.Violet)
    scores
    |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
    |> Seq.iter (fun (state, score) ->
        let x, y = stateToIndices state
        matrix.[x, y] <- score
        let c = max (min (127 + int (score / float (board.Width * board.Height) * 10.0 + 0.5)) 255) 0
        let color = Color.FromArgb(c, c, c)
        bitmap.SetPixel(x, y, color)
    )
    bitmap.SetPixel(state.Player1Position, state.Player2Position, System.Drawing.Color.Red)
    bitmap.SetPixel(state.Player1Position + board.Width * board.Height, state.Player2Position, System.Drawing.Color.Red)
    bitmap

// like drawScores but scores of both players are overlaid over each other
let drawScores' (board : GameBoard) state (scores : IDictionary<GameState,float>) =
    let stateToIndices (state : GameState) =
        state.Player1Position, state.Player2Position
    let matrix = Array2D.create (board.Width * board.Height) (board.Width * board.Height) (0.0, 0.0)
    let bitmap = new Bitmap(matrix |> Array2D.length1, matrix |> Array2D.length2)
    use graphics = Graphics.FromImage(bitmap)
    graphics.Clear(Color.Violet)
    scores
    |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
    |> Seq.iter (fun (state, score) ->
        let x, y = stateToIndices state
        match state.Turn with
        | Player1 -> matrix.[x, y] <- score, snd matrix.[x, y]
        | Player2 -> matrix.[x, y] <- fst matrix.[x, y], score
        let r = max (min (127 + int (fst matrix.[x, y] / float (board.Width * board.Height) * 10.0 + 0.5)) 255) 0
        let b = max (min (127 + int (snd matrix.[x, y] / float (board.Width * board.Height) * 10.0 + 0.5)) 255) 0
        let color = Color.FromArgb(r, 0, b)
        bitmap.SetPixel(x, y, color)
    )
    bitmap.SetPixel(state.Player1Position, state.Player2Position, System.Drawing.Color.Green)
    bitmap

let drawWinLooseScores (board : GameBoard) state (scores : IDictionary<GameState,(float * float)>) =
    let stateToIndices (state : GameState) =
        state.Player1Position + (if state.Turn = Player1 then 0 else board.Width * board.Height), state.Player2Position
    let bitmap = new Bitmap(board.Width * board.Height * 2, board.Width * board.Height)
    use graphics = Graphics.FromImage(bitmap)
    graphics.Clear(Color.Violet)
    scores
    |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
    |> Seq.iter (fun (state, (p1WinProb, p2WinProb)) ->
        let x, y = stateToIndices state
        let r = int (p1WinProb * 255.0 + 0.5)
        let b = int (p2WinProb * 255.0 + 0.5)
        let g = max (255 - r - b) 0
        let color = Color.FromArgb(r, g, b)
        bitmap.SetPixel(x, y, color)
    )
    bitmap.SetPixel(state.Player1Position, state.Player2Position, System.Drawing.Color.Red)
    bitmap.SetPixel(state.Player1Position + board.Width * board.Height, state.Player2Position, System.Drawing.Color.Red)
    bitmap

let drawState (board : GameBoard) (state : GameState) =
    let fieldColors =
        [None,   Color.FromArgb(214, 214, 214);
         Red,    Color.FromArgb(255, 165, 165);
         Blue,   Color.FromArgb(165, 255, 255);
         Yellow, Color.FromArgb(255, 255, 165);
         Purple, Color.FromArgb(255, 165, 255)] |> Map.ofList
    let startField1Color = Color.FromArgb(132, 255, 132)
    let startField2Color = Color.FromArgb(247, 206, 206)
    let valveColors =
        [Red,    Color.Red;
         Blue,   Color.Cyan;
         Yellow, Color.Yellow;
         Purple, Color.Magenta] |> Map.ofList
    let fieldWidth = 47
    let fieldHeight = 41
    let fieldXY i =
        let fx = i / board.Height
        let fy = if fx % 2 = 0 then i % board.Height else board.Height - 1 - (i % board.Height)
        fx, fy
    let bitmap = new Bitmap(board.Width * fieldWidth + 1, board.Height * fieldHeight + 1)
    use graphics = Graphics.FromImage(bitmap)

    for i = 0 to board.Width * board.Height - 1 do
        let fx, fy = fieldXY i
        let c = 
            if i = 0 then startField1Color
            elif i = board.Width * board.Height - 1 then startField2Color
            else Map.find board.Fields.[i] fieldColors
        graphics.FillRectangle(new SolidBrush(c), fx * fieldWidth, fy * fieldHeight, fieldWidth, fieldHeight)
        graphics.DrawRectangle(Pens.Black, fx * fieldWidth, fy * fieldHeight, fieldWidth, fieldHeight)

    let wallPen = new Pen(Color.Black, 7.0f)
    wallPen.StartCap <- LineCap.Round
    wallPen.EndCap <- LineCap.Round
    for i = 0 to board.Width - 2 do
        let p1, p2 =
            if i % 2 = 0 then
                new Point((i + 1) * fieldWidth, 0), new Point((i + 1) * fieldWidth, (board.Height - 1) * fieldHeight)
            else
                new Point((i + 1) * fieldWidth, fieldHeight), new Point((i + 1) * fieldWidth, board.Height * fieldHeight)
        graphics.DrawLine(wallPen, p1, p2)

    let openValveColor = board.Fields.[if state.Turn = Player1 then state.Player2Position else state.Player1Position]
    for i = 0 to board.Width * board.Height - 1 do
        let fx, fy = fieldXY i
        if board.Valves.[i] <> None then
            let c = Map.find board.Valves.[i] valveColors        
            let valveOuterPen = new Pen(Color.Black, 11.0f)
            let valveInnerPen = new Pen(c, 7.0f)
            valveOuterPen.StartCap <- LineCap.Round
            valveOuterPen.EndCap <- LineCap.Round
            valveInnerPen.StartCap <- LineCap.Round
            valveInnerPen.EndCap <- LineCap.Round
            let p1, p2 =
                if board.Valves.[i] = openValveColor then
                   new Point((fx + 1) * fieldWidth, fy * fieldHeight + 7), new Point((fx + 1) * fieldWidth, (fy + 1) * fieldHeight - 7)
                else
                   new Point((fx + 1) * fieldWidth, fy * fieldHeight + 18), new Point((fx + 1) * fieldWidth, (fy + 1) * fieldHeight - 18)
            graphics.DrawLine(valveOuterPen, p1, p2)
            graphics.DrawLine(valveInnerPen, p1, p2)

    let drawPlayer player position size color color2 =
        let fx, fy = fieldXY position
        let x, y = (float32 fx + 0.5f) * float32 fieldWidth, (float32 fy + 0.5f) * float32 fieldHeight
        graphics.TranslateTransform(x, y)
        let rotationAngle = 
            if player = Player1 then
                if position % board.Height = board.Height - 1 && position <> board.Width * board.Height - 1 then
                    -90.0f
                else
                    if fx % 2 = 0 then 0.0f else 180.0f
            else
                if position % board.Height = 0 && position <> 0 then
                    90.0f
                else
                    if fx % 2 = 0 then 180.0f else 0.0f
        graphics.RotateTransform(rotationAngle)
        graphics.FillEllipse(new SolidBrush(color), 0.6f * -size, -size, 1.2f * size, 2.0f * size)
        graphics.FillPolygon(new SolidBrush(color2), [|new Point(-2, -5); new Point(2, -5); new Point(2, 1); new Point(5, 1); new Point(0, 7); new Point(-5, 1); new Point(-2, 1); new Point(-2, -5)|])
        graphics.ResetTransform()
    drawPlayer Player1 state.Player1Position 16.0f (Color.FromArgb(57, 115, 8)) (Color.FromArgb(132, 255, 132))
    drawPlayer Player2 state.Player2Position 16.0f (Color.FromArgb(173, 41, 57)) (Color.FromArgb(247, 206, 206))

    bitmap

let selectLowest count key seq =
    if Seq.isEmpty seq then [||] else
    let mutable selected = new List<'T>()
    let mutable highestSelected = key (seq |> Seq.head)
    for item in seq do
        let k = key item
        if selected.Count < count || k < highestSelected then
            selected.Add(item)
            selected <- new List<'T>(selected |> Seq.sortBy key |> Seq.take (min count selected.Count))
            highestSelected <- selected |> Seq.map key |> Seq.max
    selected |> Seq.toArray