module ValvoProcessReader

open System
open System.Diagnostics
open System.Runtime.InteropServices;
open Valvo

module private NativeMethods =
    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern nativeint OpenProcess(int dwDesiredAccess, bool bInheritHandle, int dwProcessId)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool ReadProcessMemory(nativeint hProcess, nativeint lpBaseAddress, byte[] lpBuffer, int dwSize, nativeint& lpNumberOfBytesRead)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool CloseHandle(nativeint hObject)

    [<DllImport("user32.dll", SetLastError = true)>]
    extern IntPtr FindWindow(string lpClassName, string lpWindowName);

    [<DllImport("user32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
    extern nativeint SendMessage(nativeint hWnd, uint16 Msg, nativeint wParam, nativeint lParam)

    let PROCESS_VM_READ = 0x0010
    let WM_LBUTTONDOWN = 0x0201us
    let WM_LBUTTONUP = 0x0202us
    let MK_LBUTTON = 0x0001n

let private readBytes hProcess address length =
    let buffer = Array.zeroCreate length
    let mutable bytesRead = 0n

    let success = NativeMethods.ReadProcessMemory(hProcess, address, buffer, buffer.Length, &bytesRead)

    if not success || bytesRead <> nativeint (buffer.Length) then
        failwith "Error reading process memory"

    buffer

let private readInt32 hProcess address =
    let buffer = readBytes hProcess address 4
    BitConverter.ToInt32(buffer, 0)

let readBoardAndState() =
    let valvoProcess = Process.GetProcessesByName("Valvo") |> Array.exactlyOne

    let hProcess = NativeMethods.OpenProcess(NativeMethods.PROCESS_VM_READ, false, valvoProcess.Id)

    if hProcess = 0n then
        failwith "OpenProcess failed"

    try
        let helperAddress = readInt32 hProcess (valvoProcess.MainModule.BaseAddress + 0x974F4n) |> nativeint
        
        let board =
            let boardBuffer = readBytes hProcess (helperAddress + 0x39Cn) (6 * 56)

            let reorder s =
                s |> Seq.chunkBySize 7 |> Seq.mapi (fun i x -> if i % 2 = 1 then Seq.rev x else x |> Array.toSeq) |> Seq.concat
            let idToFieldColor x = 
                if x > 3uy then ValueNone else ValueSome (int x)
            let idToValveColor x = 
                if x = 0uy then ValueNone else ValueSome (int x - 1)
            
            let fields = [0..55] |> Seq.map (fun i -> boardBuffer.[i * 6]) |> Seq.map idToFieldColor |> reorder |> Seq.toArray
            let valves = [0..55] |> Seq.map (fun i -> boardBuffer.[i * 6 + 2]) |> Seq.map idToValveColor |> reorder |> Seq.toArray
        
            { Width = 8; Height = 7; Fields = fields; Valves = valves }
        
        let state =
            let positionBuffer = readBytes hProcess (helperAddress + 0x4EEn) (4 * 4)
            
            let player1X = BitConverter.ToInt32(positionBuffer, 0) - 1
            let player1Y = BitConverter.ToInt32(positionBuffer, 4) - 1
            let player2X = BitConverter.ToInt32(positionBuffer, 8) - 1
            let player2Y = BitConverter.ToInt32(positionBuffer, 12) - 1
            
            let xyToIndex x y =
                x * board.Height + if x % 2 = 0 then y else board.Height - y - 1
            let player1Position = xyToIndex player1X player1Y
            let player2Position = xyToIndex player2X player2Y
            
            let turnBuffer = readBytes hProcess (helperAddress + 0x502n) 1
            
            let turn = 
                match turnBuffer.[0] with
                | 0uy 
                | 1uy -> Player1
                | 2uy -> Player2
                | _ -> failwith "Unexpected value read"
            
            { Player1Position = player1Position; Player2Position = player2Position; Turn = turn }
        
        let largeMove =
            let largeMoveBuffer = readBytes hProcess (helperAddress + 0xFF0n) 1
            
            match largeMoveBuffer.[0] with
            | 1uy -> false
            | 2uy -> true
            | _   -> failwith "Unexpected value read"

        board, state, largeMove

    finally
        NativeMethods.CloseHandle(hProcess) |> ignore
        
let performClick fx fy =
    let valvoWindow = NativeMethods.FindWindow("TFormValvo", "Valvo")

    if valvoWindow = 0n then
        failwith "Could not find Valvo window"

    let x = 128 + 47 * fx
    let y = 126 + 41 * fy

    let lParam = nativeint x ||| (nativeint y <<< 16)
    
    NativeMethods.SendMessage(valvoWindow, NativeMethods.WM_LBUTTONDOWN, NativeMethods.MK_LBUTTON, lParam) |> ignore
    NativeMethods.SendMessage(valvoWindow, NativeMethods.WM_LBUTTONUP, 0n, lParam) |> ignore
