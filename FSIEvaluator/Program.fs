module FSIEvaluator
// Learn more about F# at http://fsharp.org
open System
open FSharp.Compiler.Interactive.Shell
open FSharp.Compiler.SourceCodeServices
open System.Net

open System
open System.IO
open System.Text
open FSharp.Compiler.Interactive
open System.IO.Pipes

module IO =
    let write (writer:PipeStream) (s:string) =
        let bytes = System.Text.UTF8Encoding.UTF8.GetBytes s
        let bytesLen =
            let len = Array.length bytes
            [| 0xFEuy
            ;  byte (len >>> 24)
            ;  byte ((len >>> 16) &&& 0xFF)
            ;  byte ((len >>> 8) &&& 0xFF)
            ;  byte (len &&& 0xFF)
            ;  0x0Fuy
            |]
        writer.Write(bytesLen, 0, 6)
        writer.Write(bytes, 0, Array.length bytes)
        writer.Flush ()
        writer.WaitForPipeDrain ()
    let rec read (reader:PipeStream) timeout =
        let parseLengthBytes (buffer : byte array) ofs =
            ((int buffer.[0+ofs]) <<< 24) |||
            ((int buffer.[1+ofs]) <<< 16) |||
            ((int buffer.[2+ofs]) <<< 8) |||
            (int buffer.[3+ofs]), buffer.[4+ofs] = 0x0Fuy
        let remaining, sync =
            let lenBuf =
                Async.RunSynchronously (reader.AsyncRead 6, timeout)
            if lenBuf.[0] = 0xFEuy then
                parseLengthBytes lenBuf 1
            else
                0, false
        let rec readData (remaining, sync) =
            if sync then
                printfn "Reading - %d chars remaining." remaining
                let buf = Async.RunSynchronously (reader.AsyncRead remaining, timeout)
                printfn "Read complete."
                System.Text.UTF8Encoding.UTF8.GetString(buf)
            else
                printfn "Re-synchronizing..."
                while byte (reader.ReadByte ()) <> 0xFEuy do ()
                let lenBuf =
                    Async.RunSynchronously (reader.AsyncRead 5, timeout)
                readData <| parseLengthBytes lenBuf 0
        readData (remaining, sync)

// Initialize output and input streams
let sbOut = new StringBuilder()
let sbErr = new StringBuilder()
let inStream = new StringReader("")
let outStream = new StringWriter(sbOut)
let errStream = new StringWriter(sbErr)

// Build command line arguments & start FSI session
let args = [| "dotnet"; "fsi"; "--noninteractive" |]
let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
let fsiSession = FsiEvaluationSession.Create(fsiConfig, args, inStream, outStream, errStream)

let (~%) (s : string) = WebUtility.HtmlEncode s

// next 2 are (largely) taken from https://stackoverflow.com/a/51828781 .
let rec (|TFunc|_|) (typ: Type) =
    if typ.IsGenericType && typ.GetGenericTypeDefinition () = typeof<int->int>.GetGenericTypeDefinition () then
        match typ.GetGenericArguments() with
        | [|targ1; targ2|] -> Some (targ1, targ2)
        | _ -> None
    else
        None

and (|TList|_|) (typ:Type) =
    if typ.IsGenericType && typ.GetGenericTypeDefinition () = typeof<List<int>>.GetGenericTypeDefinition () then
        match typ.GetGenericArguments () with
        | [|p|] -> Some p
        | _ -> None
    else None

and (|TOption|_|) (typ:Type) =
    if typ.IsGenericType && typ.GetGenericTypeDefinition () = typeof<Option<int>>.GetGenericTypeDefinition () then
        match typ.GetGenericArguments () with
        | [|p|] -> Some p
        | _ -> None
    else None

and (|TTuple|_|) =
    let t2 = typeof<Tuple<int,int>>.GetGenericTypeDefinition ()
    let t3 = typeof<Tuple<int,int,int>>.GetGenericTypeDefinition ()
    let t4 = typeof<Tuple<int,int,int,int>>.GetGenericTypeDefinition ()
    let t5 = typeof<Tuple<int,int,int,int,int>>.GetGenericTypeDefinition ()
    fun (typ:Type) -> 
        if typ.IsGenericType then
            let gtd = typ.GetGenericTypeDefinition ()
            if gtd = t2 || gtd = t3 || gtd = t4 || gtd = t5 then
                match typ.GetGenericArguments () with
                | [|a;b|] -> Some [a;b]
                | [|a;b;c|] -> Some [a;b;c]
                | [|a;b;c;d|] -> Some [a;b;c;d]
                | [|a;b;c;d;e|] -> Some [a;b;c;d;e]
                | _ -> None
            else None
        else None

and typeStr (typ: Type) =
    match typ with
    | TFunc (t1, t2) when t1.IsGenericType -> sprintf "(%s) 🡲 %s" (typeStr t1) (typeStr t2)
    | TFunc (t1, t2) -> sprintf "%s 🡲 %s" (typeStr t1) (typeStr t2)
    | TList p -> sprintf "%s list" (typeStr p)
    | TOption p -> sprintf "%s option" (typeStr p)
    | TTuple [a;b] -> sprintf "%s * %s" (typeStr a) (typeStr b)
    | TTuple [a;b;c] -> sprintf "%s * %s * %s" (typeStr a) (typeStr b) (typeStr c)
    | TTuple [a;b;c;d] -> sprintf "%s * %s * %s * %s" (typeStr a) (typeStr b) (typeStr c) (typeStr d)
    | TTuple [a;b;c;d;e] -> sprintf "%s * %s * %s * %s * %s" (typeStr a) (typeStr b) (typeStr c) (typeStr d) (typeStr e)
    | typ when typ = typeof<int> -> "int"
    | typ when typ = typeof<string> -> "string"
    | typ when typ = typeof<unit> -> "unit"
    | typ when typ = typeof<bool> -> "bool"
    | typ when typ = typeof<float> -> "float"
    | typ when typ.IsGenericParameter -> sprintf "'%s" (string typ)
    | typ -> if typ.IsGenericType then sprintf "(%O)" typ else string typ

/// Evaluate expression & return the result
let evalExpression text =
    let result, warnings = fsiSession.EvalExpressionNonThrowing text
    match result with
    | Choice1Of2 (Some value) ->
        sprintf "<pre>%s</pre><i>%s</i>" %value.ReflectionValue %value.ReflectionType
    | Choice1Of2 None ->
        "I evaluated this, but couldn't make any sense of the result!  What funny thing have you typed in?"
    | Choice2Of2 e ->
        sprintf "<b>Exception</b>\n<i>%s</i>" %e.Message

let runPipe handle =
    let client = new NamedPipeClientStream(".", handle, PipeDirection.InOut, PipeOptions.None)
    printfn "Connecting pipe to server..."
    client.Connect ()
    printfn "Client active."
    let read = IO.read client
    let write = IO.write client
    let rec repl () =
        let expr = read -1
        write <| evalExpression expr
        repl ()
    repl ()

[<EntryPoint>]
let main argv =
    match argv with
    | [|handle|] ->
        runPipe handle
        0
    | _ ->
        printfn "ARGV wasn't what I expected!  It was: %A" argv
        -1
