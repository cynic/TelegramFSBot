// Learn more about F# at http://fsharp.org

open System
open Funogram
open Funogram.Api
//open Funogram.Tools
open Funogram.Types
open Funogram.Telegram.Api
open Funogram.Telegram.Types
open Funogram.Telegram.Bot
open FSharpx.Option
open System.Diagnostics

open System.IO.Pipes

let startProcess handle =
    let path =
        System.Reflection.Assembly.GetExecutingAssembly().Location
        |> System.IO.Path.GetDirectoryName
    let v = new ProcessStartInfo()
    v.FileName <- "dotnet"
    v.WorkingDirectory <- path
    v.Arguments <- sprintf "FSIEvaluator.dll %s" handle
    let p = new Process()
    p.StartInfo <- v
    ignore <| p.Start ()
    printfn "Process started; PID is %d" p.Id
    p

let pipeServer : MailboxProcessor<string * AsyncReplyChannel<string>> =
    MailboxProcessor.Start(fun inbox ->
        let handle = System.Guid.NewGuid().ToString()
        let pipeServer () =
            let pipeServer =
                new NamedPipeServerStream(handle, PipeDirection.InOut)
            let client = startProcess handle
            let kill () =
                printfn "Disposing of pipe resources."
                pipeServer.Dispose ()
                if not <| client.HasExited then
                    printfn "Client %d not dead yet - killing it." client.Id
                    client.Kill true
            let write =
                FSIEvaluator.IO.write pipeServer
            let read = FSIEvaluator.IO.read pipeServer
            printfn "Pipe waiting for connection ..."
            pipeServer.WaitForConnection ()
            printfn "Pipe connected!"
            kill, write, read
        let rec loop (kill, write, read) =
            async {
                let! (msg, reply) = inbox.Receive ()
                let k, w, r =
                    try
                        write msg
                        reply.Reply(read 5000)
                        kill, write, read
                    with
                    | e ->
                        kill ()
                        printfn "Pipe crashed/infinilooped on input:\n-----\n%s\n-----\n%A" msg e
                        reply.Reply("Sorry, I couldn't evaluate this.  Maybe I crashed (maybe with a stack overflow?) or am looping infinitely!  Or I've been looping for 5 seconds, which is a lifetime for me...please check that your code doesn't have any infinite loops, and then send it to me again.")
                        pipeServer ()
                return! loop (k, w, r)
            }
        loop <| pipeServer ()
    )

// -----------------------------------------------------------------------------------------


// -------------------------------------------------------------------------------

let config =
    let path =
        System.IO.Path.Combine
            [| System.Reflection.Assembly.GetExecutingAssembly().Location
               |> System.IO.Path.GetDirectoryName
            ; "tokenFile"
            |]
    { defaultConfig with
        Token = System.IO.File.ReadAllText path
    }

let updateArrived (context : UpdateContext) =
    let response =
        maybe {
            let! msg = context.Update.Message |> Option.orElse context.Update.EditedMessage
            let! text = msg.Text
            let! user = msg.From
            let userId = user.Id
            let say text =
                sendMessageBase (ChatId.Int userId) text (Some ParseMode.HTML) None None (Some msg.MessageId) None
            let evaluatorComputation =
                async {
                    let! evaluation =
                        pipeServer.PostAndTryAsyncReply((fun reply -> text, reply), 5500)
                    let statement =
                        match evaluation with
                        | None ->
                            say <| sprintf "Sorry, %s, I'm taking a bit long to evaluate this code.  Are you sure you don't have an infinite loop somewhere inside it?" user.FirstName
                        | Some v ->
                            say v
                    let! result = api config statement
                    match result with
                    | Ok _ -> ()
                    | Error e ->
                        printfn "ErrorCode %d: %s" e.ErrorCode e.Description
                }
            return (Async.Start evaluatorComputation)
        }
    match response with
    | Some _ -> ()
    | None ->
        printfn "Couldn't reply for input: %A" context
    
[<EntryPoint>]
let main _ =
    printfn "Starting."
    startBot config updateArrived None
    |> Async.RunSynchronously
    0 // return an integer exit code
