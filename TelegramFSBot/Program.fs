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
open System.Data
open Npgsql

type Mode =
| Learning
| Evaluating
| QA
| Training
with
    static member ofInt integer =
        match integer with
        | 0s -> Learning
        | 1s -> Evaluating
        | 2s -> QA
        | 3s -> Training
        | _ -> failwithf "%d is not a valid Mode value" integer
    member __.toInt =
        match __ with
        | Learning -> 0s
        | Evaluating -> 1s
        | QA -> 2s
        | Training -> 3s

type Answer =
| WhitespaceInsensitive
| ShortAnswer
//| EvaluatedType
//| EvaluatedCode
with
    static member ofInt integer =
        match integer with
        | 0s -> WhitespaceInsensitive
        | 1s -> ShortAnswer
        | _ -> failwithf "%d is not a valid Mode value" integer
    member __.toInt =
        match __ with
        | WhitespaceInsensitive -> 0s
        | ShortAnswer -> 1s

let (~%) (s : string) = System.Net.WebUtility.HtmlEncode s

module Parsing =
    open FParsec

    let short =
        pstringCI "short"
        >>. skipRestOfLine true
        >>. charsTillStringCI "]]!" true 3000
        .>>. restOfLine true
        |>> (fun (q,a) -> ShortAnswer, q.Trim(), a.Trim())

    let whitespaceInsensitive =
        pstringCI "ignore whitespace"
        >>. skipRestOfLine true
        >>. charsTillStringCI "]]!" true 3000
        .>>. restOfLine true
        |>> (fun (q,a) -> WhitespaceInsensitive, q.Trim(), a.Trim())

    let parse txt =
        let parser = short <|> whitespaceInsensitive
        match run parser txt with
        | Success (v,_,_) -> Result.Ok v
        | Failure (s,_,_) -> Result.Error s

module DB =
    let conn =
        let conn = new NpgsqlConnection "Server=127.0.0.1; Port=5432; Database=TelegramFSBot; User Id=postgres; Password=postgres"
        conn.Open ()
        conn

    let setMode (userId:int64) (mode:Mode) =
        use cmd = new NpgsqlCommand("update users set mode=@mode where userid=@id",conn)
        ignore <| cmd.Parameters.AddWithValue("@mode", mode.toInt)
        ignore <| cmd.Parameters.AddWithValue("@id", userId)
        ignore <| cmd.ExecuteNonQuery ()

    let getMode (userId:int64) =
        use cmd = new NpgsqlCommand("select mode from users where userid=@id", conn)
        ignore <| cmd.Parameters.AddWithValue("@id", userId)
        match cmd.ExecuteScalar() with
        | null ->
            cmd.CommandText <- "insert into users (userid, mode) values (@id, 0)"
            match cmd.ExecuteNonQuery () with
            | 1 -> Evaluating
            | n -> failwithf "Insertion into table users affected %d rows - this is incorrect.  Dying to avoid further data corruption." n
        | :? int16 as i -> Mode.ofInt i
        | v -> failwithf "]1 Value %A of type %A obtained, unexpected." v (v.GetType())

    let rec getQuestion (userId:int64) =
        use cmd = new NpgsqlCommand(@"select questions.question from questions,users where questions.id = users.question and userid = @id", conn)
        ignore <| cmd.Parameters.AddWithValue("@id", userId)
        match cmd.ExecuteScalar () with
        | null ->
            cmd.CommandText <- @"select id from questions where ""id"" not in (select question from seen where ""user"" = @id) order by random() limit 1" // ... yes, this is an evil query...
            match cmd.ExecuteScalar () with
            | null ->
                "I have no more questions for you to answer.  Please check back tomorrow."
            | :? int as questionId ->
                cmd.CommandText <- @"update users set question=@questionId where userid=@id"
                ignore <| cmd.Parameters.AddWithValue("@questionId", questionId)
                match cmd.ExecuteNonQuery () with
                | 1 ->  getQuestion userId
                | n -> failwithf "Data corruption - quitting!  Expected to affect 1 row, affected %d instead" n
            | v -> failwithf "]2 Value %A of type %A obtained, unexpected." v (v.GetType())
        | :? string as s -> s
        | v -> failwithf "]3 Value %A of type %A obtained, unexpected." v (v.GetType())

    let markCorrect (userId:int64) =
        use transact = conn.BeginTransaction()
        use cmd = new NpgsqlCommand(@"insert into seen (""user"", question) select userid, question from users where userid=@id", conn)
        ignore <| cmd.Parameters.AddWithValue("@id", userId)
        ignore <| cmd.ExecuteNonQuery ()
        cmd.CommandText <- "update users set question=NULL where userid=@id"
        ignore <| cmd.ExecuteNonQuery ()
        transact.Commit ()

    let getAnswer (userId:int64) =
        use cmd = new NpgsqlCommand(@"select answer, interpretation from questions,users where questions.id = users.question and userid = @id", conn)
        ignore <| cmd.Parameters.AddWithValue("@id", userId)
        using (cmd.ExecuteReader ()) (fun reader ->
            if reader.Read () = false then
                None
            else
                let (answer:string,interpretation:int16) = unbox reader.[0], unbox reader.[1]
                let answers = answer.Split("~") |> Seq.toList |> List.map (fun v -> v.Trim ())
                Some (Answer.ofInt interpretation, answers)
        )

    let addQuestion (i:Answer) q a userId =
        use cmd = new NpgsqlCommand(@"insert into questions (question, answer, interpretation, submitter) values (@q, @a, @i, @s)", conn)
        ignore <| cmd.Parameters.AddWithValue("@q", q)
        ignore <| cmd.Parameters.AddWithValue("@a", a)
        ignore <| cmd.Parameters.AddWithValue("@i", i.toInt)
        ignore <| cmd.Parameters.AddWithValue("@s", userId)
        match cmd.ExecuteNonQuery () with
        | 1 -> ()
        | n -> failwithf "Expecting to add a question, but modified %d rows - what went wrong??" n

(*
// I've spent about FOUR hours trying to actually get this to work, either with Postgres or Sqlite.
// Sick of it; the fact is that it's flaky and filled with "oh, load this ... no try this ... maybe this?"
// ... until you basically happen to hit on _just_ the right combination.  Pretty pathetic.
// And even if, somehow, you do manage to get it to work, an update can wreck your dependencies
// and lead to the same insane bug again.

// This is a long-running issue (https://github.com/fsprojects/SQLProvider/issues/373) and is still
// there in mid-2020.  The issue is, apparently, "Closed" 😂 ... yeah, _right_!

type Sql =
    SqlDataProvider<
        Common.DatabaseProviderTypes.POSTGRESQL,
        "Host=localhost;Database=TelegramFSBot;Username=postgres;Password=postgres",
        "", // ConnectionNameString
        @"C:\Users\cinyc\.nuget\packages\npgsql\4.1.3.1\lib\netstandard2.1",
        100, // individuals amount
        true, "public, admin, postgres">
*)

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

type ResponseType =
| AsReply
| AsMessage

let greetingWithTime () =
    match DateTime.Now.Hour with
    | 5 | 6 | 7 | 8 | 9 | 10 | 11 -> "Good morning"
    | 12 | 13 | 14 | 15 | 16 | 17 -> "Good afternoon"
    | 18 | 19 -> "Hello"
    | 20 | 21 | 22 | 23 -> "Good evening"
    | 0 | 1 | 2 | 3 | 4 -> "Good night 💤 (or early morning?)"
    | _ -> "Wotcher"

let normalMessage text (user:User) say =
    async {
        let! evaluation =
            pipeServer.PostAndTryAsyncReply((fun reply -> text, reply), 5500)
        let output =
            match evaluation with
            | None ->
                say <| sprintf "Sorry, %s, I'm taking a bit long to evaluate this code.  Are you sure you don't have an infinite loop somewhere inside it?" user.FirstName
            | Some v ->
                say v
        return output
    } |> Async.RunSynchronously

let startMessage (user:User) say =
    //say <| sprintf "%s, %s.  I'm here to help you with your functional programming.  By default, I will take F# code that you give me, evaluate it, and give you the result.  But I can do some other things, too!\n\n👋 <b>/start</b> will display this message\n👋 <b>/question</b> puts me into a question-and-answer mode, where I will give you small problems to solve\n👋 <b>/learn</b> puts me into \"learning\" mode, where <i>you</i> 💪 can teach me 🤖\n👋 <b>/eval</b> puts me into the default mode, where you can type code and have it evaluated by me.\n👋 <b>/help</b> gives you more information about the mode I'm in\n👋 <b>/about</b> tells you more about me." (greetingWithTime ()) user.FirstName
    say <| sprintf "%s, %s.  I'm here to help you with your functional programming.  By default, I will take F# code that you give me, evaluate it, and give you the result.  But I can do some other things, too!\n\n👋 <b>/start</b> will display this message\n👋 <b>/question</b> puts me into a question-and-answer mode, where I will give you small problems to solve\n👋 <b>/eval</b> puts me into the default mode, where you can type code and have it evaluated by me.\n👋 <b>/about</b> tells you more about me." (greetingWithTime ()) user.FirstName

let aboutMessage (user:User) say =
    say <| sprintf "Oh, %s, I hate to talk about myself!  But if you insist...\n\nI was born in a small Integrated Development Environment not far from here, and had a happy childhood where I spent most of my time playing with the lambdas, climbing the higher-order functions, and growing some monads in my back-yard.  I am written in F#, of course, and have been blessed to have wonderful godparents like Funogram and FSharp.Compiler.Service, both of whom I depend on quite heavily.  If you have any questions, suggestions, compliments, or complaints about me, you may direct them to my parent, <a href=\"tg://user?id=924587038\">🧎🏾</a>, who has made me into the bot I am today.  (Just click on his emoji and you can start a conversation with him)" user.FirstName

let questionMessage (answer:string option) (user:User) say =
    match answer with
    | None ->
        say <| DB.getQuestion user.Id
    | Some answer ->
        if String.IsNullOrWhiteSpace answer then
            say <| "Sorry, I didn't quite catch that?"
        else
            let assess expected pass =
                let passed = List.exists pass expected
                let modelAnswer = List.head expected
                if passed then
                    DB.markCorrect user.Id
                    say <| sprintf "<b>CORRECT!</b>\n<i>%s</i> was the expected answer.\n\n%s" %modelAnswer %(DB.getQuestion user.Id)
                else
                    say <| sprintf "Sorry, %s, that's not the answer that I was looking for.  Try again?" %user.FirstName
            match DB.getAnswer user.Id with
            | Some (ShortAnswer, expected) ->
                assess expected <| (fun e -> answer.Trim() = e)
            | Some (WhitespaceInsensitive, expected) ->
                assess expected <| fun e -> answer.Replace(" ","") = e.Replace(" ","")
            | None ->
                // I don't have any question down for this user, so let's ask one.
                // We take this branch in /question mode, when all questions have been
                // answered, and the user then says something.
                say <| DB.getQuestion user.Id

let parseAndInsert text (user:User) say =
    match Parsing.parse text with
    | Ok (i,q,a) ->
        DB.addQuestion i q a user.Id
        say <| "Question added, thank you!"
    | Error e ->
        say <| sprintf "Error during parse: <i>%s</i>" %e

let updateArrived (context : UpdateContext) =
    let response =
        maybe {
            let! msg = context.Update.Message |> Option.orElse context.Update.EditedMessage
            let! text = msg.Text
            let! user = msg.From
            let mode = DB.getMode user.Id // invoked for side-effect (!!!)
            let say replyType text =
                let replyType =
                    match replyType with
                    | AsReply -> Some msg.MessageId
                    | AsMessage -> None
                sendMessageBase (ChatId.Int user.Id) text (Some ParseMode.HTML) None None replyType None
            let output =
                match text with
                | "/start" -> startMessage user (say AsMessage)
                | "/about" -> aboutMessage user (say AsMessage)
(*
                | "/learn" ->
                    DB.setMode user.Id Learning
*)
                | "/question" ->
                    DB.setMode user.Id QA
                    questionMessage None user (say AsMessage)
                | "/eval" ->
                    DB.setMode user.Id Evaluating
                    say AsMessage "Evaluation mode is active, thank you.  I will evaluate all of your messages as code."
                | "/help" -> say AsMessage "NOT IMPLEMENTED YET!"
                | "/train" ->
                    if user.Id = 924587038L || user.Id = 924171662L || user.Id = 1011333639L || user.Id = 1264691500L then
                        DB.setMode user.Id Training
                        say AsMessage "Mega-thrusters are GO!"
                    else
                        say AsMessage "Naughty, naughty ... YOU shouldn't be trying THAT mode.  How did you even find out about it, I wonder? 🤔"
                | _ ->
                    match mode with
                    | Learning -> say AsMessage "NOT IMPLEMENTED YET!"
                    | QA -> questionMessage (Some text) user (say AsReply)
                    | Training -> parseAndInsert text user (say AsReply)
                    | Evaluating ->
                        normalMessage text user (say AsReply)
            let evaluatorComputation =
                async {
                    let! result = api config output
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
    try
        startBot config updateArrived None
        |> Async.RunSynchronously
    finally
        DB.conn.Dispose ()
    0 // return an integer exit code
