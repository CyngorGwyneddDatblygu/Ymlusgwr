open System
open System.IO
open System.Net
open System.Text.RegularExpressions
open System.Threading
open System.Collections.Concurrent
open System.Collections.Generic


module Cofnodi =

    type MathCofnod =
        | Dadfygio
        
    let cofnodi (mathCofnod : MathCofnod) neges =
        let amser = DateTime.UtcNow.ToString("yyyy-MM-dd HH:mm:ss")
        Printf.kprintf (printfn "%s [%s] %s"
                            <| amser
                            <| mathCofnod.ToString()
                            ) neges
        
    let dadfygio neges =
        (cofnodi MathCofnod.Dadfygio) neges


module Cymhorthwyr =
    open Cofnodi    
    
    let nôlHtml (uri  : Uri) =
        try
            dadfygio "nôlHtml i %A" uri
            let cais = WebRequest.Create(uri)
            cais.Timeout <- 5000
            use ymateb = cais.GetResponse()
            let isHtml = ymateb.ContentType.ToLower().Contains("html")
            if isHtml then
                use stream = ymateb.GetResponseStream()
                use reader = new StreamReader(stream)
                let html = reader.ReadToEnd()
                Some html, Some HttpStatusCode.OK
            else
                None, Some HttpStatusCode.OK
        with
            | :? WebException as ex ->
                if ex.Response <> null then
                    None, Some (ex.Response :?> HttpWebResponse).StatusCode
                else
                    None, None
            | _ -> None, None

    let nôlCysylltau (html : string) =
        let patrwm = Regex(@"(href|src|rel)=""([^""]*)""")
        let matches = patrwm.Matches(html)
        seq { for m in matches -> m } |> Seq.map (fun m -> m.Groups.[2].Value)

    let normaleiddioCysylltau uriSylfaenol cysylltau =
        let patrwmProtocol = Regex(@"^[a-zA-Z]+:")
        cysylltau
        |> Seq.map (fun cyswllt ->
                if patrwmProtocol.IsMatch(cyswllt) then
                    Uri(cyswllt)
                else
                    Uri(uriSylfaenol, cyswllt)
            )

    let yrUnWefan (uriA : Uri) (uriB : Uri) =
        uriA.Scheme = uriB.Scheme && uriA.Host = uriB.Host && uriA.Port = uriB.Port


module Ymlusgo =

    open Cymhorthwyr
    open Cofnodi
    open System

    type CanlyniadTudalen = { UriTudalen : Uri ; Statws : HttpStatusCode option ; Cysylltau : Uri list }

    type Neges =
        | Ciwio of Uri
        | ChwilioAmWaith
        | CasgliwrYnBarod of rhifCasgliwr : int
        | WediGorffen

    type NegesCasglu =
        | Casglu of Uri*MailboxProcessor<Neges>

    type StadCasgliwr =
        | Barod
        | Prysur
        | WediCau

    type Casgliwr = { Mailbox : MailboxProcessor<NegesCasglu> ; Stad : StadCasgliwr ; Uri : Uri option }
    

    let ymlusgo url =
        let tudalenCychwyn = Uri(url)
        let tudalennauIProfi = ConcurrentQueue<Uri>()
        let tudalennauWediProfi = ConcurrentDictionary<Uri, CanlyniadTudalen>()
        let niferCasglwyr = 10

        let goruchwyliwr =
            let casglwyr : Dictionary<int,Casgliwr> =
                [ 1..niferCasglwyr ]
                |> List.fold (fun dictCasglwyr i ->
                    dictCasglwyr.[i] <- {
                        Mailbox = (MailboxProcessor.Start(fun inbox ->
                                let rec loop state =
                                    async {
                                        let! msg = inbox.Receive()
                                        dadfygio "Casgliwr %d wedi derbyn neges %A" i msg
                                        match msg with
                                        | Casglu(uri, mailboxGoruwchwyliwr) ->

                                                // Gwirio os yw'r dudalen wedi prosesu'n barod - dim angen gwneud unrhyw beth os mae o
                                                if not (tudalennauWediProfi.ContainsKey uri) then
                                                    let canlyniad = nôlHtml uri
                                                    let cysylltauTudalen =
                                                        match canlyniad with
                                                        | Some html, Some statws when statws = HttpStatusCode.OK ->
                                                            // os yr un wefan
                                                            if yrUnWefan tudalenCychwyn uri then
                                                                nôlCysylltau html |> normaleiddioCysylltau uri |> List.ofSeq
                                                            else
                                                                dadfygio "%A yn wefan gwahanol" uri
                                                                []
                                                        | _ ->
                                                            dadfygio "Dim HTML i %A" uri 
                                                            []

                                                    let canlyniadTudalen = { UriTudalen = uri ; Statws = snd canlyniad ; Cysylltau = cysylltauTudalen }

                                                    // Ciwio'r cysylltau
                                                    cysylltauTudalen |> List.iter (fun ur -> mailboxGoruwchwyliwr.Post (Ciwio ur) )
                                                    
                                                    dadfygio "Ychwanegu %A" canlyniadTudalen

                                                    tudalennauWediProfi.TryAdd(uri, canlyniadTudalen) |> ignore // anwybyddu os yn bodoli yn barod (h.y. wedi ei ychwanegu gan thread arall)
                                                    
                                                dadfygio "Casgliwr %d wedi cwblhau %A" i uri
                                                mailboxGoruwchwyliwr.Post <| CasgliwrYnBarod i
                                                return! loop state
                                    }
                                loop 0)) ;
                        Stad = Barod
                        Uri = None
                    }
                    dictCasglwyr                    
                    ) (Dictionary<int,Casgliwr>())

            MailboxProcessor.Start(fun inbox ->
                let rec loop state =
                    async {
                        let! msg = inbox.Receive()
                        
                        match msg with
                        | ChwilioAmWaith -> ()
                        | _ -> dadfygio "Wedi derbyn neges: %A" msg
                        
                        match msg with
                        | Ciwio uri ->                                                        
                            tudalennauIProfi.Enqueue(uri)
                            inbox.Post ChwilioAmWaith
                            return! loop state
                        | ChwilioAmWaith ->
                            // Chwilio am casgliwr gwag
                            let casglwyrRhydd = [
                                for kv in casglwyr do
                                    match kv.Value.Stad with
                                    | Barod -> yield kv.Key
                                    | _ -> ()
                                    //if kv.Value.Barod then
                                    //    yield kv.Key
                            ]
                            //printfn "CasglwyrRhydd: %A" casglwyrRhydd
                            if List.isEmpty casglwyrRhydd then
                                //printfn "Dim gweithwyr yn rhydd"
                                Thread.Sleep 100
                            else
                                let llwyddiant, uri = tudalennauIProfi.TryDequeue()
                                if llwyddiant then
                                    let mbox = casglwyr.[casglwyrRhydd.Head].Mailbox
                                    mbox.Post <| Casglu(uri, inbox)
                                    casglwyr.[casglwyrRhydd.Head] <- { Mailbox = mbox ; Stad = Prysur ; Uri = Some uri } // Gosod fel dim yn barod
                                else
                                    dadfygio "**CIW YN WAG**"
                                    if (List.length casglwyrRhydd) = casglwyr.Count then
                                        dadfygio "***CIW YN WAG A BOB CASGLIWR YN BAROD***"
                                        inbox.Post WediGorffen
                                        return! loop state
                                    Thread.Sleep 100
                            inbox.Post ChwilioAmWaith
                            return! loop state
                        | CasgliwrYnBarod rhifCasgliwr ->
                            let casgliwr = casglwyr.[rhifCasgliwr]
                            casglwyr.[rhifCasgliwr] <- { Mailbox = casgliwr.Mailbox ; Stad = Barod ; Uri = None }
                            dadfygio "Wedi gosod casgliwr %d yn barod" rhifCasgliwr
                            inbox.Post ChwilioAmWaith
                            return! loop state
                        | WediGorffen ->
                            let cysylltauWediTorri =
                                [
                                    for kv in tudalennauWediProfi do
                                        match kv.Value.Statws with
                                        | None -> ()                                        
                                        | Some statws when statws = HttpStatusCode.OK -> ()
                                        | _ -> yield kv.Value
                                ]
                                |> List.map (fun t -> t.UriTudalen, t.Statws)
                            printfn "Cysylltau wedi torri:"
                            cysylltauWediTorri
                            |> List.iter (fun (uri,statws) -> printfn "%A\t%A" uri statws)
                            printfn "---"
                            (inbox :> IDisposable).Dispose()
                    }
                loop 0)
        goruchwyliwr.Post (Ciwio tudalenCychwyn)
        [ "" ]


open Ymlusgo
open Cofnodi

[<EntryPoint>]
let main argv =
    let url = "https://oriel.madarch.org"
   
    let rhywbeth = ymlusgo url    
    
    dadfygio "Wedi gorffen!"
    Console.ReadLine() |> ignore
    0 // return an integer exit code
