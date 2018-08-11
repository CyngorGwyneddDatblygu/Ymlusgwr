module Ymlusgo

open System
open System.Net
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading
open Cymhorthwyr
open Cofnodi


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
    let mutable wediGorffen = false

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
                    
                    dadfygio "Wedi derbyn neges: %A" msg
                    
                    match msg with
                    | Ciwio uri ->                                                        
                        tudalennauIProfi.Enqueue(uri)
                        inbox.Post ChwilioAmWaith
                        return! loop state
                        
                    | ChwilioAmWaith ->
                        if wediGorffen then
                            dadfygio "ChwilioAmWaith on wedi gorffen yn barod"
                            return ()
                            
                        // Chwilio am casgliwr gwag
                        let casglwyrRhydd = [
                            for kv in casglwyr do
                                match kv.Value.Stad with
                                | Barod -> yield kv.Key
                                | _ -> ()
                        ]
                        if List.isEmpty casglwyrRhydd then
                            Thread.Sleep 100
                        else
                            let llwyddiant, uri = tudalennauIProfi.TryDequeue()
                            if llwyddiant then
                                let mbox = casglwyr.[casglwyrRhydd.Head].Mailbox
                                mbox.Post <| Casglu(uri, inbox)
                                casglwyr.[casglwyrRhydd.Head] <- { Mailbox = mbox ; Stad = Prysur ; Uri = Some uri } // Gosod fel prysur
                                inbox.Post ChwilioAmWaith
                            else
                                dadfygio "Ciw yw wag"
                                if (List.length casglwyrRhydd) = casglwyr.Count then
                                    dadfygio "***CIW YN WAG A BOB CASGLIWR YN BAROD***"
                                    inbox.Post WediGorffen
                                else
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
                        dadfygio "Gwneud WediGorffen, gyda statws wediGorffen: %b" wediGorffen
                        wediGorffen <- true
                        (inbox :> IDisposable).Dispose()
                        return ()
                }
            loop 0)
    goruchwyliwr.Post (Ciwio tudalenCychwyn)
    
    while not wediGorffen do
        Thread.Sleep 50
    
    tudalennauWediProfi