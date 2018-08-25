module Program

open System.Diagnostics
open System.Net
open CommandLine
open Ymlusgo
open Cofnodi

type Options = {
    [<Value(0, MetaName = "url", HelpText = "Y URL i ymlusgo", Required = true)>]
    Url : string
}

type CanlyniadRhedeg =
    | Llwyddiant = 0
    | Methiant = 1


let rhedeg dewisiadau = 
    let url = dewisiadau.Url

    let stopwatch = Stopwatch.StartNew()
    
    printfn "Ymlusgo %s" url
   
    let tudalennau = ymlusgo url
    
    let cysylltauWediTorri =
        [
            for kv in tudalennau do
                match kv.Value.Statws with
                | Some statws when statws = HttpStatusCode.OK -> ()
                | _ -> yield kv.Value.UriTudalen, kv.Value.Statws
        ]
        |> List.map (fun (uri, statws) ->
                uri.ToString(),
                (
                    (match statws with
                    | Some httpStatusCode -> (httpStatusCode.ToString())
                    | _ -> ""),
    
                    [
                        for kv in tudalennau do
                            if kv.Value.Cysylltau |> List.contains uri then
                                yield kv.Key.ToString()
                    ]
                )
            )
        |> Map.ofList
    
    cysylltauWediTorri
    |> Map.iter (fun cyswlltWediTorri (statws, tudalennauGydarCyswllt) ->
        printfn "%s\t%s" cyswlltWediTorri statws
        tudalennauGydarCyswllt |> List.iter(fun tud -> printfn "\t%s" tud))    

    stopwatch.Stop()
    printfn "Wedi gorffen am %s, cymerwyd %d eiliad." (System.DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")) (stopwatch.ElapsedMilliseconds/(int64 1000))
    CanlyniadRhedeg.Llwyddiant

let methu gwallau =
    //printfn "Methu: Errors = %A" errors
    CanlyniadRhedeg.Methiant

[<EntryPoint>]
let main argv =
    //let url = "https://oriel.madarch.org"
    
    //gosodMathauICofnodi [ Dadfygio ]
    
    let result = CommandLine.Parser.Default.ParseArguments<Options>(argv)
    let canlyniadRheged =
        match result with
        | :? Parsed<Options> as parsed -> rhedeg parsed.Value
        | :? NotParsed<Options> as notParsed -> methu notParsed.Errors
        | _ -> failwith "Gwall wrth dosrannu paramedrau"

    int canlyniadRheged