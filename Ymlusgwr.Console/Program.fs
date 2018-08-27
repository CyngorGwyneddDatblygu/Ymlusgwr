module Program

open Allbynnu
open System.Diagnostics
open System.Net
open CommandLine
open Ymlusgo
open Cofnodi

type Options = {
    [<Value(0, MetaName = "url", HelpText = "Y URL i ymlusgo", Required = true)>]
    Url : string
    
    [<Option("allbynnu-toredig-html")>]
    AllbynnuToredigHtml : string
}

type CanlyniadRhedeg =
    | Llwyddiant = 0
    | Methiant = 1


let rhedeg dewisiadau = 
    let url = dewisiadau.Url

    let stopwatch = Stopwatch.StartNew()
    
    printfn "Ymlusgo %s" url
   
    let tudalennau = ymlusgo url
    
    if dewisiadau.AllbynnuToredigHtml <> null then
        nôlCysylltauWediTorri tudalennau
        |> toredigIHtml dewisiadau.AllbynnuToredigHtml url
            
    stopwatch.Stop()
    printfn "Wedi gorffen am %s, cymerwyd %d eiliad." (System.DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")) (stopwatch.ElapsedMilliseconds/(int64 1000))
    CanlyniadRhedeg.Llwyddiant

let methu gwallau =
    //printfn "Methu: Errors = %A" errors
    CanlyniadRhedeg.Methiant

[<EntryPoint>]
let main argv =
    //let url = "https://oriel.madarch.org"
    
    gosodMathauICofnodi [ Dadfygio ]
    
    let result = CommandLine.Parser.Default.ParseArguments<Options>(argv)
    let canlyniadRheged =
        match result with
        | :? Parsed<Options> as parsed -> rhedeg parsed.Value
        | :? NotParsed<Options> as notParsed -> methu notParsed.Errors
        | _ -> failwith "Gwall wrth dosrannu paramedrau"

    int canlyniadRheged