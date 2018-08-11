module Program

open System.Net
open Ymlusgo
open Cofnodi

[<EntryPoint>]
let main argv =
    let url = "https://oriel.madarch.org"
    
    //gosodMathauICofnodi [ Dadfygio ]
   
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
    
    0
