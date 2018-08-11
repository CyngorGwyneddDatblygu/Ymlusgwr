module Program

open System.Net
open Ymlusgo
open Cofnodi

[<EntryPoint>]
let main argv =
    let url = "https://oriel.madarch.org"
   
    let tudalennau = ymlusgo url
    
    let cysylltauWediTorri =
        [
            for kv in tudalennau do
                match kv.Value.Statws with
                | Some statws when statws = HttpStatusCode.OK -> ()
                | _ -> yield kv.Value.UriTudalen
        ]
        |> List.map (fun uri ->
                uri.ToString(),
                [
                    for kv in tudalennau do
                        if kv.Value.Cysylltau |> List.contains uri then
                            yield kv.Key.ToString()
                ]
            )
        |> Map.ofList
    
    cysylltauWediTorri
    |> Map.iter (fun cyswlltWediTorri tudalennauGydarCyswllt ->
        printfn "%s" cyswlltWediTorri
        tudalennauGydarCyswllt |> List.iter(fun tud -> printfn "\t%s" tud))    
    
    0
