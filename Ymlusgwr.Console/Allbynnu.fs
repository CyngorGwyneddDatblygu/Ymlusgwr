module Allbynnu

open System
open System.Collections.Generic
open System.IO
open System.Net
open Ymlusgo


let nôlCysylltauWediTorri (tudalennau : IDictionary<Uri, CanlyniadTudalen>) =
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

let toredigIHtml (enwFfeil : string) (urlCychwyn : string) (cysylltauWediTorri : Map<string,(string*string list)>) =
    
    let llinellauHtml =
        cysylltauWediTorri
        |> Map.fold (fun str tudalenToredig (statws, tudalennauSynCynnwys) ->
            let rhestrCynhwysyddion =
                tudalennauSynCynnwys
                |> List.fold (fun str url -> str + sprintf "<li>%s</li>" url) ""
                |> sprintf "<ul>%s</ul"

            str + sprintf "<tr><td class=\".urlToredig\">%s</td><td class=\"statws\">%s</td><td class=\"nifer\">%d</td><td class=\"cynhwysyddion\">%s</td></tr>"
                            tudalenToredig statws (tudalennauSynCynnwys |> List.length) rhestrCynhwysyddion) ""

    let amser = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")

    let html =
        sprintf """<!DOCTYPE html />
<html>
  <head>
    <meta charset="utf-8" />
    <title>Cysylltau wedi torri i %s ar %s</title>
  </head>
  <body>
    <h1>Cysylltau wedi torri i %s ar %s</h1>
    <table>
      <thead>
        <tr>
          <th class="urlToredig">URL Toredig</th>
          <th class="statws">Statws</th>
          <th class="nifer" title="Nifer o dudalennau sy'n cynnwys y URL sydd wedi torri">Nifer</th>
          <th class="cynhwysyddion" title="Y tudalennau sy'n cynnwys y URL sydd wedi torri">Cynhwysyddion</th>
        </tr>
      </thead>
      <tbody>
%s
      </tbody>
    </table>
  </body>
</html>""" urlCychwyn amser urlCychwyn amser llinellauHtml

    File.WriteAllText(enwFfeil, html)    
    ()

//        cysylltauWediTorri
//        |> Map.iter (fun cyswlltWediTorri (statws, tudalennauGydarCyswllt) ->
//        printfn "%s\t%s" cyswlltWediTorri statws
//        tudalennauGydarCyswllt |> List.iter(fun tud -> printfn "\t%s" tud))   