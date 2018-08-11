module Cymhorthwyr

open System
open System.IO
open System.Net
open System.Text.RegularExpressions
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