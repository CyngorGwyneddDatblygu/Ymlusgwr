module Cofnodi

open System
open System.Collections.Generic
open System.IO

type MathCofnod =
    | Dadfygio

let private mathauICofnodi = HashSet<MathCofnod>()

let gosodMathauICofnodi (mathau : seq<MathCofnod>) =
    mathau
    |> Seq.iter (fun math -> mathauICofnodi.Add(math) |> ignore)
   
let cofnodi (mathCofnod : MathCofnod) (neges : Printf.StringFormat<'a, unit>) =
    let amser = DateTime.UtcNow.ToString("yyyy-MM-dd HH:mm:ss")
    let writer =
        if mathauICofnodi.Contains mathCofnod then
            stdout
        else
            new StreamWriter(Stream.Null) :> TextWriter
    
    Printf.kprintf (fprintfn writer "%s [%s] %s"
                            <| amser
                            <| mathCofnod.ToString()
                    ) neges
    
let dadfygio neges =
    (cofnodi MathCofnod.Dadfygio) (neges : Printf.StringFormat<'a, unit>)
