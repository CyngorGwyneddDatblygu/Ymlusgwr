module Cofnodi

open System

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
