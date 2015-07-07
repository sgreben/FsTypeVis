(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/FsTypeVis"

(**
Tutorial
========

Let's define some types to visualise. We'll use the customer/order model from [here](domain model from http://fsharpforfunandprofit.com/posts/no-uml-diagrams/).
*)
#r "FsTypeVis.dll"
open System
open FsTypeVis

// 
type Customer = {name:string; location:string}
type NormalOrder = {date: DateTime; number: string; customer: Customer}
type SpecialOrder = {date: DateTime; number: string; customer: Customer}
type Order = 
    | Normal of NormalOrder
    | Special of SpecialOrder 
type Confirm =  Confirm of (Order -> Order)
type Close =  Close of (Order -> Order)
type Dispatch =  Dispatch of (Order -> Order)
type Receive = Receive of (SpecialOrder -> SpecialOrder)

(**

Generating a type map:

*)

let typeMap = Simple_type.Make.typeMap [ typeof<Dispatch>
                                         typeof<Receive>
                                         typeof<Confirm>
                                         typeof<Close> ]
(**

Compile to an intermediate graphics/layout language: 
    
*)

let colors = Visualisation.Palettes.bassCss
let visMap = Visualisation.visTypeMap typeMap colors

(**

Render as HTML:   

*)

visMap 
    |> Map.toSeq 
    |> Seq.map snd 
    |> Seq.map Graphics.Rendering.Html.render