module FsTypeVis.Visualisation
open FsTypeVis
open Simple_type
open Dependencies
open Graphics

module List = 
    let intersperse sep = 
        let rec intersperse' acc = 
            function 
            | [] -> List.rev acc
            | [ x ] -> List.rev (x :: acc)
            | [ x; y ] -> List.rev (y :: sep :: x :: acc)
            | x :: xs -> intersperse' (sep :: x :: acc) xs
        intersperse' []

module Seq = 
    let repeat xs = seq { while true do yield! xs }

module Palettes =
    let whiteText = 
        Seq.repeat [ "#FFFFFF", "#001F3F"
                     "#FFFFFF", "#222222"
                     "#FFFFFF", "#F012BE"
                     "#FFFFFF", "#0074D9"
                     "#FFFFFF", "#3D9970"
                     "#FFFFFF", "#85144B"
                     "#FFFFFF", "#B10DC9"
                     "#001F3F", "#FF851B" ]

let textBold = text >> addAttribute Bold

let nameBox id name content =
    box (None,stack (Vertical,[ textBold (link (id,name))
                                content ]))


let visualiseTypeMap (typeMap:Type_map) palette =
    let lookupType tid = typeMap.[tid]
    let typeFilter = function
        | Sum _ -> true
        | Record _ -> true
        | _ -> false
    let colorFilter = function
        | Sum _ -> true
        | Record _ -> true
        | _ -> false
    let colors = 
        Seq.zip (Map.toSeq typeMap |> Seq.filter (snd >> colorFilter)) palette
        |> Seq.map (fun ((k, _), colors) -> k, colors)
        |> Map.ofSeq
    let addTypeColors tid element = 
        match colors.TryFind tid with
        | Some (fg,bg) -> element |> addAttributes [Fg fg; Bg bg]
        | None -> element

    let rec visType tid = 
        function
        | Sum (cs,tn) -> nameBox tid tn (visSumType cs)
        | Record (fs,tn) -> nameBox tid tn (visRecordType fs)
        | Tuple ts -> visTupleType ts
        | List t -> visListType t
        | Option t -> visOptionType t
        | Set t ->  visSetType t
        | Map (t,t') -> visMapType t t'
        | Function ft -> box_ (visFunctionType ft) |> addAttribute Darken
        | Opaque (tid,tn) -> nameBox tid tn empty |> addTypeColors tid

    and visSumType constrs =
        match constrs with
         | [c] -> (stack_h (List.map visTypeRef c.args))
         | _ -> stack_h (constrs |> List.map visSumTypeConstr)
    and visSumTypeConstr c =
        box_ (stack_v(text (plain c.name) :: (c.args |> List.map visTypeRef))) |> addAttribute Darken
    and visRecordType fields =
        stack_v (fields |> List.map (fun field -> 
                    stack_h [ textBold (plain field.name)
                              visTypeRef field.typ]))
    and visTupleType ts = stack_h (ts |> List.map visTypeRef)
    and visListType t = 
        (stack_h [ visTypeRef t
                   text (plain "list") ]) |> addAttribute Darken
    and visOptionType t = 
        (stack_h [ visTypeRef t
                   text (plain "option") ]) |> addAttribute Darken
    and visSetType t = 
        (stack_h [ visTypeRef t
                   text (plain "set") ]) |> addAttribute Darken
    and visMapType t t' = 
        (stack_h [ visTypeRef t
                   visTypeRef t'
                   text (plain "map") ]) |> addAttribute Darken
    and visFunctionType = 
        function
        | Const t -> visTypeRef t
        | Fun ts -> 
            stack_h (ts
                        |> List.map visFunctionType
                        |> List.intersperse (text (plain "→")))
    and visTypeRef tid =
        let t = lookupType tid 
        match t with
        | Sum (_,tn) | Record (_,tn) | Opaque (_,tn) ->
            box (None,text (link (tid,tn))) |> addTypeColors tid
        | _ -> visType tid t |> addTypeColors tid

    typeMap
    |> Map.filter (fun _ -> typeFilter)
    |> Map.map (fun tid t -> box (Some tid, visType tid t) |> addTypeColors tid)

let visualise (t:System.Type) = 
    let typeMap = Simple_type.Make.typeMap t
    visualiseTypeMap typeMap

let visualiseLayered (t:System.Type) palette = 
    let typeMap = Simple_type.Make.typeMap t
    let visMap = visualiseTypeMap typeMap palette
    let depMap = Dependencies.depMap typeMap Dependencies.Forward
    let depRoots = typeMap |> Map.toSeq |> Seq.map fst |> Seq.toList
    let depLayers = Dependencies.dependencyLayers depMap depRoots
    depLayers
    |> Seq.map (fun tids -> 
           tids
           |> Seq.choose visMap.TryFind
           |> Seq.toList
           |> stack_h)
    |> Seq.toList
    |> stack_v 


