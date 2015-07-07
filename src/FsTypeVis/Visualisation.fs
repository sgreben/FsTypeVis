module FsTypeVis.Visualisation

open Simple_type
open Graphics.Structure

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
    let bassCss = 
        Seq.repeat [ "#001F3F", "#DDDDDD"
                     "#001F3F", "#F012BE"
                     "#001F3F", "#FF4136"
                     "#001F3F", "#FF851B"
                     "#001F3F", "#FFDC00"
                     "#001F3F", "#01FF70"
                     "#001F3F", "#2ECC40"
                     "#111111", "#0074D9"
                     "#111111", "#01FF70"
                     "#111111", "#2ECC40"
                     "#111111", "#39CCCC"
                     "#111111", "#3D9970"
                     "#111111", "#7FDBFF"
                     "#111111", "#AAAAAA"
                     "#111111", "#B10DC9"
                     "#111111", "#DDDDDD"
                     "#111111", "#F012BE"
                     "#111111", "#FF4136"
                     "#111111", "#FF851B"
                     "#111111", "#FFDC00"
                     "#001F3F", "#39CCCC"
                     "#001F3F", "#3D9970"
                     "#001F3F", "#7FDBFF"
                     "#001F3F", "#AAAAAA"
                     "#0074D9", "#001F3F"
                     "#0074D9", "#01FF70"
                     "#0074D9", "#111111"
                     "#0074D9", "#DDDDDD"
                     "#0074D9", "#FFDC00"
                     "#01FF70", "#001F3F"
                     "#01FF70", "#0074D9"
                     "#01FF70", "#111111"
                     "#01FF70", "#85144B"
                     "#01FF70", "#B10DC9"
                     "#2ECC40", "#001F3F"
                     "#2ECC40", "#111111"
                     "#2ECC40", "#85144B"
                     "#39CCCC", "#001F3F"
                     "#39CCCC", "#111111"
                     "#39CCCC", "#85144B"
                     "#3D9970", "#001F3F"
                     "#3D9970", "#111111"
                     "#7FDBFF", "#001F3F"
                     "#7FDBFF", "#111111"
                     "#7FDBFF", "#85144B"
                     "#7FDBFF", "#B10DC9"
                     "#85144B", "#01FF70"
                     "#85144B", "#2ECC40"
                     "#85144B", "#39CCCC"
                     "#85144B", "#7FDBFF"
                     "#85144B", "#AAAAAA"
                     "#85144B", "#DDDDDD"
                     "#85144B", "#FF851B"
                     "#85144B", "#FFDC00"
                     "#AAAAAA", "#001F3F"
                     "#AAAAAA", "#111111"
                     "#AAAAAA", "#85144B"
                     "#B10DC9", "#01FF70"
                     "#B10DC9", "#111111"
                     "#B10DC9", "#7FDBFF"
                     "#B10DC9", "#DDDDDD"
                     "#B10DC9", "#FFDC00"
                     "#DDDDDD", "#001F3F"
                     "#DDDDDD", "#0074D9"
                     "#DDDDDD", "#111111"
                     "#DDDDDD", "#85144B"
                     "#DDDDDD", "#B10DC9"
                     "#F012BE", "#001F3F"
                     "#F012BE", "#111111"
                     "#FF4136", "#001F3F"
                     "#FF4136", "#111111"
                     "#FF851B", "#001F3F"
                     "#FF851B", "#111111"
                     "#FF851B", "#85144B"
                     "#FFDC00", "#001F3F"
                     "#FFDC00", "#0074D9"
                     "#FFDC00", "#111111"
                     "#FFDC00", "#85144B"
                     "#FFDC00", "#B10DC9"
                     "#FFFFFF", "#001F3F"
                     "#FFFFFF", "#0074D9"
                     "#FFFFFF", "#111111"
                     "#FFFFFF", "#3D9970"
                     "#FFFFFF", "#85144B"
                     "#FFFFFF", "#B10DC9"
                     "#FFFFFF", "#F012BE"
                     "#FFFFFF", "#FF4136" ]

type Environment = {
    types : Map<Type_id,Simple_type<Type_id>>
    colors : Map<Type_id,Color*Color>
}

let textBold = text >> addAttribute Bold

let nameBox name content =
    box (None,stack (Vertical,[ textBold (plain name)
                                content ]))


let visTypeMap typeMap pallette = 
    let typeFilter = function
        | Sum _ -> true
        | Record _ -> true
        | _ -> false
    let colors = 
        Seq.zip (Map.toSeq typeMap) pallette
        |> Seq.map (fun ((k, _), colors) -> k, colors)
        |> Map.ofSeq
    
    let env = 
        { types = typeMap
          colors = colors }
    
    let lookupType tid = env.types.[tid]

    let addTypeColors tid element = 
        match env.colors.TryFind tid with
        | Some (fg,bg) -> element |> addAttribute (Fg fg) |> addAttribute (Bg bg)
        | None -> element

    let rec visType = 
        function
        | Sum (cs,tn) -> nameBox tn (visSumType cs)
        | Record (fs,tn) -> nameBox tn (visRecordType fs)
        | Tuple ts -> visTupleType ts
        | List t -> visListType t
        | Option t -> visOptionType t
        | Set t ->  visSetType t
        | Map (t,t') -> visMapType t t'
        | Function ft -> box_ (visFunctionType ft) |> addAttribute Darken
        | Opaque (tid,tn) -> nameBox tn empty |> addTypeColors tid

    and visSumType constrs =
        match constrs with
         | [c] -> box_ (stack_h (List.map visTypeRef c.args))
         | _ -> stack_h (constrs |> List.map visSumTypeConstr)
    and visSumTypeConstr c =
        box_ (stack_v(text (plain c.name) :: (c.args |> List.map visTypeRef))) |> addAttribute Darken
    and visRecordType fields =
        stack_v (fields |> List.map (fun field -> 
                    stack_h [ textBold (plain field.name)
                              visTypeRef field.typ]))
    and visTupleType ts = stack_h (ts |> List.map visTypeRef)
    and visListType t = 
        box_ (stack_h [ visTypeRef t
                        text (plain "list") ]) |> addAttribute Darken
    and visOptionType t = 
        box_ (stack_h [ visTypeRef t
                        text (plain "option") ]) |> addAttribute Darken
    and visSetType t = 
        box_ (stack_h [ visTypeRef t
                        text (plain "set") ]) |> addAttribute Darken
    and visMapType t t' = 
        box_ (stack_h [ visTypeRef t
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
            box (Some tid,text (link (tid,tn))) |> addTypeColors tid
        | _ -> visType t |> addTypeColors tid

    env.types
    |> Map.filter (fun _ -> typeFilter)
    |> Map.map (fun tid t -> box (Some tid, visType t) |> addTypeColors tid)