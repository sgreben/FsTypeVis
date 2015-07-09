module FsTypeVis.Dependencies
open FsTypeVis.Simple_type

type Direction = Forward | Reverse
let depMap (typeMap:Type_map) direction =
    let lookup tid = typeMap.TryFind tid
    let depends_on m tid tid' = 
        let tid,tid' = 
            match direction with
            | Forward -> tid',tid
            | Reverse -> tid,tid'
        let deps = 
            match Map.tryFind tid' m with
            | Some deps -> Set.add tid deps
            | None -> Set.singleton tid
        Map.add tid' deps m

    let typeDeps tid m = 
        match lookup tid with
        | Some t -> fold (fun m tid' -> depends_on m tid tid') m t
        | None -> m
    let depMap = typeMap |> Map.map (fun _ _ -> Set.empty)
    typeMap |> Map.toSeq |> Seq.fold (fun m (tid,_) -> typeDeps tid m) depMap

let dependencies (depMap:Map<Type_id,Set<Type_id>>) tid = 
    match depMap.TryFind tid with
    | Some s -> s
    | None -> Set.empty

let dependencyLayers depMap roots = 
    let deps = dependencies depMap
    
    let rec loop acc = 
        function 
        | [] -> acc
        | (l, tid) :: wl -> 
            let l = l+1
            let acc, wl = 
                deps tid
                |> Set.toSeq
                |> Seq.fold (fun (acc, wl) tid' -> 
                       let l' = 
                           match Map.tryFind tid' acc with
                           | Some l' -> max l l'
                           | None -> l
                       
                       let wl = 
                           if Map.containsKey tid' acc then wl
                           else (l', tid') :: wl
                       
                       let acc = Map.add tid' l' acc
                       (acc, wl)) (acc, wl)
            loop acc wl
    roots
    |> List.map (fun r -> (0, r))
    |> loop Map.empty
    |> Map.toSeq
    |> Seq.map (fun (tid, l) -> l, tid)
    |> Seq.groupBy fst
    |> Seq.sortBy fst
    |> Seq.map (snd >> Seq.map snd)
