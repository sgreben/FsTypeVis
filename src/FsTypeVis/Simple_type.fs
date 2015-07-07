module FsTypeVis.Simple_type

open Microsoft.FSharp.Reflection

type Type_name = string
type Type_id = string
type Field_name = string
type Constructor_name = string

type Constructor<'T> = {
    name:Constructor_name
    args:'T list
}

type Record_field<'T> = {
    name:Field_name
    typ:'T
}

type Function_type<'T> = 
    | Const of 'T
    | Fun of Function_type<'T> list

type Named_type<'A> = 'A * Type_name

type Simple_type<'T> = 
    | Sum of Named_type<Constructor<'T> list>
    | Record of Named_type<Record_field<'T> list>
    | Function of Function_type<'T>
    | Tuple of 'T list
    | Option of 'T
    | List of 'T
    | Set of 'T
    | Map of 'T * 'T
    | Opaque of Named_type<'T>

let rec map f = function
    | Sum (cs,tn) -> Sum (cs |> List.map (fun c -> {name=c.name; args = List.map f c.args}),tn)
    | Record (fs,tn) -> Record (fs |> List.map (fun rf -> {name=rf.name; typ = f rf.typ}),tn)
    | Tuple ts -> Tuple (List.map f ts)
    | List t -> List (f t)
    | Option t -> Option (f t)
    | Set t -> Set (f t)
    | Map (t,t') -> Map (f t,f t')
    | Function t ->
        let rec mapFunctionType = function
            | Const t -> Const (f t)
            | Fun ts -> Fun (List.map mapFunctionType ts)
        Function (mapFunctionType t)
    | Opaque (t,tn) -> Opaque(f t, tn)

let rec fold f init = function
    | Sum (cs,_) -> List.fold (fun acc c -> List.fold f acc c.args) init cs 
    | Record (fs,_) -> List.fold (fun acc rf -> f acc rf.typ) init fs 
    | Tuple ts -> List.fold f init ts
    | List t | Option t | Set t -> f init t
    | Map (t,t') -> f (f init t) t'
    | Function t ->
        let rec foldFunctionType acc = function
            | Const t -> f acc t
            | Fun ts -> List.fold foldFunctionType acc ts
        foldFunctionType init t
    | Opaque (t,_) -> f init t

let isList(t : System.Type) = t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<list<_>>
let isSet(t : System.Type) = t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Set<_>>
let isMap(t : System.Type) = t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Map<_, _>>
let isOption(t : System.Type) = t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

module Make = 
    module Array = 
        let first (a : 'a array) = a.[0]
    
    let _targs (t : System.Type) = t.GetGenericArguments()
    let _targ t =  t |> _targs |> Array.first
    let _tid (t : System.Type) = t.FullName // prettier than t.AssemblyQualifiedName, but not unique!
    let _tname (t : System.Type) = t.Name
        
    let ofSystemType' t = 
        let mapType t = 
            let targs = _targs t
            targs.[0], targs.[1]
    
        let tupleType t = 
            FSharpType.GetTupleElements t
            |> Array.toList
    
        let recordType t = 
            FSharpType.GetRecordFields t
            |> Array.map (fun p -> {name=p.Name;typ=p.PropertyType})
            |> Array.toList
    
        let rec functionType t = 
            let tl, tr = FSharpType.GetFunctionElements t
            match functionType' tr with
            | Const tr -> 
                Fun([ functionType' tl
                      Const tr ])
            | Fun tr -> Fun(functionType' tl :: tr)
        and functionType' t = 
            if FSharpType.IsFunction t then functionType t
            else Const t
            
        let sumType t = 
            FSharpType.GetUnionCases t
            |> Seq.map (fun uc -> 
                {name=uc.Name;
                 args=
                    uc.GetFields()
                    |> Seq.map (fun t -> t.PropertyType)
                    |> Seq.toList})
            |> Seq.toList

        if isOption t then Option(_targ t)
        else if isList t then List(_targ t)
        else if isSet t then Set(_targ t)
        else if isMap t then Map(mapType t)
        else if FSharpType.IsTuple t then Tuple(tupleType t)
        else if FSharpType.IsFunction t then Function(functionType t)
        else if FSharpType.IsRecord t then Record(recordType t,_tname t)
        else if FSharpType.IsUnion t then Sum(sumType t,_tname t)
        else Opaque(t,_tname t)

    let typeMap ts =
        let rec typeMap init t = 
            if Map.containsKey (_tid t) init then init 
            else let init = Map.add (_tid t) t init
                 ofSystemType' t |> fold (fun m t -> typeMap m t) init
        let env = ts |> Seq.fold typeMap Map.empty
        let env = env |> Map.map (fun _ t -> ofSystemType' t |> map _tid)
        env