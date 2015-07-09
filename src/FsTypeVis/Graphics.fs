module FsTypeVis.Graphics

type Element_id = string

type Orientation = 
    | Horizontal
    | Vertical

type Color = string

type Attribute = 
    | Bold
    | Fg of Color
    | Bg of Color
    | Darken

type withAttributes<'a> = 'a * Attribute list

type Text = 
    | Plain of string
    | Link of Element_id * string

type Element = 
    | Text of Text withAttributes
    | Stack of (Orientation * Element list) withAttributes
    | Box of (Element_id option * Element) withAttributes
    | Empty

let plain x = Plain x
let link x = Link x
let text x = Text(x, [])
let stack x = Stack(x, [])
let stack_h x = stack (Horizontal,x)
let stack_v x = stack (Vertical,x)
let box x = Box(x, [])
let box_ x = box (None,x)
let empty = Empty

let addAttribute attr = 
    let _addAttribute attr : withAttributes<'a> -> withAttributes<'a> = 
        fun (content, attributes) -> (content, attr :: attributes)
    function 
    | Text t -> Text(_addAttribute attr t)
    | Stack t -> Stack(_addAttribute attr t)
    | Box t -> Box(_addAttribute attr t)
    | Empty -> Empty

let addAttributes attrs element = attrs |> Seq.fold (fun e attr -> addAttribute attr e) element

module Rendering =

    module Html =
        let defaultStyle = 
         """body {font-family:Consolas;}
            a {text-decoration:none;}
            a:link {color:inherit}
            a:active {color:inherit}
            a:visited {color:inherit}
            a:hover {color:inherit}
            div.box {padding:0.2rem;}
            div.box:target {box-shadow: 0px 0px 10px #03f;}
            div.stack-h {float:left;margin-right:0.2rem;}
            div.stack-v {margin-bottom:0.2rem;}
            div.container {}"""

        let template style content =
            sprintf """
            <html>
            <head>
                <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
                <style>%s</style>
            </head>
            <body>
            %s
            </body>""" style content


        let s = function
            | Bg c -> sprintf "background-color:%s" c
            | Fg c -> sprintf "color:%s" c
            | Bold -> sprintf "font-weight:bold"
            | Darken -> sprintf "background-color:rgba(0,0,0,0.2)"

        let h element id attrs cls style content =
            let attrs =
                match attrs with
                | [] -> ""
                | _ -> attrs |> List.map (fun (k,v) -> sprintf "%s='%s'" k v) |> String.concat " "
            let style = 
                match style with
                | [] -> ""
                | _ -> style |> List.map s |> String.concat ";" |> sprintf "style='%s'"
            let id = 
                match id with
                | None -> ""
                | Some id -> sprintf "id='%s'" id
            let cls = 
                match cls with
                | None -> ""
                | Some cls -> sprintf "class='%s'" cls
            sprintf "<%s %s %s %s %s>%s</%s>" element id attrs cls style content element
        let clearfix = "<div style='clear:both'></div>"

        let join = String.concat "\n"

        let rec render = function
            | Text (Plain s,attrs) -> 
                h "span" None [] None attrs s
            | Text (Link (id,s),attrs) -> 
                h "a" None ["href",("#"+id)] None attrs s
            | Stack ((Horizontal,vs),attrs) ->
                h "div" None [] (Some "container") attrs (
                    vs |> List.map render |> List.map (h "div" None [] (Some "stack-h") []) |> join |> fun s -> s+clearfix)
            | Stack ((Vertical,vs),attrs) ->
                h "div" None [] (Some "container") attrs (
                    vs |> List.map render |> List.map (h "div" None [] (Some "stack-v") []) |> join)
            | Box ((id,v),attrs) -> 
                h "div" id [] (Some "box") attrs (render v)
            | Empty -> ""

        open System.IO
        let renderToFile (f:string) vs = 
            use outFile = new StreamWriter(f)
            outFile.WriteLine(template defaultStyle (render (stack_h(vs|> Seq.toList))))