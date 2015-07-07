namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FsTypeVis")>]
[<assembly: AssemblyProductAttribute("FsTypeVis")>]
[<assembly: AssemblyDescriptionAttribute("Simple visualisation of F# types")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"
