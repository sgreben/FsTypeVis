# FsTypeVis

## Status

Just started. Some things work.

## What

Simple visualisation of F# types. Converts this
```fsharp
    type Customer = 
        { name : string
          location : string }

    type NormalOrder = 
        { date : System.DateTime
          number : string
          customer : Customer }

    type SpecialOrder = 
        { date : System.DateTime
          number : string
          customer : Customer }

    type Order = 
        | Normal of NormalOrder
        | Special of SpecialOrder

    type Close = Close of (Order -> Order)
    type Dispatch = Dispatch of ((Order -> Order) -> Order)
    type Receive = Receive of (SpecialOrder -> SpecialOrder)
``` 

...into this:

![Output sample](/docs/files/img/screenshot.png?raw=true)

## Why

Inspired by a comment on this [post](http://fsharpforfunandprofit.com/posts/no-uml-diagrams/).

> As a developer, I prefer writing specifications in code like you detail. 
> But a picture really is important for sharing and design. How is F#'s reflection ability? 
> Could you automatically generate diagrams from your code? Maybe using graphviz?

## How

The tool

- Builds a simple representation of an F# type and its dependencies.
- Converts this representation to an intermediate graphics language.
- Provides a crappy HTML renderer for the graphics language.
