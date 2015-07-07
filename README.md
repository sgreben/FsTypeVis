# FsTypeVis

## Status

Just started. Some things work.

## What

Simple visualisation of F# types. 

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
