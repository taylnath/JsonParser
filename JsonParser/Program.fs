// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open FParsec

type Json =
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JList of Json list
    | JObject of Map<string, Json>
let testJSON =
    "{
        'something': 'a string',
        'numeric': 3,
        'nothing': null,
        'a list': [1, 2, 'hi'],
        'an object': { 'hi': 'hello'}
    }"
    
let notJSON = "{ 'hi': 'hello' "

[<EntryPoint>]
let main argv =
    printfn "Hello world %s" "hi"
    0 // return an integer exit code