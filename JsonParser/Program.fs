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
let j1 =
    "{
        \"something\": \"a string\",
        \"numeric\": 3,
        \"nothing\": null,
        \"a list\": [1, 2, \"hi\"],
        \"an object\": { \"hi\": \"hello\"}
    }"
    
let j2 = "{ \"hi\": \"hello\"}"
let nj1 = "{ \"hi\": \"hello\" "
let nj2 = "{ \"hi\": \"hello }"

let betweenBrackets = between (pstring "{") (pstring "}")
let betweenQuotes = (fun p -> (between (pstring "\"") (pstring "\"") p) <|> (between (pstring "'") (pstring "'")) p)

let jnull : Parser<Json, unit> = stringReturn "null" JNull
let jtrue : Parser<Json, unit> = stringReturn "true" (JBool true)
let jfalse : Parser<Json, unit> = stringReturn "false" (JBool false)
let jnumber : Parser<Json, unit> = pfloat |>> JNumber
let stringLiteral =
    let escape = anyOf "\"\\/bfnrt"
                |>> function
                    | 'b' -> "\b"
                    | 'f' -> "\u000C"
                    | 'n' -> "\n"
                    | 'r' -> "\r"
                    | 't' -> "\t"
                    | c -> string c
    let unicodeEscape =
        let hex2int c = (int c &&& 15) + (int c >>> 6)*9
        
        pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )
    let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')
    
    between (pstring "\"") (pstring "\"") (stringsSepBy normalCharSnippet escapedCharSnippet)

let jstring : Parser<Json, unit> = stringLiteral |>> JString

let jvalue, jvalueRef = createParserForwardedToRef<Json, unit>()

let listBetweenStrings sOpen sClose pElement f =
    between (pstring sOpen)
            (pstring sClose)
            (spaces >>. sepBy (pElement .>> spaces) (pstring "," >>. spaces) |>> f)

let jlist = listBetweenStrings "[" "]" jvalue JList

let keyValue = stringLiteral .>>. (spaces >>. pstring ":" >>. spaces >>. jvalue)

let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)

do jvalueRef := choice [
    jobject
    jlist
    jstring
    jnumber
    jtrue
    jfalse
    jnull
]

let json = spaces >>. jvalue .>> spaces .>> eof

let pJObject : Parser<Json, unit> = betweenBrackets
                                        (pipe3
                                             (spaces >>. (betweenQuotes (manySatisfy (fun x -> x <> '}'))))
                                             (spaces >>. (pstring ":"))
                                             (spaces >>. (manySatisfy (fun x -> x <> '}')))
                                             (fun key _ value -> JString key)
                                              )
let parseJSON = json

[<EntryPoint>]
let main argv =
    printfn "Hello world %s" "hi"
    [j1; j2; nj1; nj2]
    |> List.iter (fun s ->
    run parseJSON s
    |> (fun res ->
        match res with
        | Success (JString key, _, _) -> printfn "Success: %s" key
        | Success _ -> printfn "Success"
        | Failure (e, _, _) -> printfn "Oops: %s" e
        )
    )
    0 // return an integer exit code