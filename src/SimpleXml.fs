namespace Fable.SimpleXml

open Fable.Parsimmon 

module SimpleXml = 
    let tryParseElement (input: string) = 
        Parsimmon.parse input Parser.xmlElement 

    let parseElement (input: string) = 
        match tryParseElement input with
        | Some xml -> xml
        | None -> failwithf "Could not parse XML input as an element: %s" input


    let tryParseDocument (input: string) = 
        Parsimmon.parse input Parser.xmlDocument

    let parseDocument (input: string) = 
        match tryParseDocument input with 
        | Some document -> document 
        | None ->  failwithf "Could not parse XML input as a document: %s" input