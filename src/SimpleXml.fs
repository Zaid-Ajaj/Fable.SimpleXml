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

    /// Recursively find elements that match a predicate
    let findElementsBy (pred: XmlElement -> bool) (node: XmlElement) = 
        let rec findElements (root: XmlElement) = 
            [ if pred root then yield root
              for child in root.Children do
                yield! findElements child  ] 
        findElements node


    /// Recursively find elements that have a given tag name
    let findElementsByName (tagName: string) (node: XmlElement) =  
        findElementsBy (fun el -> el.Name = tagName) node

    /// Recursively find elements that the exact given set of attributes
    let findElementsByExactAttributes (attrs: Map<string, string>) (node: XmlElement)=
        findElementsBy (fun el -> el.Attributes = attrs) node

    /// Recursively find elements that contain a given attribute
    let findElementsByAttribute key value node =
        node
        |> findElementsBy (fun el -> 
            match Map.tryFind key el.Attributes with 
            | Some attributeValue when value = attributeValue -> true 
            | _ -> false) 

    /// Recursively try to find an element that has a given tag name and return the first matching element if any 
    let tryFindElementByName tagName root = 
        findElementsByName tagName root 
        |> List.tryHead

    let findElementByName tagName root = 
        match tryFindElementByName tagName root with
        | Some node -> node 
        | None -> failwithf "Could not find element with name '%s' inside '%s'" tagName root.Name