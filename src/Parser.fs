namespace Fable.SimpleXml

open Fable.Parsimmon

module Tuple = 
    let concat (a, b) = String.concat "" [a; b]

module Parser = 

    open AST
    let withWhitespace p = 
        Parsimmon.between (Parsimmon.optionalWhitespace) (Parsimmon.optionalWhitespace) p


    let asciiString =

      let letters = 
        [32 .. 126]
        |> List.map (char >> string)
        |> String.concat ""

      Parsimmon.oneOf letters
      |> Parsimmon.many
      |> Parsimmon.concat

    let escapedString =
        let escape =  
            Parsimmon.oneOf "\'\\/bfnrt"
            |> Parsimmon.map(function
                | "b" -> "\b"
                | "f" -> "\u000C"
                | "n" -> "\n"
                | "r" -> "\r"
                | "t" -> "\t"
                | c   -> c) // every other char is mapped to itself

        let escapedCharSnippet = 
            Parsimmon.str "\\"
            |> Parsimmon.chain escape

        let normalCharSnippet = 
            Parsimmon.str "\""
            |> Parsimmon.bind (fun _ -> 
                Parsimmon.takeWhile (fun c -> c <> "\"")
                |> Parsimmon.seperateBy escapedCharSnippet
                |> Parsimmon.concat
            )
            |> Parsimmon.skip (Parsimmon.str "\"")


        normalCharSnippet

    let escapedStringTick =
        let escape =  
            Parsimmon.oneOf "\"\\/bfnrt"
            |> Parsimmon.map(function
                | "b" -> "\b"
                | "f" -> "\u000C"
                | "n" -> "\n"
                | "r" -> "\r"
                | "t" -> "\t"
                | c   -> c) // every other char is mapped to itself

        let escapedCharSnippet = 
            Parsimmon.str "\\"
            |> Parsimmon.chain escape

        let normalCharSnippet = 
            Parsimmon.str "\'"
            |> Parsimmon.bind (fun _ -> 
                Parsimmon.takeWhile (fun c -> c <> "\'")
                |> Parsimmon.seperateBy escapedCharSnippet
                |> Parsimmon.concat
            )
            |> Parsimmon.skip (Parsimmon.str "\'")


        normalCharSnippet

    let attributKey = 
       [ Parsimmon.letter
         Parsimmon.str "-"
         Parsimmon.str "_" ]
       |> Parsimmon.choose 
       |> Parsimmon.many 
       |> Parsimmon.concat

    let integer = 
        Parsimmon.digit
        |> Parsimmon.many
        |> Parsimmon.concat

    let letters = 
        Parsimmon.letter
        |> Parsimmon.many
        |> Parsimmon.concat

    let identifier = 
        Parsimmon.seq2 
            letters 
            integer 
        |> Parsimmon.map Tuple.concat
    let attributeValue = 
        [ escapedStringTick
          escapedString
          Parsimmon.str "true" 
          Parsimmon.str "false"
          integer ]
        |> Parsimmon.choose

    let attribute = 
        Parsimmon.seq3 
            attributKey
            (Parsimmon.str "=")
            attributeValue
        |> Parsimmon.map (fun (key,_,value) -> (key, value))

    let manyAttributes = 
        attribute
        |> Parsimmon.seperateBy (Parsimmon.str " ")
        |> withWhitespace
        |> Parsimmon.map List.ofArray
       
    let simpleTag = Parsimmon.choose [ identifier; letters  ]
    
    let tagWithoutNamespace : IParser<string option * string> = 
        simpleTag
        |> Parsimmon.map (fun tag -> None, tag)

    let tagWithNamespace = 
        Parsimmon.seq3
            simpleTag
            (Parsimmon.str ":")
            simpleTag 
        |> Parsimmon.map (fun (ns, colon, name) -> (Some ns,name))

    let tagName = Parsimmon.choose [tagWithNamespace; tagWithoutNamespace]
    
    let openingTagName = 
        Parsimmon.str "<"
        |> Parsimmon.chain tagName

    let declaration = 
        Parsimmon.seq3
            (Parsimmon.str "<?xml")
            (withWhitespace manyAttributes)
            (Parsimmon.str "?>")
        |> Parsimmon.map (fun (_, attrs, _) -> Map.ofList attrs)
        
    let selfClosingTag = 
        Parsimmon.seq4
            openingTagName
            Parsimmon.whitespace
            manyAttributes
            (Parsimmon.optionalWhitespace |> Parsimmon.chain (Parsimmon.str "/>"))
        |> Parsimmon.map (fun (tagName,_, attrs, _) -> tagName, attrs)
        
    let comment = 
        let content = 
            letters
            |> Parsimmon.seperateBy (Parsimmon.str " ")
            |> Parsimmon.map (String.concat " ")

        Parsimmon.seq3
            (withWhitespace (Parsimmon.str "<!--"))
            (content)
            (withWhitespace (Parsimmon.str "-->"))
        |> Parsimmon.map (fun (_, body, _) -> body)

    
    let nodeOpening = 
        Parsimmon.seq3
          (withWhitespace openingTagName)
          (withWhitespace manyAttributes)
          (withWhitespace (Parsimmon.str ">"))
        |> Parsimmon.map (fun (tag, attrs, _) -> tag, attrs)

    let nodeClosing ns tagName =
        let matchingTag = 
            match ns with
            | Some ns' -> sprintf "%s:%s" ns' tagName
            | None -> tagName 
        Parsimmon.seq3 
            (Parsimmon.str "</")
            (withWhitespace (Parsimmon.str matchingTag))
            (withWhitespace (Parsimmon.str ">")) 
        |> Parsimmon.map (fun _ -> ns, tagName)


    let emptyNode = 
        nodeOpening
        |> Parsimmon.bind (fun ((ns, tagName), attrs) -> 
            withWhitespace (nodeClosing ns tagName)
            |> Parsimmon.map (fun _ -> (ns, tagName), attrs))
        |> withWhitespace