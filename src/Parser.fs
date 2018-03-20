namespace Fable.SimpleXml

open Fable.Parsimmon
open System

module Tuple =
    let concat (a, b) = String.concat "" [a; b]

#nowarn "40"

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
         Parsimmon.str ":"
         Parsimmon.str "_" ]
       |> Parsimmon.choose
       |> Parsimmon.many
       |> Parsimmon.concat

    let integer =
        Parsimmon.digit
        |> Parsimmon.many
        |> Parsimmon.concat

    let letters =
        let acceptableChars =
            ['a' .. 'z']
            |> List.append ['A' .. 'Z']
            |> List.map string
            |> String.concat ""
            |> (+) "_-"
            |> Seq.toList
            |> List.map string

        Parsimmon.satisfy (fun token -> List.contains token acceptableChars)
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
        |> withWhitespace

    let selfClosingTag =
        Parsimmon.seq3
            openingTagName
            (withWhitespace manyAttributes)
            (Parsimmon.optionalWhitespace |> Parsimmon.chain (Parsimmon.str "/>"))
        |> Parsimmon.map (fun (tagName, attrs, _) -> tagName, attrs)

    let textSnippet =
        Parsimmon.satisfy (fun token -> token <> "<" && token <> ">")
        |> Parsimmon.atLeastOneOrMany
        |> Parsimmon.concat

    let comment = 
        let emptyComment = 
            Parsimmon.str "<!---->"
            |> Parsimmon.map (fun _ -> "")

        let commentWithWhitespace = 
            Parsimmon.seq3 
                (Parsimmon.str "<!--")
                (Parsimmon.optionalWhitespace)
                (Parsimmon.str "-->")
            |> Parsimmon.map (fun (_,b,_) -> b)

        let nonEmptyComment =  
            Parsimmon.seq3 
                (Parsimmon.str "<!--")
                (Parsimmon.satisfy (fun c -> c <> "-") 
                |> Parsimmon.many 
                |> Parsimmon.concat)
                (Parsimmon.str "-->")
            |> Parsimmon.map (fun (_,text,_) -> text)

        Parsimmon.choose [emptyComment; commentWithWhitespace; nonEmptyComment]

    let nodeOpening =
        Parsimmon.seq2
          openingTagName
          (withWhitespace manyAttributes)
        |> Parsimmon.bind (fun (tag, attrs) -> 
            Parsimmon.str ">"
            |> Parsimmon.map (fun (_) -> tag, attrs))

    let nodeClosing ns tagName =
        let matchingTag =
            match ns with
            | Some ns' -> sprintf "%s:%s" ns' tagName
            | None -> tagName
        Parsimmon.seq2
            (Parsimmon.str "</")
            (Parsimmon.str matchingTag)
        |> Parsimmon.chain Parsimmon.optionalWhitespace
        |> Parsimmon.chain (Parsimmon.str ">")
        |> Parsimmon.map (fun _ -> ns, tagName)


    let emptyNode =
        nodeOpening
        |> Parsimmon.bind (fun ((ns, tagName), attrs) ->
            Parsimmon.optionalWhitespace
            |> Parsimmon.chain (nodeClosing ns tagName)
            |> Parsimmon.map (fun _ -> (ns, tagName), attrs))


    let emptyNodeWithTextContent =
        nodeOpening
        |> Parsimmon.bind (fun ((ns, tagName), attrs) ->
            textSnippet
            |> Parsimmon.skip (nodeClosing ns tagName)
            |> Parsimmon.map (fun text -> text, ns, tagName, attrs))

    let textNode =
        textSnippet
        |> Parsimmon.map (fun content ->
            {
                Namespace = None
                Name = ""
                Attributes = Map.empty
                Content = content
                Children = []
                SelfClosing = false
                IsTextNode = true
                IsComment = false
            })


    let rec simpleXmlElement = Parsimmon.ofLazy <| fun () ->

        let selfClosingElement =
            selfClosingTag
            |> Parsimmon.map (fun ((ns,tag), attrs) ->
                {
                    Namespace = ns
                    Name = tag
                    Attributes = Map.ofList attrs
                    Content = ""
                    Children = []
                    SelfClosing = true
                    IsTextNode = false
                    IsComment = false
                })

        let emptyElement =
            emptyNode
            |> Parsimmon.map (fun ((ns, name), attrs) ->
                {
                    Namespace = ns
                    Name = name
                    Attributes = Map.ofList attrs
                    Content = ""
                    Children = []
                    SelfClosing = false
                    IsTextNode = false
                    IsComment = false
                })

        let commentNode = 
            comment 
            |> Parsimmon.map (fun text -> 
                {
                    Namespace = None
                    Name = ""
                    Attributes = Map.empty
                    Content = text
                    Children = []
                    SelfClosing = false
                    IsTextNode = false
                    IsComment = true
                })
                
        let emptyElementWithText =
            emptyNodeWithTextContent
            |> Parsimmon.map (fun (content, ns, name, attrs) ->
                {
                    Namespace = ns
                    Name = name
                    Attributes = Map.ofList attrs
                    Content = content
                    Children = []
                    SelfClosing = false
                    IsTextNode = false
                    IsComment = false
                })

        let mixedNodes =
            nodeOpening
            |> Parsimmon.bind (fun ((ns, tag), attrs) ->
                [ simpleXmlElement; textNode ]
                |> Parsimmon.choose
                |> Parsimmon.atLeastOneOrMany
                |> Parsimmon.skip (nodeClosing ns tag)
                |> Parsimmon.map (fun children ->
                    {
                        Namespace = ns
                        Name = tag
                        Attributes = Map.ofList attrs
                        Content = ""
                        Children = List.ofArray children
                        SelfClosing = false
                        IsTextNode = false
                        IsComment = false
                    }))

        [ commentNode
          emptyElementWithText;
          emptyElement;
          selfClosingElement;
          mixedNodes ]
        |> Parsimmon.choose

    let rec xmlElement = Parsimmon.ofLazy <| fun () ->

        [ simpleXmlElement
          nodeOpening
            |> Parsimmon.bind (fun ((ns, tagName), attrs) ->
                xmlElement
                |> Parsimmon.many
                |> Parsimmon.map List.ofArray
                |> Parsimmon.skip (nodeClosing ns tagName)
                |> Parsimmon.map (fun children ->
                    {
                       Namespace = ns
                       Name = tagName
                       Content = ""
                       Children = children
                       Attributes = Map.ofList attrs
                       SelfClosing = false
                       IsTextNode = false
                       IsComment = false
                    }))
        ]
        |> List.map withWhitespace
        |> Parsimmon.choose

    let xmlDocument =
        [ declaration
          |> withWhitespace
          |> Parsimmon.bind (fun declAttrs ->
             (withWhitespace xmlElement)
             |> Parsimmon.map (fun root ->
                 {
                     Declaration = Some declAttrs
                     Root = root
                 }))

          xmlElement
          |> Parsimmon.map (fun root ->
              {
                  Declaration = None
                  Root = root
              })
        ]
        |> Parsimmon.choose
