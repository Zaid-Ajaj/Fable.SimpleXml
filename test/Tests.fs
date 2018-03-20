module Tests

open QUnit
open Fable.SimpleXml
open Fable.SimpleXml.Parser
open Fable.Parsimmon
open Fable

registerModule "SimpleXml Tests"

let parseUsing p input =
    Parsimmon.parse input p

testCase "Parsing attributes" <| fun test ->
    ["height=\"12px\"";
     "width=\"15px\"";
     "data-attribute=\"ng-repeat\""
     "style='height:15px'"
     "is-open=true"
     "is-closed=false"]
    |> List.choose (parseUsing attribute)
    |> function
        | ["height", "12px"
           "width", "15px"
           "data-attribute", "ng-repeat"
           "style","height:15px"
           "is-open", "true"
           "is-closed", "false"] -> test.pass()
        | other -> test.unexpected other

testCase "Attribute containing escaped chars works" <| fun test ->
    """ style="'" """
    |> parseUsing (withWhitespace attribute)
    |> function
        | Some ("style", "'") -> test.pass()
        | other -> test.unexpected other

testCase "Parsing namespaced attributes works" <| fun test ->
    """ ns:key="value" """
    |> parseUsing (withWhitespace attribute)
    |> function
        | Some ("ns:key", "value") -> test.pass()
        | other -> test.unexpected other

testCase "Attribute containing escaped chars works" <| fun test ->
    """ style='"' """
    |> parseUsing (withWhitespace attribute)
    |> function
        | Some ("style", "\"") -> test.pass()
        | other -> test.unexpected other

testCase "Parsing self-closing tag works" <| fun test ->
    "<person firstName='John' lastName=\"Doe\" age=20 married=true/>"
    |> parseUsing selfClosingTag
    |> function
         | Some ((None, "person"), attributes)->
            match attributes with
            | [ "firstName", "John";
                 "lastName","Doe";
                 "age", "20";
                 "married", "true" ] -> test.pass()
            | otherResult -> test.unexpected otherResult
         | otherResult -> test.unexpected otherResult

testCase "Parsing self-closing tag without attributes or whitespace works" <| fun test ->
    "<br/>"
    |> parseUsing selfClosingTag
    |> function
        | Some ((None, "br"), []) -> test.pass()
        | otherResult -> test.unexpected otherResult

testCase "Identifier parser works" <| fun test ->
    match parseUsing identifier "h12" with
    | Some "h12" -> test.pass()
    | otherResult -> test.unexpected otherResult

testCase "Tag name parser works" <| fun test ->
    parseUsing tagName "h12"
    |> Option.map snd
    |> function
        | Some "h12" -> test.pass()
        | otherResult -> test.unexpected otherResult

    match parseUsing tagName "person" with
    | Some (None, "person") -> test.pass()
    | otherResult -> test.unexpected otherResult

testCase "Parsing self-closing tag works with identfiers" <| fun test ->
    "<html:h2 height='200px' />"
    |> parseUsing selfClosingTag
    |> function
         | Some ((Some "html", "h2"), ["height","200px"]) -> test.pass()
         | otherResult -> test.unexpected otherResult

testCase "Parsing tag with namespace works" <| fun test ->
    "ns:tag"
    |> parseUsing tagName
    |> function
        | Some (Some "ns", "tag") -> test.pass()
        | Some other -> test.unexpected other
        | None -> test.failwith "No match"

testCase "Declation parser works" <| fun test ->
    "<?xml version='1.0' ?>"
    |> parseUsing declaration
    |> function
        | Some dict ->
            match Map.tryFind "version" dict with
            | Some "1.0" -> test.pass()
            | other -> test.unexpected other
        | other -> test.unexpected other

testCase "Declation parser works with whitespace" <| fun test ->
    "  <?xml version='1.0' ?>  "
    |> parseUsing (withWhitespace declaration)
    |> function
        | Some dict ->
            match Map.tryFind "version" dict with
            | Some "1.0" -> test.pass()
            | other -> test.unexpected other
        | other -> test.unexpected other

testCase "Declation parser works with double qoutes and whitespace" <| fun test ->
    "  <?xml version=\"1.0\" ?>  "
    |> parseUsing (withWhitespace declaration)
    |> function
        | Some dict ->
            match Map.tryFind "version" dict with
            | Some "1.0" -> test.pass()
            | other -> test.unexpected other
        | None -> test.failwith "No Match"


testCase "Node opening parsing works with namespace" <| fun test ->
    "<ns:h1 height='20px' >"
    |> parseUsing nodeOpening
    |> function
        | Some ((Some "ns", "h1"), ["height", "20px"]) -> test.pass()
        | otherResult -> test.unexpected otherResult

testCase "Node opening parsing works without namespace" <| fun test ->
    "<h1 height='20px' >"
    |> parseUsing nodeOpening
    |> function
        | Some ((None, "h1"), ["height", "20px"]) -> test.pass()
        | otherResult -> test.unexpected otherResult


testCase "Node closing parsing works with provided namespace and tag and whitespace" <| fun test ->
    "</html:h1 >"
    |> parseUsing (nodeClosing (Some "html") "h1")
    |> function
        | Some ((Some "html"), "h1") -> test.pass()
        | otherResult -> test.unexpected otherResult

testCase "Node closing parsing works with provided namespace and tag with a lot whitespace" <| fun test ->
    "</html:h1  >"
    |> parseUsing (nodeClosing (Some "html") "h1")
    |> function
        | Some ((Some "html"), "h1") -> test.pass()
        | otherResult -> test.unexpected otherResult

testCase "Node closing parsing works with provided namespace and tag without whitespace" <| fun test ->
    "</html:h1>"
    |> parseUsing (nodeClosing (Some "html") "h1")
    |> function
        | Some ((Some "html"), "h1") -> test.pass()
        | otherResult -> test.unexpected otherResult


testCase "Node closing parsing doesn't yield if provided tag is different" <| fun test ->
    "</otherNs:h1>"
    |> parseUsing (nodeClosing (Some "html") "h1")
    |> function
        | None -> test.pass()
        | otherResult -> test.unexpected otherResult


testCase "Parsing empty node works" <| fun test ->
    "<h1></h1>"
    |> parseUsing emptyNode
    |> function
        | Some ((None, "h1"), []) -> test.pass()
        | otherResult -> test.unexpected otherResult

testCase "Parsing empty node works with whitespace in the middle" <| fun test ->
    "<h1>  </h1>"
    |> parseUsing emptyNode
    |> function
        | Some ((None, "h1"), []) -> test.pass()
        | otherResult -> test.unexpected otherResult


testCase "Parsing empty node works with attributes" <| fun test ->
    "<h1 height='20px' width=30 >  </h1 >"
    |> parseUsing emptyNode
    |> function
        | Some ((None, "h1"), ["height", "20px"; "width", "30"]) -> test.pass()
        | otherResult -> test.unexpected otherResult

testCase "Parsing empty node works with attributes and namespaces" <| fun test ->
    "<html:h1 height='20px' width=30 >  </html:h1  >"
    |> parseUsing emptyNode
    |> function
        | Some ((Some "html", "h1"), ["height", "20px"; "width", "30"]) -> test.pass()
        | otherResult -> test.unexpected otherResult

testCase "Parsing empty node doesn't yield when namespaces mismatch" <| fun test ->
    "  <html:h1 height='20px' width=30 >  </other:h1  >  "
    |> parseUsing emptyNode
    |> function
        | None -> test.pass()
        | otherResult -> test.unexpected otherResult

testCase "Parsing empty node doesn't yield when tags mismatch" <| fun test ->
    "  <html:h1 height='20px' width=30 >  </html:h2>  "
    |> parseUsing emptyNode
    |> function
        | None -> test.pass()
        | otherResult -> test.unexpected otherResult

testCase "Parsing simple empty node" <| fun test ->
    "<div></div>"
    |> parseUsing emptyNode
    |> function
        | Some ((None, "div"), []) -> test.pass()
        | other -> test.unexpected other

testCase "Parsing many empty nodes works" <| fun test ->
    "<div></div><h1></h1><ns:hello key='value' ></ns:hello>"
    |> parseUsing (Parsimmon.many emptyNode)
    |> function
        | Some [| (None, "div"), []
                  (None, "h1"),  []
                  (Some "ns", "hello"), ["key", "value"] |] -> test.pass()
        | other -> test.unexpected other



testCase "Chars content works" <| fun test ->
    [ "2.0"
      "text"
      "Text Content"
      "Some 2.0"
      "Some more text"
      "[]{},.!@#$%^&*()" ]
    |> List.choose (parseUsing textSnippet)
    |> function
        | [ "2.0"
            "text"
            "Text Content"
            "Some 2.0"
            "Some more text"
            "[]{},.!@#$%^&*()"] -> test.pass()
        | other -> test.unexpected other



testCase "Parsing empty node with text works" <| fun test ->
    [ "<div>hello</div>"
      "<ns:div>hello</ns:div>"
      "<p style='color:red'>Hello there</p>"
      "<span > Hello there </span>" ]
    |> List.choose (parseUsing emptyNodeWithTextContent)
    |> function
        | [ (value1, None, "div", [])
            (value2, Some "ns", "div", [])
            (value3, None, "p", ["style", "color:red"])
            (value4, None, "span", []) ] ->
            test.equal "hello" value1
            test.equal "hello" value2
            test.equal "Hello there" value3
            test.equal " Hello there " value4
        | other -> test.unexpected other

testCase "Parsing simple element works" <| fun test ->
    "<ns:div class='main' async=true>Text Content</ns:div>"
    |> parseUsing simpleXmlElement
    |> function
        | None -> test.failwith "No match"
        | Some el ->
            test.equal (Some "ns") el.Namespace
            test.equal "div" el.Name
            test.equal false el.SelfClosing
            test.equal "Text Content" el.Content
            test.isTrue (List.isEmpty el.Children)
            test.equal "main" (Map.find "class" el.Attributes)
            test.equal "true" (Map.find "async" el.Attributes)


testCase "Parsing mixed nodes: text then node" <| fun test ->
   "<root>hello <other></other></root>"
   |> parseUsing xmlElement
   |> function
       | None -> test.failwith "No match"
       | Some node ->
            let firstTextNode = List.head node.Children
            test.areEqual "hello " firstTextNode.Content

testCase "Parsing mixed nodes: node then text" <| fun test ->
   "<root><other></other> hello </root>"
   |> parseUsing xmlElement
   |> function
       | None -> test.failwith "No match"
       | Some node ->
            match node.Children with
            | [ other; hello ] -> test.areEqual " hello " hello.Content
            | otherwise -> test.unexpected otherwise

testCase "Parsing mixed nodes: self-closing node then text" <| fun test ->
   "<root><other/> hello </root>"
   |> parseUsing xmlElement
   |> function
       | None -> test.failwith "No match"
       | Some node ->
            match node.Children with
            | [ other; hello ] -> test.areEqual " hello " hello.Content
            | otherwise -> test.unexpected otherwise

testCase "Parsing mixed nodes: text then self-closing node" <| fun test ->
   "<root> hello <other/></root>"
   |> parseUsing xmlElement
   |> function
       | None -> test.failwith "No match"
       | Some node ->
            match node.Children.Head.Content with
            | " hello " -> test.pass()
            | _ -> test.unexpected node

testCase "Parsing text node works" <| fun test ->
    "hello "
    |> parseUsing textNode
    |> function
        | None -> test.failwith "No match"
        | Some el ->
            test.equal "hello " el.Content

testCase "Parsing text node works with one letter" <| fun test ->
    "h"
    |> parseUsing textNode
    |> function
        | None -> test.failwith "No match"
        | Some el -> test.passWith (sprintf "%A" el)

testCase "Parsing many text nodes works" <| fun test ->
    "hello "
    |> parseUsing (Parsimmon.atLeastOneOrMany textNode)
    |> function
        | None -> test.failwith "No match"
        | Some el -> test.passWith (sprintf "%A" el)


testCase "mixedNode parser works on many simple xml elements" <| fun test ->
    "<root><h1></h1> <other></other></root>"
    |> parseUsing xmlElement
    |> function
        | None -> test.failwith "No match"
        | Some node -> test.passWith (sprintf "%A" node)


testCase "Parsing mixed nodes: text then node then text" <| fun test ->
    "<root>hello <other></other> there</root>"
    |> parseUsing xmlElement
    |> function
        | None -> test.failwith "No match"
        | Some node -> 
            match node.Children with 
            | [ first; middle; second ] -> 
                test.areEqual "other" middle.Name
                test.areEqual "hello " first.Content 
                test.areEqual " there" second.Content
            | other -> test.unexpected other

testCase "Parsing mixed nodes: text then self-closing node then node" <| fun test ->
    "<root>hello <node /> <other></other></root>"
    |> parseUsing xmlElement
    |> function
        | None -> test.failwith "No match"
        | Some node -> test.passWith (sprintf "%A" node)


testCase "Parsing mixed nodes works inside simple xml element" <| fun test ->
    """<div class="container">
  <div class="notification">
    This container is <strong>centered</strong> on desktop.
    </div>
</div>
    """
    |> parseUsing xmlElement
    |> function
       | None -> test.failwith "No match"
       | Some node -> test.passWith (sprintf "%A" node)

testCase "Parsing simple selfclosing element works" <| fun test ->
    "<Person Id=20 FirstName='John' LastName='Doe' />"
    |> parseUsing simpleXmlElement
    |> function
        | None -> test.failwith "No match"
        | Some el ->
            test.equal None el.Namespace
            test.equal "Person" el.Name
            test.isTrue el.SelfClosing
            test.equal "" el.Content
            test.isTrue (List.isEmpty el.Children)
            test.equal "20" (Map.find "Id" el.Attributes)
            test.equal "John" (Map.find "FirstName" el.Attributes)
            test.equal "Doe" (Map.find "LastName" el.Attributes)

testCase "Parsing simple element with any text content" <| fun test ->
    "<Version>2.0</Version>"
    |> parseUsing simpleXmlElement
    |> function
        | None -> test.failwith "No match"
        | Some el ->
            test.equal None el.Namespace
            test.equal "Version" el.Name
            test.equal "2.0" el.Content
            test.isFalse el.SelfClosing
            test.isTrue (List.isEmpty el.Children)
            test.isTrue (Map.isEmpty el.Attributes)

testCase "Recursive XML node parsing works" <| fun test ->
    """
    <People>

        <Person Id=10 FirstName='John' LastName='Doe' />

        <Person>
            <Id>20</Id>
            <FirstName>Zaid</FirstName>
            <LastName makeLowerCase=true>Ajaj</LastName>
        </Person>
    </People>
    """
    |> SimpleXml.tryParseElement
    |> function
        | None -> test.failwith "No match"
        | Some people ->
            test.equal None people.Namespace
            test.equal "People" people.Name
            test.isTrue (Map.isEmpty people.Attributes)
            test.areEqual 2 (List.length (SimpleXml.findElementsByName "Person" people))
            let peopleChildren = people.Children |> List.filter (fun el -> not el.IsTextNode)
            test.equal 2 (List.length peopleChildren)
            match List.filter (fun el -> not el.IsTextNode) people.Children with
            | [ elemJohn; elemZaid ] ->
                test.areEqual None elemJohn.Namespace
                test.areEqual "Person" elemJohn.Name
                test.isTrue elemJohn.SelfClosing
                test.isTrue (List.isEmpty elemJohn.Children)
                test.areEqual "10" (Map.find "Id" elemJohn.Attributes)
                test.areEqual "John" (Map.find "FirstName" elemJohn.Attributes)
                test.areEqual "Doe" (Map.find "LastName" elemJohn.Attributes)

                test.areEqual None elemZaid.Namespace
                test.areEqual "Person" elemZaid.Name
                test.areEqual true (Map.isEmpty elemZaid.Attributes)
                let nonTextNodes = List.filter (fun el -> not el.IsTextNode) elemZaid.Children
                test.areEqual 3 (List.length nonTextNodes)
               
                [ SimpleXml.findElementByName "Id" elemZaid
                  SimpleXml.findElementByName "FirstName" elemZaid
                  SimpleXml.findElementByName "LastName" elemZaid ] 
                |> List.map (fun el -> el.Content)
                |> function 
                     | [ "20"; "Zaid";"Ajaj" ] -> 
                        match SimpleXml.tryFindElementByName "LastName" elemZaid with 
                        | Some lastName -> 
                            test.areEqual "true" (Map.find "makeLowerCase" lastName.Attributes)
                        | None -> test.failwith "Should not happen"
                     | other -> test.unexpected other                
            | other -> test.unexpected other


testCase "Parsing document works" <| fun test ->
    """
    <?xml version='1.0' ?>
    <People>
        <Person Id=10 />
    </People>
    """
    |> SimpleXml.tryParseDocument
    |> function
        | None -> test.failwith "No match"
        | Some document -> test.passWith (sprintf "%A" document)

testCase "Parsing document without xml declaration works" <| fun test ->
    """
    <People>
        <Person Id=10 />
    </People>
    """
    |> SimpleXml.tryParseDocument
    |> function
        | None -> test.failwith "No match"
        | Some document -> test.passWith (sprintf "%A" document)

testCase "Parsing element with text keep text meaningful whitespaces" <| fun test ->
    """<div class="container"><div class="notification">This container is <strong>centered</strong> on desktop.</div></div>"""
    |> SimpleXml.tryParseElement
    |> function
        | None -> test.failwith "No match"
        | Some container ->
            match container.Children with
            | [ notification ] ->
                match notification.Children with
                | [ textBefore; strong; textAfter ] ->
                    test.equal "This container is " textBefore.Content
                    test.equal "centered" strong.Content
                    test.equal " on desktop." textAfter.Content
                | other ->
                    test.unexpected other
            | other ->
                test.unexpected other

testCase "SimpleXml.parseManyElements works" <| fun test ->
    "<Person Name='John' />
     <Person Name='Jane' />
     <Person Name='Doe' />"
    |> SimpleXml.parseManyElements
    |> List.filter (fun el -> not el.IsTextNode)
    |> List.map (fun el -> Map.find "Name" el.Attributes)
    |> function 
        | [ "John"; "Jane"; "Doe" ] -> test.pass()
        | otherwise -> test.unexpected otherwise

testCase "SimpleXml use case" <| fun test ->
    "<People>
        <Person>John</Person>
        <Person>Jane</Person>
    </People>"
    |> SimpleXml.parseElementNonStrict
    |> SimpleXml.children
    |> List.map SimpleXml.content 
    |> test.areEqual [ "John"; "Jane" ]

testCase "SimpleXml.parseNonStrict excludes dummy whitespace" <| fun test ->
    "<People>
        <Person Id=10>John</Person>
        <Person Id=20>Jane</Person>
    </People>"
    |> SimpleXml.parseElementNonStrict
    |> SimpleXml.children
    |> List.map (fun el -> int (Map.find "Id" el.Attributes), el.Content ) 
    |> test.areEqual [ (10, "John"); (20, "Jane") ]  


type Person = { Id : int; Name: string }

let createPerson id name = 
    { Id = id; Name = name }

testCase "Parsing people works" <| fun test ->
    """
    <People>
        <Person Id=1 Name='John' />
        <Person Id='2' Name="Jane" />
    </People>
    """
    |> SimpleXml.parseElement
    |> SimpleXml.findElementsByName "Person"
    |> List.map (fun elem -> 
        let id = int (Map.find "Id" elem.Attributes)
        let name = Map.find "Name" elem.Attributes 
        createPerson id name)
    |> test.areEqual [{ Id = 1; Name = "John" };  
                      { Id = 2; Name = "Jane" }]  


testCase "Parsing empty comments works" <| fun test ->
    [ "<!---->"
      "<!-- -->"
      "<!--hello there-->"
      "<!-- hello there -->"
      "<!-- <other /> -->"
      "<!-- \n\n -->" ]
    |> List.choose (parseUsing comment)
    |> test.areEqual [ "" ; " "; "hello there"; " hello there "; " <other /> "; " \n\n " ]

testCase "Parsing XML with comments works" <| fun test ->
    "<Hello>
        <!--Just commenting here xD-->
        <!-- Another comment -->
        <There><!-- and another--></There>
    </Hello>"
    |> SimpleXml.parseElement
    |> SimpleXml.findElementsBy (fun el -> el.IsComment)
    |> List.map SimpleXml.content
    |> test.areEqual [ "Just commenting here xD"
                       " Another comment " 
                       " and another" ]