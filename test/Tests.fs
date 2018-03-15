module Tests

open QUnit
open Fable.SimpleXml
open Fable.SimpleXml.Parser
open Fable.Parsimmon

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

testCase "Parsing comment works: empty" <| fun test ->
    "<!--  -->"
    |> parseUsing comment
    |> function 
        | Some "" -> test.pass()
        | otherResult -> test.unexpected otherResult 

testCase "Parsing comment works: one word" <| fun test ->
    "<!-- this -->"
    |> parseUsing comment
    |> function 
        | Some "this " -> test.pass()
        | otherResult -> test.unexpected otherResult 

testCase "Parsing comment works: sentence" <| fun test ->
    "<!-- this is a comment -->"
    |> parseUsing comment
    |> function 
        | Some "this is a comment " -> test.pass()
        | None -> test.failwith "No match"
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

testCase "Node opening parsing works without namespace with whitespace in the beginning" <| fun test ->
    " <h1 height='20px' >"
    |> parseUsing nodeOpening 
    |> function 
        | Some ((None, "h1"), ["height", "20px"]) -> test.pass()
        | otherResult -> test.unexpected otherResult

testCase "Node opening parsing works without namespace with whitespace at the end" <| fun test ->
    "<h1 height='20px' > "
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


testCase "Parsing empty node works with whitespace everywhere" <| fun test ->
    "  <h1  >  </h1  >  "
    |> parseUsing emptyNode 
    |> function 
        | Some ((None, "h1"), []) -> test.pass()
        | otherResult -> test.unexpected otherResult

testCase "Parsing empty node works with whitespace everywhere and attributes" <| fun test ->
    "  <h1 height='20px' width=30 >  </h1  >  "
    |> parseUsing emptyNode 
    |> function 
        | Some ((None, "h1"), ["height", "20px"; "width", "30"]) -> test.pass()
        | otherResult -> test.unexpected otherResult

testCase "Parsing empty node works with whitespace everywhere and attributes and namespaces" <| fun test ->
    "  <html:h1 height='20px' width=30 >  </html:h1  >  "
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

testCase "Parsing many empty nodes with new lines works" <| fun test ->
    """ 
    <div></div>
    <h1></h1>
    <ns:hello key='value' ></ns:hello>
    """
    |> parseUsing (Parsimmon.many emptyNode)
    |> function 
        | Some [| (None, "div"), [] 
                  (None, "h1"),  []
                  (Some "ns", "hello"), ["key", "value"] |] -> test.pass()
        | other -> test.unexpected other

testCase "Parsing empty node with text works" <| fun test -> 
    [ "<div>hello</div>"
      "<ns:div>hello</ns:div>"
      "<p style='color:red'>Hello there</p>"
      "<span > Hello there </span>" ]
    |> List.choose (parseUsing emptyNodeWithTextContent) 
    |> function 
        | [ ("hello", None, "div", [])
            ("hello", Some "ns", "div", [])
            ("Hello there", None, "p", ["style", "color:red"])
            ("Hello there ", None, "span", []) ] -> 
            test.pass()
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
            test.equal 2 (List.length people.Children)
            match people.Children with
            | [ elemJohn; elemZaid ] -> 
                test.equal None elemJohn.Namespace
                test.equal "Person" elemJohn.Name
                test.isTrue elemJohn.SelfClosing
                test.isTrue (List.isEmpty elemJohn.Children)
                test.equal "10" (Map.find "Id" elemJohn.Attributes)
                test.equal "John" (Map.find "FirstName" elemJohn.Attributes)
                test.equal "Doe" (Map.find "LastName" elemJohn.Attributes)

                test.equal None elemZaid.Namespace
                test.equal "Person" elemZaid.Name 
                test.equal true (Map.isEmpty elemZaid.Attributes)
                test.equal 3 (List.length elemZaid.Children)
                match elemZaid.Children with 
                | [ { Name = "Id"; 
                      Content = "20"; 
                      Children = [];  
                      Namespace = None;
                      Attributes = _
                      SelfClosing = false  };
                    { Name = "FirstName"; 
                      Content = "Zaid"; 
                      Children = [];  
                      Namespace = None;
                      Attributes = _
                      SelfClosing = false  };
                    { Name = "LastName"; 
                      Content = "Ajaj"; 
                      Children = [];  
                      Namespace = None;
                      Attributes = attrs
                      SelfClosing = false  } ] when Map.toList attrs = [ "makeLowerCase", "true" ] -> test.pass()
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