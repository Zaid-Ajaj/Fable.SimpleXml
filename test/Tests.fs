module Tests

open QUnit
open Fable.SimpleXml.AST
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
        | Some (Declaration dict) -> 
            match Map.tryFind "version" dict with
            | Some "1.0" -> test.pass()
            | other -> test.unexpected other
        | Some other -> test.unexpected other
        | None -> test.failwith "No Match"

testCase "Declation parser works with whitespace" <| fun test ->
    "  <?xml version='1.0' ?>  "
    |> parseUsing (withWhitespace declaration)
    |> function
        | Some (Declaration dict) -> 
            match Map.tryFind "version" dict with
            | Some "1.0" -> test.pass()
            | other -> test.unexpected other
        | Some other -> test.unexpected other
        | None -> test.failwith "No Match"