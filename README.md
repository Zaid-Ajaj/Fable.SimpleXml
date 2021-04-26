# Fable.SimpleXml [![Build Status](https://travis-ci.org/Zaid-Ajaj/Fable.SimpleXml.svg?branch=master)](https://travis-ci.org/Zaid-Ajaj/Fable.SimpleXml) [![Build Status](https://ci.appveyor.com/api/projects/status/s3esn08o7d4ehlm1?svg=true)](https://ci.appveyor.com/project/Zaid-Ajaj/fable-simplexml) [![Nuget](https://img.shields.io/nuget/v/Fable.SimpleXml.svg?maxAge=0&colorB=brightgreen)](https://www.nuget.org/packages/Fable.SimpleXml)

A simple library for parsing Xml strings into structured Xml data. Works in browser and node without installing any extra javascript dependencies. It is written using parser combinators from [Fable.Parsimmon](https://github.com/Zaid-Ajaj/Fable.Parsimmon)


### See the library in action in the [Test page](https://zaid-ajaj.github.io/Fable.SimpleXml/)


### Installation
Install from nuget using paket
```sh
paket add nuget Fable.SimpleXml --project path/to/YourProject.fsproj
```
Make sure the references are added to your paket files
```sh
# paket.dependencies (solution-wide dependencies)
nuget Fable.SimpleXml

# paket.refernces (project-specific dependencies)
Fable.SimpleXml
```

### Example usage
```fs
type Person = { Id : int; Name: string }

let createPerson id name =
    { Id = id; Name = name }

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
```
Or you can inspect the content of the elements:
```fs
testCase "SimpleXml use case" <| fun test ->
    "<People>
        <Person>John</Person>
        <Person>Jane</Person>
    </People>"
    |> SimpleXml.parseElementNonStrict
    |> SimpleXml.children
    |> List.map SimpleXml.content
    |> test.areEqual [ "John"; "Jane" ]
```
### API

```fs
// Parsing functions
SimpleXml.tryParseElement : string -> Option<XmlElement>
SimpleXml.parseElement : string -> XmlElement
SimpleXml.tryParseDocument : string -> Option<XmlDocument>
SimpleXml.parseDocument : string -> XmlDocument

// Non strict parsing functions, exludes text nodes between elements (leaving Content intact)
SimpleXml.parseElementNonStrict : string -> XmlElement
SimpleXml.tryParseElementNonStrict : string -> Option<XmlElement>
SimpleXml.tryParseDocumentNonStrict : string -> Option<XmlDocument>
SimpleXml.parseDocumentNonStrict : string -> XmlDocument

// Search functions
SimpleXml.findElementsBy : (XmlElement -> bool) -> XmlElement -> XmlElement list
SimpleXml.findElementsByName : string -> XmlElement -> XmlElement list
SimpleXml.findElementByName : string -> XmlElement -> XmlElement
SimpleXml.findElementsByExactAttributes : Map<string, string> -> XmlElement -> XmlElement list
SimpleXml.findElementByAttribute : string -> string -> XmlElement -> XmlElement list
SimpleXml.tryFindElementByAttributes : Map<string, string> -> XmlElement -> Option<XmlElement>
SimpleXml.tryFindElementByName : string -> XmlElement -> Option<XmlElement>
/// ... AND MORE ...
```

Where `XmlElement` and `XmlDocument` are defined as follows:
```fs
type XmlElement = {
    Namespace : string option
    Name : string
    Attributes : Map<string, string>
    Content : string
    Children : XmlElement list
    SelfClosing : bool
    IsTextNode : bool
    IsComment : bool
}

type XmlDocument = {
    Declaration : Map<string, string> option
    Root : XmlElement
}
```

### Generate Xml:
Create Xml from a tree structure. Opening the `Fable.SimpleXml.Generator` module, gives you access to these:
 - `node`: creates a nested element
 - `leaf`: creates a self-closing element
 - `text`: creates a terminal node with text
 - `comment`: creates a comment
 - `namespace`: adds a namespace prefix to a `node` or a `leaf`
 - `attr.value`: create an attribute
 - `ofXmlElement`/`ofXmlElements`: Can be used to convert a `XmlElemnt` to a Xml tree
 - `serializeXml`: converts a Xml tree to a xml string

> Indentation is not supported yet. PR's are welcome ;)

```fs
open Fable.SimpleXml.Generator

let people =
    node "people" [ ] [
        leaf "person" [
            attr.value("name", "John Doe")
            attr.value("age", 26)
            attr.value("married", false)
        ]

        leaf "person" [
            attr.value("name", "Jane Doe")
            attr.value("age", 25)
            attr.value("married", true)
        ]
    ]

serializeXml people
```
will generate:
```xml
<people>
  <person name="John Doe" age="26" married="false" />
  <person name="Jane Doe" age="25" married="true" />
</people>
```

Use nested property
```fs
let person =
    node "person" [ ] [
        node "id" [ ] [ text "1" ]
        node "name" [ ] [ text "John" ]
        node "married" [ ] [ text "false" ]
    ]

serializeXml person
```
will generate
```xml
<person>
    <id>1</id>
    <name>John</name>
    <married>false</married>
</person>
```

Access any xml to update/change it
```fs
let xml =
    """people>
        <person name="John Doe" age="26" married="false" />
        <person name="Jane Doe" age="25" married="true" />
    </people>"""

let parsedXml = SimpleXml.parseElement xml

// .. Work with the xml by removing, changing, adding any xml element or attribute including comments and namespaces. ..

// Revert XmlElement back to xml string

let xmlTree = Generator.ofXmlElement parsedXml

serializeXml xmlTree
```

### Running sample app locally
```sh
./build.sh RunSample
#or
build RunSample
```
### Running the tests live
```sh
./build.sh RunLiveTests
```
### Building the tests and running QUnit cli runner
```sh
./build.sh RunTests
```
or just `Ctrl + Shift + B` to run the cli tests as a VS Code task