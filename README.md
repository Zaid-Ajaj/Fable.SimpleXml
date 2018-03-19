# Fable.SimpleXml [![Nuget](https://img.shields.io/nuget/v/Fable.SimpleXml.svg?colorB=green)](https://www.nuget.org/packages/Fable.SimpleXml)   [![Build Status](https://travis-ci.org/Zaid-Ajaj/Fable.SimpleXml.svg?branch=master)](https://travis-ci.org/Zaid-Ajaj/Fable.SimpleXml)

## See the library in action in the [Test page](https://zaid-ajaj.github.io/Fable.SimpleXml/)

A simple library for parsing Xml strings into structured Xml data. Works in browser and node without installing any extra javascript dependencies. It is written using parser combinators from [Fable.Parsimmon](https://github.com/Zaid-Ajaj/Fable.Parsimmon)

Template Used to build the library: [fable-library-template](https://github.com/Zaid-Ajaj/fable-library-template)

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
}

type XmlDocument = {
    Declaration : Map<string, string> option 
    Root : XmlElement 
}
```


### Known Issues
 - Parsing Comments doens't work
 - Attributes must be on the same line

### Running the tests live 
```sh
./build.sh RunLiveTests 
```
### Building the tests and running QUnit cli runner
```sh
./build.sh RunTests
```
or just `Ctrl + Shift + B` to run the cli tests as a VS Code task