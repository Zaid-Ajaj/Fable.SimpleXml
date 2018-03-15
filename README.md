# Fable.SimpleXml [![Nuget](https://img.shields.io/nuget/v/Fable.SimpleXml.svg?colorB=green)](https://www.nuget.org/packages/Fable.SimpleXml)   [![Build Status](https://travis-ci.org/Zaid-Ajaj/Fable.SimpleXml.svg?branch=master)](https://travis-ci.org/Zaid-Ajaj/Fable.SimpleXml)

A simple library for parsing Xml strings into structured Xml data. It is written using parser combinators from [Fable.Parsimmon](https://github.com/Zaid-Ajaj/Fable.Parsimmon)

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


### Using the library

The whole API is the following:
```fs
SimpleXml.tryParseElement : string -> Option<XmlElement>
SimpleXml.parseElement : string -> XmlElement
SimpleXml.tryParseDocument : string -> Option<XmlDocument>
SimpleXml.parseDocument : string -> XmlDocument
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
}
type XmlDocument = {
    Declaration : Map<string, string> option 
    Root : XmlElement 
}
```

Running the watching the tests live 
```sh
bash build.sh RunLiveTests 
```
Building the tests and running QUnut cli runner
```sh
bash build.sh RunTests
```
or just `Ctrl + Shift + B` to run the cli tests as a VS Code task