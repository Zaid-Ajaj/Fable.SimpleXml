namespace Fable.SimpleXml

[<AutoOpen>]
module AST = 
    
    type Version = Version of float

    type XmlElement = { 
        Namespace : string option
        Name : string
        Attributes : Map<string, string>
        Children : Xml list 
        SelfClosing : bool
    }

    and Xml =
        | Declaration of Map<string, string>
        | Element of XmlElement
        | Content of string
        | Comment of string
        