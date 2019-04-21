namespace Fable.SimpleXml

module Generator = 

    type XAttribute = 
        | String of string * string 
        | Int of string * int 
        | Number of string * float 
        | Boolean of string * bool 

    type Tag = Tag of string 

    type XNode = 
        | Text of string
        | Leaf of (Tag * XAttribute list)
        | XNodeList of (Tag * XAttribute list * XNode list)  

    type attr() = 
        static member value(name, value) = XAttribute.String(name, value) 
        static member value(name, value) = XAttribute.Int(name, value)  
        static member value(name, value) = XAttribute.Number(name, value)  
        static member value(name, value) = XAttribute.Boolean(name, value)  

    let leaf name attrs = XNode.Leaf(Tag(name), attrs)
    let node name attrs values = XNode.XNodeList (Tag(name), attrs, values)
    let text value = XNode.Text value 
    
    let serializeAttr = function 
        | XAttribute.String (key, value) -> sprintf "%s=\"%s\"" key value 
        | XAttribute.Int (key, value) -> sprintf "%s=\"%d\"" key value
        | XAttribute.Number (key, value) -> sprintf "%s=\"%f\"" key value
        | XAttribute.Boolean (key, value) -> sprintf "%s=\"%s\"" key (string value)
        
    let rec serializeXml = function 
        | XNode.Text text -> text 
        | XNode.Leaf (Tag(tag), [ ]) -> sprintf "<%s />" tag
        | XNode.Leaf (Tag(tag), attributes) ->
            attributes
            |> List.map serializeAttr 
            |> String.concat " "
            |> sprintf "<%s %s />" tag 
        
        | XNode.XNodeList (Tag(tag), [ ], children) ->
            let childNodes = 
                children 
                |> List.map serializeXml
                |> String.concat ""
            
            sprintf "<%s>%s</%s>" tag childNodes tag

        | XNode.XNodeList (Tag(tag), attributes, children) ->
            let attributes =         
                attributes
                |> List.map serializeAttr 
                |> String.concat " "
         
            let childNodes = 
                children 
                |> List.map serializeXml
                |> String.concat ""
            
            sprintf "<%s %s>%s</%s>" tag attributes childNodes tag