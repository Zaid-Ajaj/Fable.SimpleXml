module App

open Fable.SimpleXml
open Fable
open Fable.Core
open Fable.Import.Browser

[<Emit("$0.value")>]
let getValue (x: obj) : string = jsNative

[<Emit("$0.value = $1")>]
let setValue (x: obj) (y: string) : unit = jsNative

let sampleZero = "<Book Title='Lord of the Rings' Pages=500 ISBN=\"263468624687\" />"
let sampleOne = 
    """<Person>
    <Id>20</Id>
    <FirstName>John</FirstName>
    <LastName>Doe</LastName>
    <Married>true</Married>
</Person>
    """
let sampleTwo = 
    """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>netstandard2.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="App.fs" />
  </ItemGroup>
  <ItemGroup>
    <ProjectReference Include="..\src\Fable.SimpleXml.fsproj" />
  </ItemGroup>
  <Import Project="..\.paket\Paket.Restore.targets" />
</Project>
    """

let sampleThree = 
    """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <Description>A library for easily working with Xml in Fable projects</Description>
    <PackageProjectUrl>https://github.com/Zaid-Ajaj/Fable.SimpleXml</PackageProjectUrl>
    <RepositoryUrl>https://github.com/Zaid-Ajaj/Fable.SimpleXml.git</RepositoryUrl>
    <PackageLicenseUrl>https://github.com/Zaid-Ajaj/Fable.SimpleXml/blob/master/LICENSE</PackageLicenseUrl>
    <PackageIconUrl></PackageIconUrl>
    <PackageTags>fsharp;fable</PackageTags>
    <Authors>Zaid Ajaj</Authors>
    <Version>1.0</Version>
    <TargetFramework>netstandard2.0</TargetFramework>
    <GenerateDocumentationFile>true</GenerateDocumentationFile>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="AST.fs" />
    <Compile Include="Parser.fs" />
    <Compile Include="SimpleXml.fs" />
  </ItemGroup>
  <ItemGroup>
    <Content Include="*.fsproj; *.fs;" PackagePath="fable\" />
  </ItemGroup>
  <Import Project="..\.paket\Paket.Restore.targets" />
</Project>
    """

let sampleFour = 
    """<?xml version="1.0" encoding="UTF-8"?>
<breakfast_menu>
  <food>
    <name>Belgian Waffles</name>
    <price>$5.95</price>
    <description>Two of our famous Belgian Waffles with plenty of real maple syrup</description>
    <calories>650</calories>
  </food>
  <food>
    <name>Strawberry Belgian Waffles</name>
    <price>$7.95</price>
    <description>Light Belgian waffles covered with strawberries and whipped cream</description>
    <calories>900</calories>
  </food>
  <food>
    <name>Berry-Berry Belgian Waffles</name>
    <price>$8.95</price>
    <description>Light Belgian waffles covered with an assortment of fresh berries and whipped cream</description>
    <calories>900</calories>
  </food>
  <food>
    <name>French Toast</name>
    <price>$4.50</price>
    <description>Thick slices made from our homemade sourdough bread</description>
    <calories>600</calories>
  </food>
  <food>
    <name>Homestyle Breakfast</name>
    <price>$6.95</price>
    <description>Two eggs, bacon or sausage, toast, and our ever-popular hash browns</description>
    <calories>950</calories>
  </food>
</breakfast_menu>
    """

[<Emit("JSON.stringify(JSON.parse($0), null, 4)")>]
let beautify (input: string) : string = jsNative

let parseElem = document.getElementById "parseElem"
let parseDoc = document.getElementById "parseDoc"

let parseElemNonStrict = document.getElementById "parseElemNonStrict"
let parseDocNonStrict = document.getElementById "parseDocNonStrict"


let zero = document.getElementById "zero"
let one = document.getElementById "one"
let two = document.getElementById "two"
let three = document.getElementById "three"
let four = document.getElementById "four"

let input = document.getElementById "input"
let output = document.getElementById "output" 


zero.addEventListener("click", unbox (fun ev -> 
   setValue input sampleZero)) 

one.addEventListener("click", unbox (fun ev -> 
   setValue input sampleOne)) 

two.addEventListener("click", unbox (fun ev -> 
   setValue input sampleTwo)) 
 
three.addEventListener("click", unbox (fun ev -> 
   setValue input sampleThree)) 

four.addEventListener("click", unbox (fun ev -> 
   setValue input sampleFour)) 

parseElem.addEventListener("click", unbox (fun ev -> 
   match SimpleXml.tryParseElement (getValue input) with 
   | Some el -> setValue output (beautify (sprintf "%A" el))
   | None -> setValue output "Could not parse input XML as an element, if there is a declaration element <?xml ... ?>, then try parsing as a document"
))

parseDoc.addEventListener("click", unbox (fun ev -> 
   match SimpleXml.tryParseDocument (getValue input) with 
   | Some el -> setValue output (beautify (sprintf "%A" el))
   | None -> setValue output "Could not parse input XML as a document"
))

parseElemNonStrict.addEventListener("click", unbox (fun _ -> 
   match SimpleXml.tryParseElementNonStrict (getValue input) with 
   | Some el -> setValue output (beautify (sprintf "%A" el))
   | None -> setValue output "Could not parse input XML as an element, if there is a declaration element <?xml ... ?>, then try parsing as a document"
))

parseDocNonStrict.addEventListener("click", unbox (fun _ -> 
   match SimpleXml.tryParseDocumentNonStrict (getValue input) with 
   | Some el -> setValue output (beautify (sprintf "%A" el))
   | None -> setValue output "Could not parse input XML as a document"
))