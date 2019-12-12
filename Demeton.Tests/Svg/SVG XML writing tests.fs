module Tests.Svg.``SVG XML writing tests``

open Svg
open Svg.DocumentModel

open System.Text
open System.Xml

open Xunit
open Swensen.Unquote

// Aliasing these types in order to avoid opening System.IO module - it has
// Path type that conflicts with Svg.Path.
type SeekOrigin = System.IO.SeekOrigin
type Stream = System.IO.Stream
type StreamReader = System.IO.StreamReader
type MemoryStream = System.IO.MemoryStream

[<Literal>]
let XLinkNS = "http://www.w3.org/1999/xlink";

[<Literal>]
let InkscapeNS = "http://www.inkscape.org/namespaces/inkscape";

[<Literal>]
let ScalableMapsSvgExtNS = "https://scalablemaps.com/schemas/svg-ext";

let writeEndElement  (writer: XmlWriter) =
    writer.WriteEndElement()
    writer

let writeId (ElementName id: ElementName) (writer: XmlWriter) =
    writer.WriteAttributeString("id", id)
    writer

let writeClasses (classes: Classes option) (writer: XmlWriter) =
    let classesToString someClasses = someClasses |> String.concat " " 
    
    match classes with
    | None -> writer
    | Some someClasses ->
        writer.WriteAttributeString("class", (classesToString someClasses))
        writer
        
let writeStyle (style: Style option) (writer: XmlWriter) =
    match style with
    | None -> writer
    | Some someStyle ->
        writer.WriteAttributeString("style", someStyle.Style)
        writer
    
let writeLengthAttr attrName length (writer: XmlWriter) =
    writer.WriteAttributeString(attrName, length |> SvgLength.toString)
    writer
    
let writeViewBox (viewBox: ViewBox) (writer: XmlWriter) =
    let viewBoxToString() =
        [| viewBox.MinX; viewBox.MinY; viewBox.Width; viewBox.Height |]
        |> Array.map (fun x ->
            x.ToString("0.##",
                       System.Globalization.CultureInfo.InvariantCulture))
        |> String.concat " "
    
    writer.WriteAttributeString("viewBox", viewBoxToString())
    writer
    
let writePathData pathData (writer: XmlWriter) =
    writer.WriteAttributeString("d", PathDataFuncs.pathDataToString pathData)
    writer
    
let writePath (path: Path) (writer: XmlWriter) =
    writer.WriteStartElement("path")

    writer
    |> writeId path.Id
    |> writeStyle path.Style
    |> writeClasses path.Classes
    |> writePathData path.PathData
    |> writeEndElement

let writeChildren
    (children: GroupChildElement list)
    // we need to parametrize writeGroup to avoid circular dependency
    writeGroup
    (writer: XmlWriter) =
    children
    |> List.iter (
        function
        | Group group -> writer |> writeGroup group |> ignore
        | Path path -> writer |> writePath path |> ignore
        )
    writer
    
let rec writeGroup (group: Group) (writer: XmlWriter) =
    writer.WriteStartElement("g")
    
    writer
    |> writeId group.Id
    |> writeStyle group.Style
    |> writeClasses group.Classes
    |> writeChildren group.Children writeGroup
    |> writeEndElement
       
let writeSvg (doc: Document) (writer: XmlWriter) =
    writer.WriteStartElement(
        "",
        "svg",
        "http://www.w3.org/2000/svg");

    writer.WriteAttributeString(
        "xmlns",
        "xlink",
        "",
        XLinkNS);

    writer.WriteAttributeString("version", "1.1");
    writer.WriteAttributeString(
        "xml",
        "space",
        "",
        "preserve");
    
    writer
    |> writeLengthAttr "width" doc.Width
    |> writeLengthAttr "height" doc.Height
    |> writeClasses doc.Classes
    |> writeStyle doc.Style
    |> writeViewBox doc.ViewBox
    |> writeChildren doc.Children writeGroup
    |> writeEndElement

let writeSvgXml (stream: Stream) doc =
    let xmlWriterSettings =
        XmlWriterSettings(
            Encoding = UTF8Encoding(false),
            Indent = true,
            IndentChars = "\t",
            NewLineOnAttributes = false,
            OmitXmlDeclaration = false)
    
    use writer = XmlWriter.Create(stream, xmlWriterSettings);
    
    writer |> writeSvg doc |> ignore
    
    ignore()

[<Fact>]
let ``Can write an empty SVG document``() =
    let doc =
        Document.build
            (SvgLength.mm 1415.45)  
            (SvgLength.mm 2323.11)
            { MinX = 0.; MinY = 0.; Width = 5349.7412; Height = 8780.2588 }
            []
    
    use stream = new MemoryStream()
    writeSvgXml stream doc
    
    test <@ stream.Seek(0L, SeekOrigin.Begin) = 0L @>
    
    use textReader = new StreamReader(stream)
    let svgXml = textReader.ReadToEnd()
    
    test <@ svgXml = @"<?xml version=""1.0"" encoding=""utf-8""?>
<svg xmlns:xlink=""http://www.w3.org/1999/xlink"" version=""1.1"" xml:space=""preserve"" width=""1415.45mm"" height=""2323.11mm"" viewBox=""0 0 5349.74 8780.26"" xmlns=""http://www.w3.org/2000/svg"" />"
        @>

[<Fact>]
let ``Can write a SVG document with an empty group``() =
    let group =
        Group.build (ElementName "polygons") []
        |> Group.withClass "style1"
    
    let doc =
        Document.build
            (SvgLength.mm 1415.45)  
            (SvgLength.mm 2323.11)
            { MinX = 0.; MinY = 0.; Width = 5349.7412; Height = 8780.2588 }
            [ Group group ]
    
    use stream = new MemoryStream()
    writeSvgXml stream doc
    
    test <@ stream.Seek(0L, SeekOrigin.Begin) = 0L @>
    
    use textReader = new StreamReader(stream)
    let svgXml = textReader.ReadToEnd()
    
    test <@ svgXml = @"<?xml version=""1.0"" encoding=""utf-8""?>
<svg xmlns:xlink=""http://www.w3.org/1999/xlink"" version=""1.1"" xml:space=""preserve"" width=""1415.45mm"" height=""2323.11mm"" viewBox=""0 0 5349.74 8780.26"" xmlns=""http://www.w3.org/2000/svg"">
	<g id=""polygons"" class=""style1"" />
</svg>"
        @>

[<Fact>]
let ``Can write a SVG document with a group having a path inside``() =
    let pathData: PathData = [
        MoveToAbs [(100., 100.)]
        LineToRel [(50., -50.)]
    ]
    
    let path =
        Path.build (ElementName "water-riverbank") pathData
    
    let group =
        Group.build (ElementName "polygons") [ Path path ]
        |> Group.withClass "style1"
    
    let doc =
        Document.build
            (SvgLength.mm 1415.45)  
            (SvgLength.mm 2323.11)
            { MinX = 0.; MinY = 0.; Width = 5349.7412; Height = 8780.2588 }
            [ Group group ]
    
    use stream = new MemoryStream()
    writeSvgXml stream doc
    
    test <@ stream.Seek(0L, SeekOrigin.Begin) = 0L @>
    
    use textReader = new StreamReader(stream)
    let svgXml = textReader.ReadToEnd()
    
    test <@ svgXml = @"<?xml version=""1.0"" encoding=""utf-8""?>
<svg xmlns:xlink=""http://www.w3.org/1999/xlink"" version=""1.1"" xml:space=""preserve"" width=""1415.45mm"" height=""2323.11mm"" viewBox=""0 0 5349.74 8780.26"" xmlns=""http://www.w3.org/2000/svg"">
	<g id=""polygons"" class=""style1"">
		<path id=""water-riverbank"" d=""M100 100l50-50"" />
	</g>
</svg>"
        @>

