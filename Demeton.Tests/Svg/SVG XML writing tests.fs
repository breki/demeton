module Tests.Svg.``SVG XML writing tests``

open Svg
open Svg.DocumentModel

open System.IO
open System.Text
open System.Xml

open Xunit
open Swensen.Unquote

[<Literal>]
let XLinkNS = "http://www.w3.org/1999/xlink";

[<Literal>]
let InkscapeNS = "http://www.inkscape.org/namespaces/inkscape";

[<Literal>]
let ScalableMapsSvgExtNS = "https://scalablemaps.com/schemas/svg-ext";

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

let writeSvgXml (stream: Stream) doc =
    let xmlWriterSettings =
        new XmlWriterSettings(
            Encoding = new UTF8Encoding(false),
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
<svg xmlns:xlink=""http://www.w3.org/1999/xlink"" version=""1.1"" xml:space=""preserve"" width=""1415.45mm"" height=""2323.11mm"" viewBox=""0 0 5349.74 8780.26"" xmlns=""http://www.w3.org/2000/svg"" />"             @>

