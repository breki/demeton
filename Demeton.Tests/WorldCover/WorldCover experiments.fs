module Tests.WorldCover.``WorldCover experiments``


open System
open Demeton.DemTypes
open FsUnit
open Xunit
open BitMiracle.LibTiff.Classic
open Swensen.Unquote

// todo 5: this function should accept the SrtmTileId that needs to be read
//   and then find the required TIFF tiles to fill the HeightsArray.
//   We can then test the result by implementing a custom shader that renders
//   just the water bodies. We will deal with mixing this together with AW3D
//   later.


let readWorldCoverRaster (fileName: string) : DemHeight[] =
    use tiff = Tiff.Open(fileName, "r")

    let width = unbox (tiff.GetField(TiffTag.IMAGEWIDTH).[0].Value)

    if width <> 36000 then
        failwithf $"Expected width 36000, but got %d{width}"

    let height = unbox (tiff.GetField(TiffTag.IMAGELENGTH).[0].Value)

    if height <> 36000 then
        failwithf $"Expected height 36000, but got %d{height}"

    let fieldValues = tiff.GetField(TiffTag.IMAGEDEPTH)
    // looks like IMAGEDEPTH is not available in the file
    // let imageDepth = unbox (fieldValues.[0].Value)

    let planarConfig: PlanarConfig =
        tiff.GetField(TiffTag.PLANARCONFIG).[0].Value :?> PlanarConfig

    if planarConfig <> PlanarConfig.CONTIG then
        failwithf
            $"Expected CONTIG planar configuration, but got %A{planarConfig}"

    if not (tiff.IsTiled()) then
        failwithf "Expected a tiled TIFF file"

    let tileWidth = unbox (tiff.GetField(TiffTag.TILEWIDTH).[0].Value)
    let tileLength = unbox (tiff.GetField(TiffTag.TILELENGTH).[0].Value)

    let buffer = Array.zeroCreate<byte> (tiff.TileSize())

    for tileY in [ 0..tileLength..height ] do
        for tileX in [ 0..tileWidth..width ] do
            /// 0-based byte offset in buffer at which to begin storing read
            /// and decoded bytes
            let offset = 0
            /// z-coordinate of the pixel within a tile to be read and decoded
            let tileZ = 0
            /// The zero-based index of the sample plane. The plane parameter is
            /// used only if data are organized in separate planes
            /// (PLANARCONFIG = SEPARATE). In other cases the value is ignored.
            let plane = int16 0

            // The tile to read and decode is selected by the (x, y, z, plane)
            // coordinates (i.e. ReadTile returns the data for the tile
            // containing the specified coordinates.
            let bytesInTile =
                tiff.ReadTile(buffer, offset, tileX, tileY, tileZ, plane)

            if bytesInTile = -1 then
                failwith "Could not read tile."

    // Process the tile data stored in 'buffer'
    // ...

    raise (NotImplementedException())


[<Fact(Skip = "in progress")>]
let ``Load WorldCover file into a DemHeight`` () =
    let raster =
        readWorldCoverRaster
            @"Samples\ESA_WorldCover_10m_2021_v200_N45E006_Map.tif"

    test <@ raster <> null @>
