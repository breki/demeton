# Demeton

## One minute intro
Demeton is an F# library for generating hillshading (both in raster and vector forms) from [NASA's SRTM](https://en.wikipedia.org/wiki/Shuttle_Radar_Topography_Mission) digital elevation model.

The library is currently (September 2019) in development, so it's not yet production-ready. 

### Main goals I want to achieve
- Reading of zipped SRTM HGT files and converting them to **PNG grayscale images** (for more optimized local storage and interoperability). - **(working on this right now)**
- Generating **raster hillshades** for a given area (using algorithms I developed for [Maperitive](https://maperitive.net), and probably adding some more).
- The ability to convert the **raster hillshade into the vector form** (so it can be used in SVG vector maps like the ones I offer on [ScalableMaps](https://scalablemaps.com).
- Generating **elevation contours**.
- The API should be available from **C#**.
- OS-independent **console** application.
- Practice in learning [F#](https://fsharp.org/) (which is a great language) and functional programming.
