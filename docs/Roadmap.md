# Demeton Roadmap

The page lists features I plan to implement (in the order of priority).

## Support for other map projections
Right now Demeton outputs the maps in Spherical Mercator projection, but for my own purposes I need support other types of projections, too. The projection would be provided through [PROJ](https://proj.org/usage/projections.html) parameters. The PROJ parser has already been implemented and what's left now is to implement [individual projections](https://proj.org/operations/projections/index.html#projections). I'm not sure how much effort would take to implement support for all of the projections, I will start with the ones I need, probably by porting a PROJ library from another language.

## Georeferencing metadata
Right now Demeton only generates raster tile PNG files, but there is no accompanying information that tells the user where each of the tiles lies in space. This information is needed when you want to combine these tiles with other cartographic content (like road vector layers, landuse etc.). 

So each tile would probably need a "sidecar" file with this georeferencing information, but I'm not sure in what format this file should be. What I do believe is that the file should be in an easy-to-parse format (key-value pairs, JSON, XML...).

## More shaders and filters
Current Demeton offers four shader operations, but I plan to add more. I am also thinking about introducing low and high pass filters to augment the shaders.

## Vector output
The problem with hillshading rasters (bitmaps) is that they require a lot of memory and they don't scale - they are prepared for a certain map scale (and DPI) and when zoomed beyond that you can see the constituent pixels. For a reasonably sized map, a printing-quality raster covering it requires large image files which are difficult to work with.

A possible alternative would be to transform the raster into a series of vector polygons of the same (or similar) color/shade. This process has different names: vectorization, image tracing, posterization. 

I still have to decide on the exact mechanics of this: 
1. How to quantize colors into a (small) set of similar colors (to avoid having too many polygons).   
1. How to deal with transparency (when it comes to semi-transparent hillshading).

One open issue is the output format for this vectorized hillshading. Possibilities that come to my mind:
1. Binary file (maybe using protobuf).
1. GeoJSON (could be too verbose for this quantity of data).
1. SVG (more like an extra output feature, since you lose georeferencing information).

one extra feature would be a SVG export of these contours.

Some useful links: 
- https://en.wikipedia.org/wiki/Image_tracing
- https://en.wikipedia.org/wiki/Color_quantization
- https://en.wikipedia.org/wiki/Posterization

## Generating elevation contours
This task is similar to vectorization and some of the vectorization code could probably be reused. Instead of polygons, the end result here would be a list of polylines (or perhaps even better - Bezier curves). Each polyline would have an associated elevation. 

I think it would still be useful to break up contours into a grid of tiles, to avoid very long polylines.

As with the vectorization, there is an open question about the output format (and extra feature would be a SVG export of these contours).

## C# API
Since I'm fairly new to F#, I have no experience with F#-C# interoperability (except calling Framework libraries in F#, of course). So I'm not sure how much the existing Demeton code would need to be changed to accommodate accessing it from C# (I assume a lot of it would need to be changed).

Perhaps a better approach would be to implement a C#-specific wrapper around the existing code.

In any case, this is the least important feature for me right now, hence its position at the back of the queue ;-).