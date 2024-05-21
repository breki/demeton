```
ShadeCommand.run
  (options: Options)
  (generateTile: ShadedRasterTileGenerator)
  (saveTile: ShadedRasterTileSaver)
  : Result<unit, string> =  
```

```
  createMapProjection 
    projection 
    mapScale
    : Result<Projection, string>
```

```
  runWithProjection
    mapProjection
    (options: Options)
    (generateTile: ShadedRasterTileGenerator)
    (saveTile: ShadedRasterTileSaver)
    : Result<unit, string>
```

```
      ShadeCommand.generateShadedRasterTile
        (heightsArrayFetchers: DemHeightsArrayFetcher[])
        (createShaderFunction: ShadingFuncFactory)
        srtmLevel
        tileRect
        rootShadingStep
        mapProjection
        : Result<RawImageData option, string>
```

```
          executeShadingStep
            shadingFuncFactory
            compositingFuncFactory
            (heightsArrays: HeightsArray[])
            srtmLevel
            tileRect
            forward
            inverse
            (step: ShadingStep)
            : RawImageData
```

```
      ShadeCommand.saveShadedRasterTile
        (ensureDirectoryExists: DirectoryExistsEnsurer)
        (openFileToWrite: FileWriter)
        (writePngToStream: Png.File.PngStreamWriter)
        (options: Options)
        (maxTileIndex: int)
        (tileIndexX, tileIndexY)
        (tileRect: Rect)
        imageData
        : Result<string, string>
```
