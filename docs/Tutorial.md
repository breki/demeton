# Demeton - 10 Minutes Tutorial

This tutorial is a short walk-through of some of the capabilities of Demeton's command line. Don't worry, it is not that complex, as you will see.

## Getting the tool
First of all, download the [latest release](https://github.com/breki/demeton/releases/latest). There are several packages available in the release:
- If you are on **Linux**, use **`Demeton-linux-x64-full.zip`** package. It contains Demeton's libraries as well as .NET Core stuff needed to run it. **NOTE:** you still need to install `libgdiplus` package on your system (if you don't already have it).
- If you are on **Windows** and you don't have .NET Core 3 installed, please use the **`Demeton-win10-x64-full.zip`** package, as it includes .NET Core libraries.
- If you are on **Windows** and you already have .NET Core 3 installed, you can use the smaller **`Demeton-win10-x64.zip`** package.

After you have downloaded the package, unzip it to your local disk and open a command line terminal to that directory.

## Getting the test data
In order to generate hillshadings, we need a digital elevation model (DEM) that we can work on. Demeton's GitHub repository has some sample DEM tiles that we will use in this tutorial. 

If you are on Linux, simply run `download-sample-data.sh` script in the command line terminal. It will download two DEM tiles to a subdirectory path `cache/0`.

On Windows, you will have to do this manually, unfortunately. Follow these steps:
1. In the directory where you unzipped Demeton package, create a subdirectory called `cache` and then inside of it, create a subdirectory called `0` (zero).
1. Go to https://github.com/breki/demeton/blob/master/samples/cache/0/N46E013.png with your browser and click on the `Download` button. Copy the downloaded file to the newly created subdirectory path (`cache/0`).
1. Repeat the same for https://github.com/breki/demeton/blob/master/samples/cache/0/N46E014.png

## Our first hillshading

```bash
./Demeton.Console shade 13.49437,46.159668,14.236633,46.543914
```