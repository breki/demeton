``` ini

BenchmarkDotNet=v0.11.5, OS=Windows 10.0.18362
Intel Core i7-7700 CPU 3.60GHz (Kaby Lake), 1 CPU, 8 logical and 4 physical cores
.NET Core SDK=2.2.401
  [Host] : .NET Core ? (CoreCLR 4.6.27817.03, CoreFX 4.6.27818.02), 64bit RyuJIT DEBUG

InvocationCount=1  UnrollFactor=1  

```
|  Method | ImageSize | Mean | Error |
|-------- |---------- |-----:|------:|
| Filter1 |      3600 |   NA |    NA |

Benchmarks with issues:
  PngFilterComparison.Filter1: Job-VVKNZC(InvocationCount=1, UnrollFactor=1) [ImageSize=3600]
