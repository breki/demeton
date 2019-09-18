``` ini

BenchmarkDotNet=v0.11.5, OS=Windows 10.0.18362
Intel Core i7-7700 CPU 3.60GHz (Kaby Lake), 1 CPU, 8 logical and 4 physical cores
.NET Core SDK=2.2.401
  [Host] : .NET Core ? (CoreCLR 4.6.27817.03, CoreFX 4.6.27818.02), 64bit RyuJIT DEBUG


```
|                 Method | ArraySize | Mean | Error |
|----------------------- |---------- |-----:|------:|
|  **AccessArrayViaIndexer** |       **100** |   **NA** |    **NA** |
| AccessArrayThroughSpan |       100 |   NA |    NA |
|  **AccessArrayViaIndexer** |      **1000** |   **NA** |    **NA** |
| AccessArrayThroughSpan |      1000 |   NA |    NA |
|  **AccessArrayViaIndexer** |     **10000** |   **NA** |    **NA** |
| AccessArrayThroughSpan |     10000 |   NA |    NA |
|  **AccessArrayViaIndexer** |    **100000** |   **NA** |    **NA** |
| AccessArrayThroughSpan |    100000 |   NA |    NA |

Benchmarks with issues:
  ArrayAccessComparison.AccessArrayViaIndexer: DefaultJob [ArraySize=100]
  ArrayAccessComparison.AccessArrayThroughSpan: DefaultJob [ArraySize=100]
  ArrayAccessComparison.AccessArrayViaIndexer: DefaultJob [ArraySize=1000]
  ArrayAccessComparison.AccessArrayThroughSpan: DefaultJob [ArraySize=1000]
  ArrayAccessComparison.AccessArrayViaIndexer: DefaultJob [ArraySize=10000]
  ArrayAccessComparison.AccessArrayThroughSpan: DefaultJob [ArraySize=10000]
  ArrayAccessComparison.AccessArrayViaIndexer: DefaultJob [ArraySize=100000]
  ArrayAccessComparison.AccessArrayThroughSpan: DefaultJob [ArraySize=100000]
