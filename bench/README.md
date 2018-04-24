# Scalameta benchmarks

## Running benchmarks

```
bin/bench
```

## Benchmarking methodology

Benchmarks involve running `bin/bench All` on my laptop, which includes:
  * Compiling scalap with Scala 2.11
  * Compiling scalap with Scala 2.11 and semanticdb-scalac

The numbers are probably not stable enough (because laptop)
and not precise enough (because 5/10/1). However, I can rerun these benchmarks
quickly enough not to get annoyed when doing interactive development,
and that's what matters now.

## Benchmarking configurations

ScalametaBaseline corresponds to the following compiler plugin options:

```
-P:semanticdb:mode:slim
-P:semanticdb:symbols:definitions
-P:semanticdb:synthetics:none
```

Other Scalameta configurations progressively enable more and more features:
  * ScalametaFullContents enables `-P:semanticdb:mode:fat`
  * ScalametaFullSymbols further enables `-P:semanticdb:symbols:all`
  * ScalametaFullSynthetics further enables `-P:semanticdb:synthetics:all`

## Results

time/operation (ms/op) = lower score is better

```
Benchmark                      Mode    Cnt Score
[info] Benchmark                              Mode  Cnt     Score    Error   Units
[info] QuickScalacBaseline.run              sample   42  1288.101 ± 50.795   ms/op (0% overhead)
[info] QuickScalacRangepos.run              sample   40  1340.133 ± 34.653   ms/op (4% overhead)
[info] QuickScalametaBaseline.run           sample   31  1780.414 ± 112.561  ms/op (38% overhead)
[info] QuickScalametaFullSymbols.run        sample   30  1794.813 ±  52.375  ms/op (39% overhead)
[info] QuickScalametaFullContents.run       sample   27  1971.634 ± 237.622  ms/op (53% overhead)
[info] QuickScalametaFullSynthetics.run     sample   29  1982.315 ± 130.798  ms/op (53% overhead)
```