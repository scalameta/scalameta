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
-P:semanticdb:denotations:definitions
-P:semanticdb:synthetics:none
```

Other Scalameta configurations progressively enable more and more features:
  * ScalametaFullContents enables `-P:semanticdb:mode:fat`
  * ScalametaFullDenotations further enables `-P:semanticdb:denotations:all`
  * ScalametaFullSynthetics further enables `-P:semanticdb:synthetics:all`

## Results

time/operation (ms/op) = lower score is better

```
Benchmark                      Mode    Cnt Score
QuickScalacBaseline            sample  70  741.478 ± 6.719 ms/op     
QuickScalacRangepos            sample  63  825.612 ± 15.735 ms/op    (12% overhead)
QuickScalametaBaseline         sample  50  1035.070 ± 23.457 ms/op   (40% overhead)
QuickScalametaFullContents     sample  50  1064.116 ± 24.879 ms/op   (44% overhead)
QuickScalametaFullDenotations  sample  45  1208.193 ± 36.802 ms/op   (63% overhead)
QuickScalametaFullSynthetics   sample  48  1146.530 ± 34.069 ms/op   (55% overhead)
```