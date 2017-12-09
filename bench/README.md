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

```
Benchmark                      Mode    Cnt Score
QuickScalacBaseline            sample  35  1544.163 ± 18.766 ms/op
QuickScalacRangepos            sample  35  1623.975 ± 25.106 ms/op
QuickScalametaBaseline         sample  15  3870.224 ± 375.329 ms/op
QuickScalametaFullContents     sample  15  3856.802 ± 352.485 ms/op
QuickScalametaFullDenotations  sample  15  4224.783 ± 387.847 ms/op
QuickScalametaFullSynthetics   sample  15  4260.294 ± 342.503 ms/op
```
