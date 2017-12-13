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
QuickScalacBaseline            sample  35  1595.274 ± 15.263  ms/op
QuickScalacRangepos            sample  35  1703.097 ± 25.044  ms/op  (6% overhead)
QuickScalametaOpt              sample  15  1934.902 ± 29.232  ms/op  (21% overhead)
QuickScalametaBaseline         sample  15  2020.256 ± 39.586  ms/op  (25% overhead)
QuickScalametaFullContents     sample  15  2081.549 ± 38.977  ms/op  (30% overhead)
QuickScalametaFullDenotations  sample  15  2236.738 ± 28.241  ms/op  (39% overhead)
QuickScalametaFullSynthetics   sample  15  2272.054 ± 81.361  ms/op  (42% overhead)
```
