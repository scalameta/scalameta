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
-P:semanticdb:overrides:none
```

Other Scalameta configurations progressively enable more and more features:
  * ScalametaFullContents enables `-P:semanticdb:mode:fat`
  * ScalametaFullDenotations further enables `-P:semanticdb:denotations:all`
  * ScalametaFullSynthetics further enables `-P:semanticdb:synthetics:all`
  * ScalametaFullOverrides further enables `-P:semanticdb:overrides:all`

## Results

```
Benchmark                      Mode    Cnt Score
QuickScalacBaseline            sample  50  1042.956 ± 17.837 ms/op   
QuickScalacRangepos            sample  50  1089.995 ± 11.297 ms/op   ( 5% overhead)
QuickScalametaBaseline         sample  40  1294.257 ± 22.686 ms/op   (25% overhead)
QuickScalametaFullContents     sample  40  1295.044 ± 11.326 ms/op   (25% overhead)
QuickScalametaFullDenotations  sample  40  1395.760 ± 12.056 ms/op   (34% overhead)
QuickScalametaFullOverrides    sample  38  1427.002 ± 21.510 ms/op   (37% overhead)
QuickScalametaFullSynthetics   sample  40  1345.795 ± 15.959 ms/op   (30% overhead)
```