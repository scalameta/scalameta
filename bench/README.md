# Scalameta benchmarks

## Running benchmarks

```
bin/bench
```

## Methodology

Benchmarks involve running `bin/bench` on my laptop, which includes:
  * Compiling scalap with Scala 2.12
  * Compiling scalap with Scala 2.12 and semanticdb-scalac
  * Converting JDK into SemanticDB with Metacp
  * Converting Scala Library into SemanticDB with Metacp

The numbers are probably not stable enough (because laptop)
and not precise enough (because 5/10/1). However, I can rerun these benchmarks
quickly enough not to get annoyed when doing interactive development,
and that's what matters now.

## Configurations

<table>
  <th>
    <td><code>text</code></td>
    <td><code>symbols</code></td>
    <td><code>occurrences</code></td>
    <td><code>diagnostics</code></td>
    <td><code>synthetics</code></td>
  </th>
  <tr>
    <td><code>ScalametaBaseline</code></td>
    <td><code>off</code></td>
    <td><code>on</code></td>
    <td><code>on</code></td>
    <td><code>on</code></td>
    <td><code>off</code></td>
  </tr>
</table>

## Results

These results were obtained on Eugene's MacBook Pro (Retina, 15-inch, Mid 2015)
with i7 4980HQ, 16 GB RAM and 256 GB SSD running macOS Sierra 10.12.6.

time/operation (ms/op) = lower score is better

```
Benchmark         Mode    Cnt Score
ScalacBaseline    sample  51  1006.982 ± 13.579 ms/op
ScalacRangepos    sample  50  1061.746 ± 10.769 ms/op (+6%)
ScalametaBaseline sample  40  1365.613 ± 18.792 ms/op (+35%)

Benchmark          Mode    Cnt Score
MetacpScalaLibrary sample  9   3387.600 ± 50.458  ms/op
MetacpJDK          sample  12  2961.528 ± 36.541  ms/op
```
