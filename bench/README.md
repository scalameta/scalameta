# Scalameta benchmarks

## Running benchmarks

```
bin/bench
```

## Methodology

Benchmarks involve running `bin/bench` on my laptop, which includes:
  * Compiling scalap with Scala 2.12
  * Compiling scalap with Scala 2.12 and semanticdb-scalac

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
    <td><code>experimental:synthetics</code></td>
  </th>
  <tr>
    <td><code>ScalametaBaseline</code></td>
    <td><code>off</code></td>
    <td><code>on</code></td>
    <td><code>on</code></td>
    <td><code>on</code></td>
    <td><code>off</code></td>
  </tr>
  <tr>
    <td><code>ScalametaFullText</code></td>
    <td><code>on</code></td>
    <td><code>on</code></td>
    <td><code>on</code></td>
    <td><code>on</code></td>
    <td><code>off</code></td>
  </tr>
</table>

## Results

time/operation (ms/op) = lower score is better

```
Benchmark              Mode    Cnt Score
QuickScalacBaseline    sample  51  1006.982 ± 13.579 ms/op
QuickScalacRangepos    sample  50  1061.746 ± 10.769 ms/op (+6%)
QuickScalametaBaseline sample  40  1354.603 ± 20.615 ms/op (+35%)
QuickScalametaFullText sample  39  1409.716 ± 27.101 ms/op (+40%)
```
