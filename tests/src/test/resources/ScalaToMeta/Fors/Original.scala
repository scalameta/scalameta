object Fors {
  for (i <- List(1, 2, 3); j = i; k = j; if (k % 2) == 0) yield i
  for ((i1, i2) <- List((1, 2), (2, 3)); j = i1; k = i2; if (k % 2) == 0) yield (i1, i2, j, k)
}