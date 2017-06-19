# scalameta-parsers

A JS API for the `parsers` module of [`scalameta`](https://github.com/scalameta/scalameta).

It exposes two functions:

- `parseSource(code: string)`: parses the given code as a full-fledged source file

- `parseStat(code: string)`: parses the given code a statement (useful for parsing single expressions without wrapping them into an object or class)

Example:

```js
const { parseStat, parseSource } = require('scalameta-parsers');

const tree1 = parseStat('val answer = 42');
console.log(tree1);

// Output:

// { type: 'Defn.Val',
//   children:
//    [ { type: 'Pat.Var.Term', children: [Object], pos: [Object] },
//      { type: 'Lit.Int', children: [], pos: [Object], value: 42 } ],
//   pos: { start: 0, end: 15 } }

const tree2 = parseSource(`
object Main {
  def main(args: Array[String]): Unit =
    println("Hello, World!")
}`);
console.log(tree2);

// Output:

// { type: 'Source',
//   children: [ { type: 'Defn.Object', children: [Object], pos: [Object] } ],
//   pos: { start: 0, end: 85 } }
```

