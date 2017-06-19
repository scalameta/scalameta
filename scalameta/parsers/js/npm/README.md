# scalameta-parsers

A JS API for the `parsers` module of [`scalameta`](https://github.com/scalameta/scalameta).

It exposes two functions:

- `parseSource(code: string)`: parses the given code as a full-fledges source file

- `parseStat(code: string)`: parses the given code a statement (useful for parsing single expressions without wrapping them into an object or class)
