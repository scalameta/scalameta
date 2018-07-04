## Project overview

The code in this repository is primarily divided into three top-level directories:

- `scalameta/`: syntactic APIs like tokenizers, parsers, trees, quasiquotes and
  pretty printers.
- `semanticdb/`: semantic APIs like the SemanticDB protobuf schema, compiler plugin,
  metacp to process classpaths and metap to pretty-print SemanticDB payloads.
- `tests/`: unit and integration tests for both Scalameta and SemanticDB.

## Testing

The exact test command to run tests depends on what you are working on:

- `testsJVM/test`: run fast unit tests for both syntactic and semantic APIs.
- `testsJVM/slow:test`: run slow integration tests for semantic APIs.
- `testsJVM/all:test`: run all tests, both fast and slow.

Tips to make your edit/test/debug worflow as productive as possible:

- Use `testOnly` to speedup your workflow. Running `testsJVM/test` may
  still be too slow if you want fast feedback while iterating on a small change.
  It's recommended to use `testsJVM/testOnly *CUSTOM_SUITE` where `CUSTOM_SUITE`
  is the name of your test suite.
- Use `testOnly *CUSTOM_SUITE -- -z "TEST_NAME"` to target an individual test case
  from a test suite. Some test suites contains a lot of unit tests so it may still
  be too slow to use `testOnly`. To target an individual slow test you must use
  the `all` scope `testsJVM/all:testOnly *CUSTOM_SUITE -- -z "TEST_NAME"`.
- Use the sbt command `save-expect` to make `ExpectSuite` pass. This command
  writes to disk the output of the current behavior. The diff may be large,
  please review it thoroughly to ensure that the new expected behavior is correct.
  The name "expect tests" comes from
  [this blog post](https://blog.janestreet.com/testing-with-expectations/).

## Ticket Guidelines

- **Bugs**:
  - Contain steps to reproduce.
  - Contain a code sample that exhibits the error.
  - *Ideally* contain your scalameta/semanticdb versions.
  - *Ideally* contain the scala version you are using.
  - Inform us if the issue is blocking you with no visible workaround
    - This will give the ticket priority
- **Features**
  - Show a code example of how that feature would be used.

## Contribution Guidelines

- **All code PRs should**:
  - have a meaningful commit message description
  - comment important things
  - include unit tests (positive and negative)
  - pass [CI](http://drone.geirsson.com:8001/scalameta/scalameta), which
    automatically runs when your pull request is submitted.
  - be reformatted with `./bin/scalafmt`
  - add a relevant entry to the
    [changelog](https://github.com/scalameta/scalameta/tree/master/changelog)
    for the next stable release
- **Be prepared to discuss/argue-for your changes if you want them merged**!
  You will probably need to refactor so your changes fit into the larger
  codebase
- **If your code is hard to unit test, and you don't want to unit test it,
  that's ok**. But be prepared to argue why that's the case!
- **It's entirely possible your changes won't be merged**, or will get ripped
  out later. This is also the case for maintainer changes, even
  [@xeno-by](https://github.com/xeno-by)!
- **Even a rejected/reverted PR is valuable**! It helps explore the solution
  space, and know what works and what doesn't. For every line in the repo, at
  least three lines were tried, committed, and reverted/refactored, and more
  than 10 were tried without committing.
- **Feel free to send Proof-Of-Concept PRs** that you don't intend to get merged.

## Documentation Guidelines

- Prefer the terms `enrichment` and `extension` as opposed to `pimp`

In case of any questions, don't hesitate to ask on our
[gitter channel](https://gitter.im/scalameta/scalameta).

(these guidelines have been adapted from
https://github.com/lihaoyi/ammonite#contribution-guidelines)

