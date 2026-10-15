// See https://github.com/scalameta/scalameta/issues/1806
package example

class Issue1806(x: Issue1806 { type T = Issue1806 with Issue1806 }) {
  val inParent: (Issue1806 { type U }) { type T } = ???
  val inDecl: Issue1806 { def m: Issue1806 { def n: Int } } = ???
  val inArg: List[Issue1806 { type T = Issue1806 with Issue1806 }] = ???
  val flat: Issue1806 { type T } = ???
  type Alias = Issue1806 { type T = Issue1806 with Issue1806 }
}
