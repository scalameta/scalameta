### Host implementor notes

Palladium provides foundational data structures for metaprogramming defined in [Trees.scala](/reflection/core/Trees.scala) along with several levels of APIs (syntactic and semantic).

While syntactic services are implemented in Palladium itself, semantic services require external implementations called *hosts*, because it would be unreasonable to, for example, implement Scala's type inference or implicit resolution algorithms from scratch. In [Hosts.scala](reflection/semantic/Hosts.scala) we have encapsulated a minimalistic API surface that's required from hosts to invigorate Palladium.
