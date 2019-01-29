package selfs

class B/*selfs.B#*/

class C1/*selfs.C1#*/ extends B/*selfs.B#*/ { self/*local0*/ =>
}

class C2/*selfs.C2#*/ extends B/*selfs.B#*/ { self/*local1*/: B/*selfs.B#*/ =>
}

class C3/*selfs.C3#*/ extends B/*selfs.B#*/ { self/*local2*/: B/*selfs.B#*/ with Int/*scala.Int#*/ =>
}

class C4/*selfs.C4#*/ extends B/*selfs.B#*/ { _ =>
}

class C5/*selfs.C5#*/ extends B/*selfs.B#*/ { _: B/*selfs.B#*/ =>
}

class C6/*selfs.C6#*/ extends B/*selfs.B#*/ { this: B/*selfs.B#*/ =>
}

abstract class C7/*selfs.C7#*/ { _: B/*selfs.B#*/ =>
}
