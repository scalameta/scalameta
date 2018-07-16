package selfs

class B

class C1 extends B { self =>
}

class C2 extends B { self: B =>
}

class C3 extends B { self: B with Int =>
}

class C4 extends B { _ =>
}

class C5 extends B { _: B =>
}

class C6 extends B { this: B =>
}

abstract class C7 { _: B =>
}
