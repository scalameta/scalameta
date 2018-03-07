package com.javacp;

public class MetacJava {
    public static class StaticInner {
        public void isNotStatic() {}
        public static void isStatic() {}
        public class NonStatic {
            public void method(NonStatic e) {}
        }
    }
    public class Overload1 { public class A {} }
    public class Overload2 { public class A {} }
    public void overload(Overload1.A a) {}
    public void overload(Overload2.A a) {}
}
