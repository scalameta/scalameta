package com.javacp;

class Outer {

    class Inner {
        // the compiler injects a first implicit param `this$` which is flagged mandated
        // - only when using `-parameters` before JDK21
        // - always as of JDK21
        Inner(Integer... values) {}

        public void printOuter() {
            // For all JDKs, the generated code relies on an implicit class field `this$`
            // holding a reference to the outer class.
            System.out.println(Outer.this);
        }

        class InnerInner {
            InnerInner(Integer... values) {}

            public void printOuters() {
                System.out.println(Outer.this);
                System.out.println(Outer.Inner.this);
            }
        }
    }

    class InnerUnusedOuter {
        // the compiler injects a first implicit param `this$` which is flagged mandated
        // - only when using `-parameters` before JDK21
        // - always as of JDK21
        InnerUnusedOuter(Integer... values) {}

        // Before JDK18, the `this$` synthetic field holding the reference to the outer
        // class was synthetized. As of JDK18, that field is not present if unused. 
    }

    private class PrivateInnerUnusedOuter {
        // the compiler injects a first implicit param `this$` which is flagged synthetic
        // - only when using `-parameters` before JDK21
        // - always as of JDK21
        PrivateInnerUnusedOuter(Integer... values) {}

        // Before JDK18, the `this$` synthetic field holding the reference to the outer
        // class was synthetized. As of JDK18, that field is not present if unused. 
    }

}

