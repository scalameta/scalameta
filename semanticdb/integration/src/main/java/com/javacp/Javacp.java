package com.javacp;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;

public class Javacp<A extends CharSequence, B> extends ArrayList<A> implements Comparable<B>, Serializable {

    @Override
    public int compareTo(B b) {
        return 0;
    }

    class InnerShadowTypeParam<A> {
        public void move(A a, B b) {
        }
    }

    ArrayList<A> genericField;
    int[] arrayField;

    ArrayList<A> genericMethod() {
        return null;
    }

    int[] arrayMethod() {
        return new int[0];
    }

    void genericParams(A a, B b) {
    }

    void primitiveParams(int a, long b, float c, double d, short e, byte f, boolean g, char h) {
    }

    public void overload(java.io.File a) {
    }

    public void overload(sourcecode.File a) {
    }

    // primitive fields
    public int Int;
    public long Long;
    public float Float;
    public short Short;
    public byte Byte;
    public boolean Boolean;
    public char Char;

    void typeParams(ArrayList<HashMap<A, String[]>> a, Hashtable<String, B> b) {
    }

    <C extends Integer> void methodTypeParams(C c) {
    }

    private int privateField;
    protected int protectedField;
    public int publicField;
    int packagePrivateField;

    private void privateMethod() {
    }

    protected void protectedMethod() {
    }

    public void publicMethod() {
    }

    void packagePrivateMethod() {
    }

}