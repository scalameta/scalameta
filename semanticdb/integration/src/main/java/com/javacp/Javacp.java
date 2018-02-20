package com.javacp;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

public class Javacp<T extends CharSequence, U> extends ArrayList<T> implements Serializable, Comparable<U> {

    @Override
    public int compareTo(U o) {
        return 0;
    }

    public Javacp(Integer i) {

    }

    public <T> HashSet<T> signature(HashMap<ArrayList<HashMap<String, Integer[]>>, String[]> arg, String[] arg2) {
        return null;
    }

    int isPackagePrivateField;
    private int isPrivateField;
    protected int isProtectedField;
    public int isPublicField;

    int isPackagePrivateMethod() { return 0; }
    private int isPrivateMethod() { return 0; }
    protected int isProtectedMethod() { return 0; }
    public int isPublicMethod() { return 0; };

    final int isVal = 0;

    public void overload(File file) { }
    public void overload(sourcecode.File file) { }

}