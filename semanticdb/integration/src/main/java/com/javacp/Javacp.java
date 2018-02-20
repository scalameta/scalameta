package com.javacp;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

public class Javacp<T extends CharSequence, U> extends ArrayList<T> implements Comparable<U>, Serializable {

    @Override
    public int compareTo(U o) {
        return 0;
    }

    class Inner<T> {
        public void move(T e, U u) {}
    }

}