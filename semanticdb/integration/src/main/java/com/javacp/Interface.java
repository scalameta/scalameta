package com.javacp;

public interface Interface {
  void a();
  default void d() {}
  static void s() {}
  void ap(int x);
  default void dp(int x) {}
  static void sp(int x) {}
  default <T> void dt(T x) {}
}
