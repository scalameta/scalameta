package com.javacp;

// #1492: the JLS-mandated `values()`/`valueOf(String)` are compiler-synthesized and must be
// SYNTHETIC, while same-named user overloads with a different signature must NOT be. This fixture
// exercises the signature comparison in javacp (see Javacp.sproperties / isSyntheticEnumMethod).
public enum EnumOverloads {
  ONE;

  public static EnumOverloads valueOf(int ordinal) { return ONE; }
  public static int values(String name) { return 0; }
}
