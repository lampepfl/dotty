import java.util.function.*;

@FunctionalInterface
interface SAMJava1 {
  public String[] f(String x);
}

@FunctionalInterface
interface SAMJava2 {
  public void f(int x);
}

class J {
  public void g1(SAMJava1 s) {
  }

  public void g2(SAMJava2 s) {
  }

  public void h1(Function<String, String[]> s) {
  }
}