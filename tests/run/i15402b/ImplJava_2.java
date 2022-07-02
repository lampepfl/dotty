interface FooJavaFromScala extends NamedScala {
  default FooJavaFromScala self() {
    return this;
  }
  NamedScala foo(NamedScala x);
}

interface FooJavaFromJava extends NamedJava {
  default FooJavaFromJava self() {
    return this;
  }
  NamedJava foo(NamedJava x);
}

class BarJavaFromJava implements FooJavaFromJava {
  public NamedJava foo(NamedJava x) {
    return x;
  }
}

class BarJavaFromScala implements FooJavaFromScala {
  public NamedScala foo(NamedScala x) {
    return x;
  }
}
