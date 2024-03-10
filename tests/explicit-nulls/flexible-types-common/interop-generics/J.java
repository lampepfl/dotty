
class I<T> {}

class J {
  <T> I<T> foo(T x) {
  	return new I<T>();
  }

  <T> I<T>[] bar(T x) {
    Object[] r = new Object[]{new I<T>()};
    return (I<T>[]) r;
  }
}
