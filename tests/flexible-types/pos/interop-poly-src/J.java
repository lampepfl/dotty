import java.util.*;

class JavaCat<T> {
  T prop;
}

class J {
  static <T> ScalaCat<T> getScalaCat() {
    return null;
  }

  static <T> JavaCat<T> getJavaCat() {
    return null;
  }

  static List<String[]> getListOfStringArray() {
    List<String[]> as = new ArrayList<String[]>();
    as.add(new String[1]);
    return as;
  }

  static List<String>[] getArrayOfStringList() {
    return (List<String>[]) new List[1];
  }

  static List<List<String[]>[]> getComplexStrings() {
    return new ArrayList<List<String[]>[]>();
  }
}
