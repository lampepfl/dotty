
class JavaBox<T> {
	T contents;

	JavaBox(T contents) { this.contents = contents; }
}

class Forwarder {

  static <T extends JavaBox<String>> void putInJavaBox(T box, String s) {
	box.contents = s;
  }
  
  static <T extends ScalaBox<String>> void putInScalaBox(T box, String s) {
	box.setContents(s);  
  }
}
