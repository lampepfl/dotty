trait FooScalaFromScala extends NamedScala:
  def self: FooScalaFromScala = this
  def foo(x: NamedScala): NamedScala

trait FooScalaFromJava extends NamedJava:
  def self: FooScalaFromJava = this
  def foo(x: NamedJava): NamedJava