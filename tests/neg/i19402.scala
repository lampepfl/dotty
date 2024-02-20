object Example {

  class Bar(foo: Foo)
  
  class Foo
  
  given Foo = Foo()
  
  given bar(foo: Foo): Bar = Bar(foo) // error
  
}
