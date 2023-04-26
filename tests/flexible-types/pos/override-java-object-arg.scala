// When we load a Java class file, if a java method has an argument with type
// 'Object', it (the method argument) gets loaded by Dotty as 'Any' (as opposed to 'AnyRef').
// This is pre-explicit-nulls behaviour.
// There is special logic in the type comparer that allows that method to be overridden
// with a corresponding argument with type 'AnyRef | Null' (or `Object | Null`).
// This test verifies that we can continue to override such methods, except that in
// the explicit nulls world we override with 'AnyRef|Null'.

import javax.management.{Notification, NotificationEmitter, NotificationListener}

class Foo {

  def bar(): Unit = {
    val listener = new NotificationListener() {
      // The second argument in the base interface is loaded with type 'Any', but we override
      // it with 'AnyRef|Null'.
      override def handleNotification(n: Notification|Null, emitter: Object|Null): Unit = {
      }
    }

    val listener2 = new NotificationListener() {
      // The second argument in the base interface is loaded with type 'Any', but we override
      // it with 'AnyRef|Null'.
      override def handleNotification(n: Notification|Null, emitter: AnyRef|Null): Unit = {
      }
    }

    val listener3 = new NotificationListener() {
      override def handleNotification(n: Notification|Null, emitter: Object): Unit = {
      }
    }

    val listener4 = new NotificationListener() {
      override def handleNotification(n: Notification, emitter: Object|Null): Unit = {
      }
    }

    val listener5 = new NotificationListener() {
      override def handleNotification(n: Notification, emitter: Object): Unit = {
      }
    }
  }
}
