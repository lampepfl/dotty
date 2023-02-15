package dotty.tools
package dotc
package config

import scala.language.unsafeNulls

/** For placing a wrapper function around property functions.
 *  Motivated by places like google app engine throwing exceptions
 *  on property lookups.
 */
trait WrappedProperties extends PropertiesTrait {
  def wrap[T](body: => T): Option[T]

  protected def propCategory: String     = "wrapped"
  protected def pickJarBasedOn: Class[?] = this.getClass

  override def propIsSet(name: String): Boolean              = wrap(super.propIsSet(name)) exists (x => x)
  override def propOrElse(name: String, alt: String): String = wrap(super.propOrElse(name, alt)) getOrElse alt
  override def setProp(name: String, value: String): String  = wrap(super.setProp(name, value)).orNull
  override def clearProp(name: String): String               = wrap(super.clearProp(name)).orNull
  override def envOrElse(name: String, alt: String): String  = wrap(super.envOrElse(name, alt)) getOrElse alt
  override def envOrNone(name: String): Option[String]       = wrap(super.envOrNone(name)).flatten

  def systemProperties: Iterator[(String, String)] = {
    import scala.jdk.CollectionConverters._
    wrap(System.getProperties.asScala.iterator) getOrElse Iterator.empty
  }
}

object WrappedProperties {
  object AccessControl extends WrappedProperties {
    def wrap[T](body: => T): Option[T] =
      try
        val result: T = body
        Some(result)
      catch {
        // the actual exception we are concerned with is AccessControlException,
        // but that's deprecated on JDK 17, so catching its superclass is a convenient
        // way to avoid a deprecation warning
        case _: SecurityException =>
          None
      }
  }
}
