package dotty.tools.dotc.util

import scala.language.unsafeNulls

import java.net.URLClassLoader
import java.nio.file.Paths

import dotty.tools.repl.AbstractFileClassLoader

object ClasspathFromClassloader {

  /** Attempt to recreate a classpath from a classloader.
   *
   *  BEWARE: with exotic enough classloaders, this may not work at all or do
   *  the wrong thing.
   */
  def apply(cl: ClassLoader | Null): String = {
    val classpathBuff = List.newBuilder[String]
    def collectClassLoaderPaths(cl: ClassLoader): Unit = {
      if (cl != null) {
        cl match {
          case cl: URLClassLoader =>
            // This is wrong if we're in a subclass of URLClassLoader
            // that filters loading classes from its parent ¯\_(ツ)_/¯
            collectClassLoaderPaths(cl.getParent)
            // Parent classloaders are searched before their child, so the part of
            // the classpath coming from the child is added at the _end_ of the
            // classpath.
            classpathBuff ++=
              cl.getURLs.iterator.map(url => Paths.get(url.toURI).toAbsolutePath.toString)
          case _ =>
            if cl.getClass.getName == classOf[AbstractFileClassLoader].getName then
              // HACK: We can't just collect the classpath from arbitrary parent
              // classloaders since the current classloader might intentionally
              // filter loading classes from its parent (for example
              // BootFilteredLoader in the sbt launcher does this and we really
              // don't want to include the scala-library that sbt depends on
              // here), but we do need to look at the parent of the REPL
              // classloader, so we special case it. We can't do this using a type
              // test since the REPL classloader class itself is normally loaded
              // with a different classloader.
              collectClassLoaderPaths(cl.getParent)
            else if cl eq ClassLoader.getSystemClassLoader then
              // HACK: For Java 9+, if the classloader is an AppClassLoader then use the classpath from the system
              // property `java.class.path`.
              classpathBuff += System.getProperty("java.class.path")
        }
      }
    }
    collectClassLoaderPaths(cl)
    classpathBuff.result().mkString(java.io.File.pathSeparator)
  }
}
