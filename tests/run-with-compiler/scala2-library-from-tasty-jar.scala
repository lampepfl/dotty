import dotty.tools.io.Directory
import dotty.tools.dotc.util.ClasspathFromClassloader

import java.io.File.pathSeparator
import java.io.File.separator

@main def Test: Unit =
  blacklistsOnlyContainsClassesThatExist()
  // FIXME this test does not work on JDK8
  // Caused by: dotty.tools.dotc.core.TypeError$$anon$1: package scala.quoted.runtime.Expr does not have a member method quote
  if System.getProperty("java.specification.version") != "1.8" then
    compileFromTastyInJar(compileBlacklisted)

def blacklistsOnlyContainsClassesThatExist() =
  val scalaLibTastyPathsSet = scalaLibTastyPaths.toSet
  assert(compileBlacklisted.diff(scalaLibTastyPathsSet).isEmpty,
    compileBlacklisted.diff(scalaLibTastyPathsSet).mkString(
      "`loadBlacklisted` contains names that are not in `scalaLibTastyPaths`: \n  ", "\n  ", "\n\n"))

def dottyVersion =
  System.getProperty("java.class.path").nn.split(pathSeparator).collectFirst {
    case path if path.endsWith(".jar") && path.contains("scala3-library_3-") =>
      path.split("scala3-library_3-").last.stripSuffix(".jar")
  }.get

def scalaLibJarPath =
  s"out${separator}bootstrap${separator}scala2-library-tasty${separator}scala-$dottyVersion-nonbootstrapped${separator}scala2-library-tasty-experimental_3-$dottyVersion.jar"

def scalaLibClassesPath =
  java.nio.file.Paths.get(
    s"out${separator}bootstrap${separator}scala2-library-bootstrapped${separator}scala-$dottyVersion-nonbootstrapped${separator}classes")

lazy val scalaLibTastyPaths =
  new Directory(scalaLibClassesPath).deepFiles
    .filter(_.ext.isTasty)
    .map(_.normalize.path.stripPrefix(scalaLibClassesPath.toString + separator))
    .toList

def compileFromTastyInJar(blacklisted: Set[String]): Unit = {
  val driver = new dotty.tools.dotc.Driver
  val yFromTastyBlacklist =
    blacklisted.mkString("-Yfrom-tasty-ignore-list:", ",", "")
  val args = Array(
    "-classpath", ClasspathFromClassloader(getClass.getClassLoader),
    "-from-tasty",
    "-d", s"out${separator}scala2-library-from-tasty-jar-test-output.jar",
    "-nowarn",
    yFromTastyBlacklist,
    scalaLibJarPath,
  )
  val reporter = driver.process(args)
  assert(reporter.errorCount == 0, "Errors while re-compiling")
}

/** Set of tasty files that cannot be recompiled from TASTy */
def compileBlacklisted = Set[String](
  // See #10048
  // failed: java.lang.AssertionError: assertion failed: class Boolean
  //   at dotty.tools.backend.jvm.BCodeHelpers$BCInnerClassGen.assertClassNotArrayNotPrimitive(BCodeHelpers.scala:247)
  //   at dotty.tools.backend.jvm.BCodeHelpers$BCInnerClassGen.getClassBTypeAndRegisterInnerClass(BCodeHelpers.scala:265)
  //   at dotty.tools.backend.jvm.BCodeHelpers$BCInnerClassGen.getClassBTypeAndRegisterInnerClass$(BCodeHelpers.scala:210)
  //   at dotty.tools.backend.jvm.BCodeSkelBuilder$PlainSkelBuilder.getClassBTypeAndRegisterInnerClass(BCodeSkelBuilder.scala:62)
  //   at dotty.tools.backend.jvm.BCodeHelpers$BCInnerClassGen.internalName(BCodeHelpers.scala:237)
  s"scala${separator}Array.tasty",
  s"scala${separator}Boolean.tasty",
  s"scala${separator}Byte.tasty",
  s"scala${separator}Char.tasty",
  s"scala${separator}Double.tasty",
  s"scala${separator}Float.tasty",
  s"scala${separator}Int.tasty",
  s"scala${separator}Long.tasty",
  s"scala${separator}Short.tasty",
  s"scala${separator}Unit.tasty",
)
