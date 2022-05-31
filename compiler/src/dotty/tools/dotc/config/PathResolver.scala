package dotty.tools
package dotc
package config

import scala.language.unsafeNulls

import WrappedProperties.AccessControl
import io.{ClassPath, Directory, Path}
import classpath.{AggregateClassPath, ClassPathFactory, JrtClassPath}
import ClassPath.split
import PartialFunction.condOpt
import core.Contexts._
import Settings._
import dotty.tools.io.File

object PathResolver {

  // Imports property/environment functions which suppress
  // security exceptions.
  import AccessControl._

  def firstNonEmpty(xs: String*): String = xs find (_ != "") getOrElse ""

  /** Map all classpath elements to absolute paths and reconstruct the classpath.
   */
  def makeAbsolute(cp: String): String = ClassPath.map(cp, x => Path(x).toAbsolute.path)

  /** pretty print class path
   */
  def ppcp(s: String): String = split(s) match {
    case Nil      => ""
    case Seq(x)   => x
    case xs       => xs.map("\n" + _).mkString
  }

  /** Values found solely by inspecting environment or property variables.
   */
  object Environment {
    private def searchForBootClasspath = (
      systemProperties find (_._1 endsWith ".boot.class.path") map (_._2) getOrElse ""
    )

    /** Environment variables which java pays attention to so it
     *  seems we do as well.
     */
    def classPathEnv: String        =  envOrElse("CLASSPATH", "")
    def sourcePathEnv: String       =  envOrElse("SOURCEPATH", "")

    def javaBootClassPath: String   = propOrElse("sun.boot.class.path", searchForBootClasspath)

    def javaExtDirs: String         = propOrEmpty("java.ext.dirs")
    def scalaHome: String           = propOrEmpty("scala.home")
    def scalaExtDirs: String        = propOrEmpty("scala.ext.dirs")

    /** The java classpath and whether to use it.
     */
    def javaUserClassPath: String   = propOrElse("java.class.path", "")
    def useJavaClassPath: Boolean    = propOrFalse("scala.usejavacp")

    override def toString: String = s"""
      |object Environment {
      |  scalaHome          = $scalaHome (useJavaClassPath = $useJavaClassPath)
      |  javaBootClassPath  = <${javaBootClassPath.length} chars>
      |  javaExtDirs        = ${ppcp(javaExtDirs)}
      |  javaUserClassPath  = ${ppcp(javaUserClassPath)}
      |  scalaExtDirs       = ${ppcp(scalaExtDirs)}
      |}""".trim.stripMargin
  }

  /** Default values based on those in Environment as interpreted according
   *  to the path resolution specification.
   */
  object Defaults {
    def scalaSourcePath: String    = Environment.sourcePathEnv
    def javaBootClassPath: String  = Environment.javaBootClassPath
    def javaUserClassPath: String  = Environment.javaUserClassPath
    def javaExtDirs: String        = Environment.javaExtDirs
    def useJavaClassPath: Boolean  = Environment.useJavaClassPath

    def scalaHome: String          = Environment.scalaHome
    def scalaHomeDir: Directory    = Directory(scalaHome)
    def scalaHomeExists: Boolean   = scalaHomeDir.isDirectory
    def scalaLibDir: Directory     = (scalaHomeDir / "lib").toDirectory
    def scalaClassesDir: Directory = (scalaHomeDir / "classes").toDirectory

    def scalaLibAsJar: File        = (scalaLibDir / "scala-library.jar").toFile
    def scalaLibAsDir: Directory   = (scalaClassesDir / "library").toDirectory

    def scalaLibDirFound: Option[Directory] =
      if (scalaLibAsJar.isFile) Some(scalaLibDir)
      else if (scalaLibAsDir.isDirectory) Some(scalaClassesDir)
      else None

    def scalaLibFound: String =
      if (scalaLibAsJar.isFile) scalaLibAsJar.path
      else if (scalaLibAsDir.isDirectory) scalaLibAsDir.path
      else ""

    // XXX It must be time for someone to figure out what all these things
    // are intended to do.  This is disabled here because it was causing all
    // the scala jars to end up on the classpath twice: one on the boot
    // classpath as set up by the runner (or regular classpath under -nobootcp)
    // and then again here.
    def scalaBootClassPath: String  = ""
    // scalaLibDirFound match {
    //   case Some(dir) if scalaHomeExists =>
    //     val paths = ClassPath expandDir dir.path
    //     join(paths: _*)
    //   case _                            => ""
    // }

    def scalaExtDirs: String = Environment.scalaExtDirs

    def scalaPluginPath: String = (scalaHomeDir / "misc" / "scala-devel" / "plugins").path

    override def toString: String = """
      |object Defaults {
      |  scalaHome            = %s
      |  javaBootClassPath    = %s
      |  scalaLibDirFound     = %s
      |  scalaLibFound        = %s
      |  scalaBootClassPath   = %s
      |  scalaPluginPath      = %s
      |}""".trim.stripMargin.format(
        scalaHome,
        ppcp(javaBootClassPath),
        scalaLibDirFound, scalaLibFound,
        ppcp(scalaBootClassPath), ppcp(scalaPluginPath)
      )
  }

  def fromPathString(path: String)(using Context): ClassPath = {
    val settings = ctx.settings.classpath.update(path)
    inContext(ctx.fresh.setSettings(settings)) {
      new PathResolver().result
    }
  }

  /** Show values in Environment and Defaults when no argument is provided.
   *  Otherwise, show values in Calculated as if those options had been given
   *  to a scala runner.
   */
  def main(args: Array[String]): Unit =
    if (args.isEmpty) {
      println(Environment)
      println(Defaults)
    }
    else inContext(ContextBase().initialCtx) {
      val ArgsSummary(sstate, rest, errors, warnings) =
        ctx.settings.processArguments(args.toList, true, ctx.settingsState)
      errors.foreach(println)
      val pr = inContext(ctx.fresh.setSettings(sstate)) {
        new PathResolver()
      }
      println(" COMMAND: 'scala %s'".format(args.mkString(" ")))
      println("RESIDUAL: 'scala %s'\n".format(rest.mkString(" ")))

      pr.result match {
        case cp: AggregateClassPath =>
          println(s"ClassPath has ${cp.aggregates.size} entries and results in:\n${cp.asClassPathStrings}")
      }
    }
}

import PathResolver.{Defaults, ppcp}

class PathResolver(using c: Context) {
  import c.base.settings

  private val classPathFactory = new ClassPathFactory

  private def cmdLineOrElse(name: String, alt: String) =
    commandLineFor(name) match {
      case Some("") | None => alt
      case Some(x)         => x
    }

  private def commandLineFor(s: String): Option[String] = condOpt(s) {
    case "javabootclasspath"  => settings.javabootclasspath.value
    case "javaextdirs"        => settings.javaextdirs.value
    case "bootclasspath"      => settings.bootclasspath.value
    case "extdirs"            => settings.extdirs.value
    case "classpath" | "cp"   => settings.classpath.value
    case "sourcepath"         => settings.sourcepath.value
  }

  /** Calculated values based on any given command line options, falling back on
   *  those in Defaults.
   */
  object Calculated {
    def scalaHome: String           = Defaults.scalaHome
    def useJavaClassPath: Boolean   = settings.usejavacp.value || Defaults.useJavaClassPath
    def javaBootClassPath: String   = cmdLineOrElse("javabootclasspath", Defaults.javaBootClassPath)
    def javaExtDirs: String         = cmdLineOrElse("javaextdirs", Defaults.javaExtDirs)
    def javaUserClassPath: String   = if (useJavaClassPath) Defaults.javaUserClassPath else ""
    def scalaBootClassPath: String  = cmdLineOrElse("bootclasspath", Defaults.scalaBootClassPath)
    def scalaExtDirs: String        = cmdLineOrElse("extdirs", Defaults.scalaExtDirs)
    /** Scaladoc doesn't need any bootstrapping, otherwise will create errors such as:
     * [scaladoc] ../scala-trunk/src/reflect/scala/reflect/macros/Reifiers.scala:89: error: object api is not a member of package reflect
     * [scaladoc] case class ReificationException(val pos: reflect.api.PositionApi, val msg: String) extends Throwable(msg)
     * [scaladoc]                                              ^
     * Because bootstrapping looks at the sourcepath and creates the package "reflect" in "<root>" it will cause the
     * typedIdentifier to pick <root>.reflect instead of the <root>.scala.reflect package.  Thus, no bootstrapping for scaladoc!
     */
    def sourcePath: String          = cmdLineOrElse("sourcepath", Defaults.scalaSourcePath)

    def userClassPath: String =
      if (!settings.classpath.isDefault) settings.classpath.value
      else sys.env.getOrElse("CLASSPATH", ".")

    import classPathFactory._

    // Assemble the elements!
    def basis: List[Iterable[ClassPath]] =
      val release = Option(ctx.settings.javaOutputVersion.value).filter(_.nonEmpty)

      List(
        JrtClassPath(release),                        // 1. The Java 9+ classpath (backed by the jrt:/ virtual system, if available)
        classesInPath(javaBootClassPath),             // 2. The Java bootstrap class path.
        contentsOfDirsInPath(javaExtDirs),            // 3. The Java extension class path.
        classesInExpandedPath(javaUserClassPath),     // 4. The Java application class path.
        classesInPath(scalaBootClassPath),            // 5. The Scala boot class path.
        contentsOfDirsInPath(scalaExtDirs),           // 6. The Scala extension class path.
        classesInExpandedPath(userClassPath),         // 7. The Scala application class path.
        sourcesInPath(sourcePath)                     // 8. The Scala source path.
      )

    lazy val containers: List[ClassPath] = basis.flatten.distinct

    override def toString: String = """
      |object Calculated {
      |  scalaHome            = %s
      |  javaBootClassPath    = %s
      |  javaExtDirs          = %s
      |  javaUserClassPath    = %s
      |  useJavaClassPath     = %s
      |  scalaBootClassPath   = %s
      |  scalaExtDirs         = %s
      |  userClassPath        = %s
      |  sourcePath           = %s
      |}""".trim.stripMargin.format(
        scalaHome,
        ppcp(javaBootClassPath), ppcp(javaExtDirs), ppcp(javaUserClassPath),
        useJavaClassPath,
        ppcp(scalaBootClassPath), ppcp(scalaExtDirs), ppcp(userClassPath),
        ppcp(sourcePath)
      )
  }

  def containers: List[ClassPath] = Calculated.containers

  lazy val result: ClassPath = {
    val cp = AggregateClassPath(containers.toIndexedSeq)

    if (settings.YlogClasspath.value) {
      Console.println("Classpath built from " + settings.toConciseString(ctx.settingsState))
      Console.println("Defaults: " + PathResolver.Defaults)
      Console.println("Calculated: " + Calculated)

      val xs = (Calculated.basis drop 2).flatten.distinct
      println("After java boot/extdirs classpath has %d entries:" format xs.size)
      xs foreach (x => println("  " + x))
    }
    cp
  }

  def asURLs: Seq[java.net.URL] = result.asURLs
}
