package dotty.tools
package repl

import java.io.{ StringWriter, PrintWriter }
import java.lang.{ ClassLoader, ExceptionInInitializerError }
import java.lang.reflect.InvocationTargetException

import dotc.ast.tpd
import dotc.core.Contexts._
import dotc.core.Denotations.Denotation
import dotc.core.Flags
import dotc.core.Flags._
import dotc.core.Symbols.{Symbol, defn}
import dotc.core.StdNames.str
import dotc.core.NameOps._
import dotc.printing.ReplPrinter
import dotc.reporting.{MessageRendering, Message, Diagnostic}
import dotc.util.SourcePosition

/** This rendering object uses `ClassLoader`s to accomplish crossing the 4th
 *  wall (i.e. fetching back values from the compiled class files put into a
 *  specific class loader capable of loading from memory) and rendering them.
 *
 *  @pre this object should be paired with a compiler session, i.e. when
 *       `ReplDriver#resetToInitial` is called, the accompanying instance of
 *       `Rendering` is no longer valid.
 */
private[repl] class Rendering(parentClassLoader: Option[ClassLoader] = None) {

  import Rendering._

  private val MaxStringElements: Int = 1000  // no need to mkString billions of elements

  /** A `MessageRenderer` for the REPL without file positions */
  private val messageRenderer = new MessageRendering {
    override def posStr(pos: SourcePosition, diagnosticLevel: String, message: Message)(using Context): String = ""
  }

  private var myClassLoader: ClassLoader = _

  private var myReplStringOf: Object => String = _


  /** Class loader used to load compiled code */
  private[repl] def classLoader()(using Context) =
    if (myClassLoader != null) myClassLoader
    else {
      val parent = parentClassLoader.getOrElse {
        val compilerClasspath = ctx.platform.classPath(using ctx).asURLs
        new java.net.URLClassLoader(compilerClasspath.toArray, null)
      }

      myClassLoader = new AbstractFileClassLoader(ctx.settings.outputDir.value, parent)
      myReplStringOf = {
        // We need to use the ScalaRunTime class coming from the scala-library
        // on the user classpath, and not the one available in the current
        // classloader, so we use reflection instead of simply calling
        // `ScalaRunTime.replStringOf`.
        val scalaRuntime = Class.forName("scala.runtime.ScalaRunTime", true, myClassLoader)
        val meth = scalaRuntime.getMethod("replStringOf", classOf[Object], classOf[Int])

        (value: Object) => meth.invoke(null, value, Integer.valueOf(MaxStringElements)).asInstanceOf[String]
      }
      myClassLoader
    }

  /** Return a String representation of a value we got from `classLoader()`. */
  private[repl] def replStringOf(value: Object)(using Context): String = {
    assert(myReplStringOf != null,
      "replStringOf should only be called on values creating using `classLoader()`, but `classLoader()` has not been called so far")
    myReplStringOf(value)
  }

  /** Load the value of the symbol using reflection.
   *
   *  Calling this method evaluates the expression using reflection
   */
  private def valueOf(sym: Symbol)(using Context): Option[String] = {
    val objectName = sym.owner.fullName.encode.toString.stripSuffix("$")
    val resObj: Class[?] = Class.forName(objectName, true, classLoader())
    val value =
      resObj
        .getDeclaredMethods.find(_.getName == sym.name.encode.toString)
        .map(_.invoke(null))
    val string = value.map(replStringOf(_).trim)
    if (!sym.is(Flags.Method) && sym.info == defn.UnitType)
      None
    else
      string.map { s =>
        if (s.startsWith(str.REPL_SESSION_LINE))
          s.drop(str.REPL_SESSION_LINE.length).dropWhile(c => c.isDigit || c == '$')
        else
          s
      }
  }

  /** Formats errors using the `messageRenderer` */
  def formatError(dia: Diagnostic)(implicit state: State): Diagnostic =
    new Diagnostic(
      messageRenderer.messageAndPos(dia.msg, dia.pos, messageRenderer.diagnosticLevel(dia))(using state.context),
      dia.pos,
      dia.level
    )

  def renderTypeDef(d: Denotation)(using Context): Diagnostic =
    infoDiagnostic("// defined " ++ d.symbol.showUser, d)

  def renderTypeAlias(d: Denotation)(using Context): Diagnostic =
    infoDiagnostic("// defined alias " ++ d.symbol.showUser, d)

  /** Render method definition result */
  def renderMethod(d: Denotation)(using Context): Diagnostic =
    infoDiagnostic(d.symbol.showUser, d)

  /** Render value definition result */
  def renderVal(d: Denotation)(using Context): Option[Diagnostic] = {
    val dcl = d.symbol.showUser

    try {
      if (d.symbol.is(Flags.Lazy)) Some(infoDiagnostic(dcl, d))
      else valueOf(d.symbol).map(value => infoDiagnostic(s"$dcl = $value", d))
    }
    catch { case ex: InvocationTargetException => Some(infoDiagnostic(renderError(ex), d)) }
  }

  /** Render the stack trace of the underlying exception */
  private def renderError(ex: InvocationTargetException): String = {
    val cause = ex.getCause match {
      case ex: ExceptionInInitializerError => ex.getCause
      case ex => ex
    }
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    cause.printStackTrace(pw)
    sw.toString
  }

  private def infoDiagnostic(msg: String, d: Denotation)(using Context): Diagnostic =
    new Diagnostic.Info(msg, d.symbol.sourcePos)

}

object Rendering {

  extension (s: Symbol)
    def showUser(using Context): String = {
      val printer = new ReplPrinter(ctx)
      val text = printer.dclText(s)
      text.mkString(ctx.settings.pageWidth.value, ctx.settings.printLines.value)
    }

}
