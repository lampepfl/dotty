package dotty.tools.dotc
package core
package tasty

import dotty.tools.tasty.{TastyBuffer, TastyReader}
import TastyBuffer.NameRef

import Contexts.*, Decorators.*
import Names.Name
import TastyUnpickler.*
import util.Spans.offsetToInt
import dotty.tools.tasty.TastyFormat.{ASTsSection, PositionsSection, CommentsSection, AttributesSection}
import java.nio.file.{Files, Paths}
import dotty.tools.io.{JarArchive, Path}
import dotty.tools.tasty.TastyFormat.header
import scala.collection.immutable.BitSet

import scala.compiletime.uninitialized
import dotty.tools.tasty.TastyBuffer.Addr
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.classpath.FileUtils.hasTastyExtension

object TastyPrinter:

  def showContents(bytes: Array[Byte], noColor: Boolean): String =
    showContents(bytes, noColor, testPickler = false)

  def showContents(bytes: Array[Byte], noColor: Boolean, testPickler: Boolean = false): String =
    val printer =
      if noColor then new TastyPrinter(bytes, testPickler)
      else new TastyAnsiiPrinter(bytes, testPickler)
    printer.showContents()

  def main(args: Array[String]): Unit = {
    // TODO: Decouple CliCommand from Context and use CliCommand.distill?
    val lineWidth = 80
    val line = "-" * lineWidth
    val noColor = args.contains("-color:never")
    var printLastLine = false
    def printTasty(fileName: String, bytes: Array[Byte]): Unit =
      println(line)
      println(fileName)
      println(line)
      println(showContents(bytes, noColor))
      println()
      printLastLine = true
    for arg <- args do
      if arg == "-color:never" then () // skip
      else if arg.startsWith("-") then println(s"bad option '$arg' was ignored")
      else if arg.endsWith(".tasty") then
        val path = Paths.get(arg)
        if Files.exists(path) then
          printTasty(arg, Files.readAllBytes(path).nn)
        else
          println("File not found: " + arg)
          System.exit(1)
      else if arg.endsWith(".jar") then
        val jar = JarArchive.open(Path(arg), create = false)
        try
          for file <- jar.iterator() if file.hasTastyExtension do
            printTasty(s"$arg ${file.path}", file.toByteArray)
        finally jar.close()
      else
        println(s"Not a '.tasty' or '.jar' file: $arg")
        System.exit(1)

    if printLastLine then
      println(line)
  }

class TastyPrinter(bytes: Array[Byte], val testPickler: Boolean) {

  def this(bytes: Array[Byte]) = this(bytes, testPickler = false)

  class TastyPrinterUnpickler extends TastyUnpickler(bytes) {
    var namesStart: Addr = uninitialized
    var namesEnd: Addr = uninitialized
    override def readNames() = {
      namesStart = reader.currentAddr
      super.readNames()
      namesEnd = reader.currentAddr
    }
  }

  private val unpickler: TastyPrinterUnpickler = new TastyPrinterUnpickler
  import unpickler.{nameAtRef, unpickle}

  private def printHeader(sb: StringBuilder): Unit =
    val header = unpickler.header
    sb.append("Header:\n")
    if testPickler then
      // these fields are not stable when the TASTy/compiler versions change, so not useful for testing
      sb.append("  version: <elided>\n")
      sb.append("  tooling: <elided>\n")
      sb.append("     UUID: <elided>\n")
    else
      sb.append(s"  version: ${header.majorVersion}.${header.minorVersion}.${header.experimentalVersion}\n")
      sb.append("  tooling: ").append(header.toolingVersion).append("\n")
      sb.append("     UUID: ").append(header.uuid).append("\n")
    end if

  private def printNames(sb: StringBuilder)(using refs: NameRefs): Unit =
    sb.append(sectionHeader(
      name = "Names",
      count = (unpickler.namesEnd.index - unpickler.namesStart.index).toString,
      base = showBase(unpickler.namesStart.index),
      lineEnd = true
    ))
    for ((name, idx) <- nameAtRef.contents.zipWithIndex) {
      val index = nameStr("%6d".format(idx))
      sb.append(index).append(": ").append(refs.nameRefToString(NameRef(idx))).append("\n")
    }

  def showContents(): String = {
    val sb: StringBuilder = new StringBuilder
    given NameRefs = unpickle0(new SourceFileUnpickler)(using NameRefs.empty).getOrElse(NameRefs.empty)
    printHeader(sb)
    printNames(sb)
    unpickle0(new TreeSectionUnpickler(sb))
    unpickle0(new PositionSectionUnpickler(sb))
    unpickle0(new CommentSectionUnpickler(sb))
    unpickle0(new AttributesSectionUnpickler(sb))
    sb.result
  }

  def unpickle0[R](sec: PrinterSectionUnpickler[R])(using NameRefs): Option[R] =
    unpickle(new SectionUnpickler[R](sec.name) {
      def unpickle(reader: TastyReader, nameAtRef: NameTable): R =
        sec.unpickle0(reader.subReader(reader.startAddr, reader.endAddr)) // fork so we can visit multiple times
    })

  class TreeSectionUnpickler(sb: StringBuilder) extends PrinterSectionUnpickler[Unit](ASTsSection) {
    import dotty.tools.tasty.TastyFormat.*
    def unpickle0(reader: TastyReader)(using refs: NameRefs): Unit = {
      import reader.*
      var indent = 0
      def newLine() = {
        val length = treeStr("%6d".format(index(currentAddr) - index(startAddr)))
        sb.append(s"\n$length:" + " " * indent)
      }
      def printNat() = sb.append(treeStr(" " + readNat()))
      def printName() = {
        val idx = readNat()
        sb.append(nameStr(" " + idx + " [" + refs.nameRefToString(NameRef(idx)) + "]"))
      }
      def printTree(): Unit = {
        newLine()
        val tag = readByte()
        sb.append(" ").append(astTagToString(tag))
        indent += 2
        if (tag >= firstLengthTreeTag) {
          val len = readNat()
          sb.append(s"(${lengthStr(len.toString)})")
          val end = currentAddr + len
          def printTrees() = until(end)(printTree())
          tag match {
            case RENAMED =>
              printName(); printName()
            case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | NAMEDARG | BIND =>
              printName(); printTrees()
            case REFINEDtype | TERMREFin | TYPEREFin | SELECTin =>
              printName(); printTree(); printTrees()
            case RETURN | HOLE =>
              printNat(); printTrees()
            case METHODtype | POLYtype | TYPELAMBDAtype =>
              printTree()
              while (currentAddr.index < end.index && !isModifierTag(nextByte)) { printTree(); printName(); }
              printTrees()
            case PARAMtype =>
              printNat(); printNat()
            case _ =>
              printTrees()
          }
          if (currentAddr != end) {
            sb.append(s"incomplete read, current = $currentAddr, end = $end\n")
            goto(end)
          }
        }
        else if (tag >= firstNatASTTreeTag) {
          tag match {
            case IDENT | IDENTtpt | SELECT | SELECTtpt | TERMREF | TYPEREF | SELFDEF => printName()
            case _ => printNat()
          }
          printTree()
        }
        else if (tag >= firstASTTreeTag)
          printTree()
        else if (tag >= firstNatTreeTag)
          tag match {
            case TERMREFpkg | TYPEREFpkg | STRINGconst | IMPORTED => printName()
            case _ => printNat()
          }
        indent -= 2
      }
      sb.append(sectionHeader("Trees", reader, lineEnd = false))
      while (!isAtEnd) {
        printTree()
        newLine()
      }
      sb.append("\n")
    }
  }

  class PositionSectionUnpickler(sb: StringBuilder) extends PrinterSectionUnpickler[Unit](PositionsSection) {
    def unpickle0(reader: TastyReader)(using tastyName: NameRefs): Unit = {
      import reader.*
      val posUnpickler = new PositionUnpickler(reader, tastyName)
      sb.append(sectionHeader("Positions", reader))
      val lineSizes = posUnpickler.lineSizes
      sb.append(s"  lines: ${lineSizes.length}\n")
      sb.append(s"  line sizes:\n")
      val windowSize = 20
      for window <-posUnpickler.lineSizes.sliding(windowSize, windowSize) do
        sb.append("     ").append(window.mkString(", ")).append("\n")
      // sb.append(posUnpickler.lineSizes.mkString("  line sizes: ", ", ", "\n"))
      sb.append("  positions:\n")
      val spans = posUnpickler.spans
      val sorted = spans.toSeq.sortBy(_._1.index)
      for ((addr, pos) <- sorted) {
        sb.append(treeStr("%6d".format(addr.index)))
        sb.append(s": ${offsetToInt(pos.start)} .. ${pos.end}\n")
      }

      val sources = posUnpickler.sourceNameRefs
      sb.append(s"\n  source paths:\n")
      val sortedPath = sources.toSeq.sortBy(_._1.index)
      for ((addr, nameRef) <- sortedPath) {
        sb.append(treeStr("%6d".format(addr.index)))
        sb.append(": ")
        sb.append(nameStr(s"${nameRef.index} [${tastyName(nameRef)}]"))
        sb.append("\n")
      }
    }
  }

  class CommentSectionUnpickler(sb: StringBuilder) extends PrinterSectionUnpickler[Unit](CommentsSection) {
    def unpickle0(reader: TastyReader)(using NameRefs): Unit = {
      import reader.*
      val comments = new CommentUnpickler(reader).comments
      if !comments.isEmpty then
        sb.append(sectionHeader("Comments", reader))
        val sorted = comments.toSeq.sortBy(_._1.index)
        for ((addr, cmt) <- sorted) {
          sb.append(treeStr("%6d".format(addr.index)))
          sb.append(s": ${cmt.raw} (expanded = ${cmt.isExpanded})\n")
        }
    }
  }

  class AttributesSectionUnpickler(sb: StringBuilder) extends PrinterSectionUnpickler[Unit](AttributesSection) {
    import dotty.tools.tasty.TastyFormat.*
    def unpickle0(reader: TastyReader)(using nameAtRef: NameRefs): Unit = {
      import reader.*
      sb.append(sectionHeader("Attributes", reader))
      while !isAtEnd do
        // TODO: Should we elide attributes under testPickler? (i.e.
        //   if we add new attributes many check files will need to be updated)
        val tag = readByte()
        sb.append("  ").append(attributeTagToString(tag))
        if isBooleanAttrTag(tag) then ()
        else if isStringAttrTag(tag) then
          val utf8Ref = readNameRef()
          val value = nameAtRef(utf8Ref).toString
          sb.append(nameStr(s" ${utf8Ref.index} [$value]"))
        sb.append("\n")
      sb.result
    }
  }

  class NameRefs(sourceFileRefs: Set[NameRef]) extends (NameRef => TermName):
    private val isSourceFile = sourceFileRefs.map(_.index).to(BitSet)

    def nameRefToString(ref: NameRef): String = this(ref).debugString

    def apply(ref: NameRef): TermName =
      if isSourceFile(ref.index) then NameRefs.elidedSourceFile
      else nameAtRef(ref)

  object NameRefs:
    import dotty.tools.dotc.core.Names.termName

    private val elidedSourceFile = termName("<elided source file name>")
    val empty = NameRefs(Set.empty)


  class SourceFileUnpickler extends PrinterSectionUnpickler[NameRefs](PositionsSection) {
    def unpickle0(reader: TastyReader)(using nameAtRef: NameRefs): NameRefs = {
      if !testPickler then return NameRefs.empty
      val buf = Set.newBuilder[NameRef]
      val posUnpickler = new PositionUnpickler(reader, nameAtRef)
      val sources = posUnpickler.sourceNameRefs
      for ((_, nameRef) <- sources.iterator) {
        buf += nameRef
      }
      NameRefs(buf.result)
    }
  }

  private final def showBase(index: Int): String =
    if testPickler then "<elided base index>" else index.toString()

  private final def sectionHeader(name: String, reader: TastyReader, lineEnd: Boolean = true): String =
    val count = reader.endAddr.index - reader.startAddr.index
    sectionHeader(name, count.toString, {showBase(reader.base)}, lineEnd)

  private final def sectionHeader(name: String, count: String, base: String, lineEnd: Boolean): String =
    val suffix = if lineEnd then "\n" else ""
    s"\n$name ($count bytes, starting from $base):$suffix"

  abstract class PrinterSectionUnpickler[T](val name: String) {
    def unpickle0(reader: TastyReader)(using refs: NameRefs): T
  }

  protected def nameStr(str: String): String = str
  protected def treeStr(str: String): String = str
  protected def lengthStr(str: String): String = str
}
