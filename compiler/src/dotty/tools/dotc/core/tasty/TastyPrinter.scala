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

import scala.compiletime.uninitialized
import dotty.tools.tasty.TastyBuffer.Addr
import dotty.tools.dotc.classpath.FileUtils.hasTastyExtension

object TastyPrinter:

  def showContents(bytes: Array[Byte], noColor: Boolean): String =
    val printer =
      if noColor then new TastyPrinter(bytes)
      else new TastyAnsiiPrinter(bytes)
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

class TastyPrinter(bytes: Array[Byte]) {

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

  private def nameToString(name: Name): String = name.debugString

  private def nameRefToString(ref: NameRef): String = nameToString(nameAtRef(ref))

  private def printHeader(sb: StringBuilder): Unit =
    val header = unpickler.header
    sb.append("Header:\n")
    sb.append(s"  version: ${header.majorVersion}.${header.minorVersion}.${header.experimentalVersion}\n")
    sb.append("  tooling: ").append(header.toolingVersion).append("\n")
    sb.append("     UUID: ").append(header.uuid).append("\n")
    sb.append("\n")

  private def printNames(sb: StringBuilder): Unit =
    sb.append(s"Names (${unpickler.namesEnd.index - unpickler.namesStart.index} bytes, starting from ${unpickler.namesStart.index}):\n")
    for ((name, idx) <- nameAtRef.contents.zipWithIndex) {
      val index = nameStr("%6d".format(idx))
      sb.append(index).append(": ").append(nameToString(name)).append("\n")
    }

  def showContents(): String = {
    val sb: StringBuilder = new StringBuilder
    printHeader(sb)
    printNames(sb)
    unpickle(new TreeSectionUnpickler(sb))
    unpickle(new PositionSectionUnpickler(sb))
    unpickle(new CommentSectionUnpickler(sb))
    unpickle(new AttributesSectionUnpickler(sb))
    sb.result
  }

  class TreeSectionUnpickler(sb: StringBuilder) extends SectionUnpickler[Unit](ASTsSection) {
    import dotty.tools.tasty.TastyFormat.*
    def unpickle(reader: TastyReader, tastyName: NameTable): Unit = {
      import reader.*
      var indent = 0
      def newLine() = {
        val length = treeStr("%6d".format(index(currentAddr) - index(startAddr)))
        sb.append(s"\n$length:" + " " * indent)
      }
      def printNat() = sb.append(treeStr(" " + readNat()))
      def printName() = {
        val idx = readNat()
        sb.append(nameStr(" " + idx + " [" + nameRefToString(NameRef(idx)) + "]"))
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
      sb.append(s"\n\nTrees (${endAddr.index - startAddr.index} bytes, starting from $base):")
      while (!isAtEnd) {
        printTree()
        newLine()
      }
    }
  }

  class PositionSectionUnpickler(sb: StringBuilder) extends SectionUnpickler[Unit](PositionsSection) {
    def unpickle(reader: TastyReader, tastyName: NameTable): Unit = {
      import reader.*
      val posUnpickler = new PositionUnpickler(reader, tastyName)
      sb.append(s"\n\nPositions (${reader.endAddr.index - reader.startAddr.index} bytes, starting from $base):\n")
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

  class CommentSectionUnpickler(sb: StringBuilder) extends SectionUnpickler[Unit](CommentsSection) {
    def unpickle(reader: TastyReader, tastyName: NameTable): Unit = {
      import reader.*
      val comments = new CommentUnpickler(reader).comments
      if !comments.isEmpty then
        sb.append(s"\n\nComments (${reader.endAddr.index - reader.startAddr.index} bytes, starting from $base):\n")
        val sorted = comments.toSeq.sortBy(_._1.index)
        for ((addr, cmt) <- sorted) {
          sb.append(treeStr("%6d".format(addr.index)))
          sb.append(s": ${cmt.raw} (expanded = ${cmt.isExpanded})\n")
        }
    }
  }

  class AttributesSectionUnpickler(sb: StringBuilder) extends SectionUnpickler[Unit](AttributesSection) {
    import dotty.tools.tasty.TastyFormat.*
    def unpickle(reader: TastyReader, tastyName: NameTable): Unit = {
      import reader.*
      sb.append(s"\n\nAttributes (${reader.endAddr.index - reader.startAddr.index} bytes, starting from $base):\n")
      while !isAtEnd do
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

  protected def nameStr(str: String): String = str
  protected def treeStr(str: String): String = str
  protected def lengthStr(str: String): String = str
}
