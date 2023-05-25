package dotty.tools
package dotc
package fromtasty

import scala.language.unsafeNulls

import io.{JarArchive, AbstractFile, Path}
import core.Contexts.*
import core.Decorators.em
import java.io.File

class TASTYRun(comp: Compiler, ictx: Context) extends Run(comp, ictx) {
  override def compile(files: List[AbstractFile]): Unit = {
    val units = tastyUnits(files)
    compileUnits(units)
  }

  private def tastyUnits(files: List[AbstractFile]): List[TASTYCompilationUnit] =
    val fromTastyIgnoreList = ctx.settings.YfromTastyIgnoreList.value.toSet
    // Resolve class names of tasty and jar files
    val classNames = files.flatMap { file =>
      file.extension match
        case "jar" =>
          JarArchive.open(Path(file.path), create = false).allFileNames()
            .map(_.stripPrefix("/")) // change paths from absolute to relative
            .filter(e => Path.extension(e) == "tasty" && !fromTastyIgnoreList(e.replace("/", File.separator)))
            .map(e => e.stripSuffix(".tasty").replace("/", "."))
            .toList
        case "tasty" => TastyFileUtil.getClassName(file)
        case "betasty" if ctx.withBestEffortTasty => 
          TastyFileUtil.getClassName(file, withBestEffortTasty = true)
        case _ =>
          report.error(em"File extension is not `tasty` or `jar`: ${file.path}")
          Nil
    }
    classNames.map(new TASTYCompilationUnit(_))
}
