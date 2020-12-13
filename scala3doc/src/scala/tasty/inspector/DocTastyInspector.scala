package scala.tasty.inspector

import dotty.tools.dotc.core.Contexts.Context

abstract class DocTastyInspector extends TastyInspector:
  def inspectFilesInDocContext(
    classpath: List[String],
    filePaths: List[String])(
      using Context): Unit = inspectFilesInContext(classpath, filePaths)