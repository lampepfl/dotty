package dotty.tools.backend.jvm

import scala.collection.mutable.ListBuffer
import dotty.tools.dotc.util.{SourcePosition, NoSourcePosition}
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.em
import scala.tools.asm.ClassWriter
import scala.tools.asm.tree.ClassNode

/**
 * Implements late stages of the backend that don't depend on a Global instance, i.e.,
 * optimizations, post-processing and classfile serialization and writing.
 */
class PostProcessor(val frontendAccess: PostProcessorFrontendAccess, val bTypes: BTypes) {
  self =>
  import bTypes.*
  import frontendAccess.{backendReporting, compilerSettings}
  import int.given

  val backendUtils = new BackendUtils(this)
  val classfileWriter  = ClassfileWriter(frontendAccess)

  def postProcessAndSendToDisk(generatedDefs: GeneratedDefs): Unit = {
    val GeneratedDefs(classes, tasty) = generatedDefs
    if !ctx.settings.YoutputOnlyTasty.value then
      postProcessClassesAndSendToDisk(classes)
    postProcessTastyAndSendToDisk(tasty)
  }

  private def postProcessClassesAndSendToDisk(classes: List[GeneratedClass]): Unit = {
    for (GeneratedClass(classNode, sourceFile, isArtifact, onFileCreated) <- classes) {
      val bytes =
        try
          if !isArtifact then setSerializableLambdas(classNode)
          setInnerClasses(classNode)
          serializeClass(classNode)
        catch
          case e: java.lang.RuntimeException if e.getMessage != null && e.getMessage.nn.contains("too large!") =>
            backendReporting.error(em"Could not write class ${classNode.name} because it exceeds JVM code size limits. ${e.getMessage}")
            null
          case ex: Throwable =>
            ex.printStackTrace()
            backendReporting.error(em"Error while emitting ${classNode.name}\n${ex.getMessage}")
            null

      if (bytes != null) {
        if (AsmUtils.traceSerializedClassEnabled && classNode.name.nn.contains(AsmUtils.traceSerializedClassPattern))
          AsmUtils.traceClass(bytes)

        val clsFile = classfileWriter.writeClass(classNode.name.nn, bytes, sourceFile)
        if clsFile != null then onFileCreated(clsFile)
      }
    }
  }

  private def postProcessTastyAndSendToDisk(tasty: List[GeneratedTasty]): Unit = {
    for (GeneratedTasty(classNode, binaryGen) <- tasty) {
      classfileWriter.writeTasty(classNode.name.nn, binaryGen())
    }
  }

  private def setSerializableLambdas(classNode: ClassNode): Unit = {
    import backendUtils.{collectSerializableLambdas, addLambdaDeserialize}
    val serializableLambdas = collectSerializableLambdas(classNode)
    if serializableLambdas.nonEmpty then
      addLambdaDeserialize(classNode, serializableLambdas)
  }

  private def setInnerClasses(classNode: ClassNode): Unit = {
    import backendUtils.{collectNestedClasses, addInnerClasses}
    classNode.innerClasses.nn.clear()
    val (declared, referred) = collectNestedClasses(classNode)
    addInnerClasses(classNode, declared, referred)
  }

  def serializeClass(classNode: ClassNode): Array[Byte] = {
    val cw = new ClassWriterWithBTypeLub(backendUtils.extraProc)
    classNode.accept(cw)
    cw.toByteArray.nn
  }

  // -----------------------------------------------------------------------------------------
  // finding the least upper bound in agreement with the bytecode verifier (given two internal names handed by ASM)
  // Background:
  //  http://gallium.inria.fr/~xleroy/publi/bytecode-verification-JAR.pdf
  //  http://comments.gmane.org/gmane.comp.java.vm.languages/2293
  //  https://github.com/scala/bug/issues/3872
  // -----------------------------------------------------------------------------------------

  /*  An `asm.ClassWriter` that uses `jvmWiseLUB()`
   *  The internal name of the least common ancestor of the types given by inameA and inameB.
   *  It's what ASM needs to know in order to compute stack map frames, http://asm.ow2.org/doc/developer-guide.html#controlflow
   */
  final class ClassWriterWithBTypeLub(flags: Int) extends ClassWriter(flags) {

    /**
     * This method is used by asm when computing stack map frames. It is thread-safe: it depends
     * only on the BTypes component, which does not depend on global.
     * TODO @lry move to a different place where no global is in scope, on bTypes.
     */
    override def getCommonSuperClass(inameA: String, inameB: String): String = {
      // All types that appear in a class node need to have their ClassBType cached, see [[cachedClassBType]].
      val a = classBTypeFromInternalName(inameA)
      val b = classBTypeFromInternalName(inameB)
      val lub = a.jvmWiseLUB(b)
      val lubName = lub.internalName
      assert(lubName != "scala/Any")
      lubName // ASM caches the answer during the lifetime of a ClassWriter. We outlive that. Not sure whether caching on our side would improve things.
    }
  }
}

/**
 * The result of code generation. [[isArtifact]] is `true` for mirror.
 */
case class GeneratedClass(classNode: ClassNode, sourceFile: AbstractFile, isArtifact: Boolean, onFileCreated: AbstractFile => Unit)
case class GeneratedTasty(classNode: ClassNode, tastyGen: () => Array[Byte])
case class GeneratedDefs(classes: List[GeneratedClass], tasty: List[GeneratedTasty])

// Temporary class, will be refactored in a future commit
trait ClassWriterForPostProcessor {
  type InternalName = String
  def write(bytes: Array[Byte], className: InternalName, sourceFile: AbstractFile): Unit
}
