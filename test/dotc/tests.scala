package dotc

import org.junit.Test
import test._

class tests extends CompilerTest {

  val noCheckOptions = List(
//        "-verbose",
//         "-Ylog:frontend",
//        "-Xprompt",
//        "-explaintypes",
//        "-Yshow-suppressed-errors",
        "-pagewidth", "160")

  implicit val defaultOptions = noCheckOptions ++ List(
      "-Yno-deep-subtypes",
      "-Ycheck:resolveSuper,mixin,restoreScopes"
  )

  val twice = List("#runs", "2", "-YnoDoubleBindings")
  val doErase = List("-Ystop-before:terminal")
  val allowDeepSubtypes = defaultOptions diff List("-Yno-deep-subtypes")

  val posDir = "./tests/pos/"
  val posSpecialDir = "./tests/pos-special/"
  val negDir = "./tests/neg/"
  val newDir = "./tests/new/"
  val dotcDir = "./src/dotty/"


  @Test def pos_t2168_pat = compileFile(posDir, "t2168", doErase)
  @Test def pos_erasure = compileFile(posDir, "erasure", doErase)
  @Test def pos_Coder() = compileFile(posDir, "Coder", doErase)
  @Test def pos_blockescapes() = compileFile(posDir, "blockescapes", doErase)
  @Test def pos_collections() = compileFile(posDir, "collections", doErase)
  @Test def pos_functions1() = compileFile(posDir, "functions1", doErase)
  @Test def pos_implicits1() = compileFile(posDir, "implicits1", doErase)
  @Test def pos_inferred() = compileFile(posDir, "inferred", doErase)
  @Test def pos_Patterns() = compileFile(posDir, "Patterns")
  @Test def pos_selftypes() = compileFile(posDir, "selftypes", doErase)
  @Test def pos_varargs() = compileFile(posDir, "varargs", doErase)
  @Test def pos_vararg_patterns() = compileFile(posDir, "vararg-pattern", doErase)
  @Test def pos_opassign() = compileFile(posDir, "opassign", doErase)
  @Test def pos_typedapply() = compileFile(posDir, "typedapply", doErase)
  @Test def pos_nameddefaults() = compileFile(posDir, "nameddefaults", doErase)
  @Test def pos_desugar() = compileFile(posDir, "desugar", doErase)
  @Test def pos_sigs() = compileFile(posDir, "sigs", doErase)
  @Test def pos_typers() = compileFile(posDir, "typers", doErase)
  @Test def pos_typedidents() = compileFile(posDir, "typedIdents", doErase)
  @Test def pos_assignments() = compileFile(posDir, "assignments", doErase)
  @Test def pos_packageobject() = compileFile(posDir, "packageobject", doErase)
  @Test def pos_overloaded() = compileFile(posDir, "overloaded", doErase)
  @Test def pos_overrides() = compileFile(posDir, "overrides", doErase)
  @Test def pos_templateParents() = compileFile(posDir, "templateParents", doErase)
  @Test def pos_structural() = compileFile(posDir, "structural", doErase)
  @Test def pos_overloadedAccess = compileFile(posDir, "overloadedAccess", doErase)
  @Test def pos_approximateUnion = compileFile(posDir, "approximateUnion", doErase)
  @Test def pos_tailcall = compileDir(posDir + "tailcall/", doErase)
  @Test def pos_nullarify = compileFile(posDir, "nullarify", "-Ycheck:nullarify" :: doErase)
  @Test def pos_subtyping = compileFile(posDir, "subtyping", doErase)
  @Test def pos_t2613 = compileFile(posSpecialDir, "t2613", doErase)(allowDeepSubtypes)

  @Test def pos_all = compileFiles(posDir, twice)
  @Test def new_all = compileFiles(newDir, twice)

  @Test def neg_blockescapes() = compileFile(negDir, "blockescapesNeg", xerrors = 1)
  @Test def neg_typedapply() = compileFile(negDir, "typedapply", xerrors = 4)
  @Test def neg_typedidents() = compileFile(negDir, "typedIdents", xerrors = 2)
  @Test def neg_assignments() = compileFile(negDir, "assignments", xerrors = 3)
  @Test def neg_typers() = compileFile(negDir, "typers", xerrors = 12)
  @Test def neg_privates() = compileFile(negDir, "privates", xerrors = 2)
  @Test def neg_rootImports = compileFile(negDir, "rootImplicits", xerrors = 2)
  @Test def neg_templateParents() = compileFile(negDir, "templateParents", xerrors = 3)
  @Test def neg_autoTupling = compileFile(posDir, "autoTuplingTest", "-language:noAutoTupling" :: Nil, xerrors = 4)
  @Test def neg_autoTupling2 = compileFile(negDir, "autoTuplingTest", xerrors = 4)
  @Test def neg_companions = compileFile(negDir, "companions", xerrors = 1)
  @Test def neg_over = compileFile(negDir, "over", xerrors = 1)
  @Test def neg_overrides = compileFile(negDir, "overrides", xerrors = 7)
  @Test def neg_projections = compileFile(negDir, "projections", xerrors = 1)
  @Test def neg_i39 = compileFile(negDir, "i39", xerrors = 1)
  @Test def neg_i50_volatile = compileFile(negDir, "i50-volatile", xerrors = 4)
  @Test def neg_t0273_doubledefs = compileFile(negDir, "t0273", xerrors = 1)
  @Test def neg_t0586_structural = compileFile(negDir, "t0586", xerrors = 1)
  @Test def neg_t0625_structural = compileFile(negDir, "t0625", xerrors = 1)(
      defaultOptions = noCheckOptions)
        // -Ycheck fails because there are structural types involving higher-kinded types.
        // these are illegal, but are tested only later.
  @Test def neg_t1131_structural = compileFile(negDir, "t1131", xerrors = 1)
  @Test def neg_zoo = compileFile(negDir, "zoo", xerrors = 1)
  @Test def neg_t1192_legalPrefix = compileFile(negDir, "t1192", xerrors = 1)
  @Test def neg_tailcall_t1672b = compileFile(negDir, "tailcall/t1672b", xerrors = 6)
  @Test def neg_tailcall_t3275 = compileFile(negDir, "tailcall/t3275", xerrors = 1)
  @Test def neg_tailcall_t6574 = compileFile(negDir, "tailcall/t6574", xerrors = 2)
  @Test def neg_tailcall = compileFile(negDir, "tailcall/tailrec", xerrors = 7)
  @Test def neg_tailcall2 = compileFile(negDir, "tailcall/tailrec-2", xerrors = 2)
  @Test def neg_tailcall3 = compileFile(negDir, "tailcall/tailrec-3", xerrors = 2)
  @Test def nef_t1279a = compileFile(negDir, "t1279a", xerrors = 1)
  @Test def neg_t1843_variances = compileFile(negDir, "t1843-variances", xerrors = 1)
  @Test def neg_t2660_ambi = compileFile(negDir, "t2660", xerrors = 2)
  @Test def neg_t2994 = compileFile(negDir, "t2994", xerrors = 2)
  @Test def neg_subtyping = compileFile(negDir, "subtyping", xerrors = 2)
  @Test def neg_variances = compileFile(negDir, "variances", xerrors = 2)
  @Test def neg_badAuxConstr = compileFile(negDir, "badAuxConstr", xerrors = 2)
  @Test def neg_typetest = compileFile(negDir, "typetest", xerrors = 1)
  @Test def neg_t1569_failedAvoid = compileFile(negDir, "t1569-failedAvoid", xerrors = 1)
  @Test def neg_cycles = compileFile(negDir, "cycles", xerrors = 8)
  @Test def neg_boundspropagation = compileFile(negDir, "boundspropagation", xerrors = 4)
  @Test def neg_refinedSubtyping = compileFile(negDir, "refinedSubtyping", xerrors = 2)

  @Test def dotc = compileDir(dotcDir + "tools/dotc", twice)(allowDeepSubtypes)
  @Test def dotc_ast = compileDir(dotcDir + "tools/dotc/ast", twice)
  @Test def dotc_config = compileDir(dotcDir + "tools/dotc/config", twice)
  @Test def dotc_core = compileDir(dotcDir + "tools/dotc/core", twice)(allowDeepSubtypes)
  @Test def dotc_core_pickling = compileDir(dotcDir + "tools/dotc/core/pickling", twice)(allowDeepSubtypes)
  // @Test def dotc_transform = compileDir(dotcDir + "tools/dotc/transform", twice)(allowDeepSubtypes)
  // @odersky causes race error in ResolveSuper

  @Test def dotc_parsing = compileDir(dotcDir + "tools/dotc/parsing", twice)
  @Test def dotc_printing = compileDir(dotcDir + "tools/dotc/printing", twice)
  @Test def dotc_reporting = compileDir(dotcDir + "tools/dotc/reporting", twice)
  @Test def dotc_typer = compileDir(dotcDir + "tools/dotc/typer", twice)
  @Test def dotc_util = compileDir(dotcDir + "tools/dotc/util", twice)
  @Test def tools_io = compileDir(dotcDir + "tools/io", twice)
  //@Test def tools = compileDir(dotcDir + "tools", "-deep" :: Nil)(allowDeepSubtypes)

  @Test def testNonCyclic = compileArgs(Array(
      dotcDir + "tools/dotc/CompilationUnit.scala",
      dotcDir + "tools/dotc/core/Types.scala",
      dotcDir + "tools/dotc/ast/Trees.scala",
      //"-Ylog:frontend",
      "-Xprompt",
      "#runs", "2"))

  @Test def testIssue_34 = compileArgs(Array(
      dotcDir + "tools/dotc/config/Properties.scala",
      dotcDir + "tools/dotc/config/PathResolver.scala",
      //"-Ylog:frontend",
      "-Xprompt",
      "#runs", "2"))

  val javaDir = "./tests/pos/java-interop/"
  @Test def java_all = compileFiles(javaDir)


  //@Test def dotc_compilercommand = compileFile(dotcDir + "tools/dotc/config/", "CompilerCommand")
}
