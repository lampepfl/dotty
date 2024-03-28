package dotty.tools.dotc
package config

import CommandLineParser.tokenize
import Settings._
import dotty.tools.dotc.config.ScalaSettingCategories._
import org.junit.Test
import org.junit.Assert._
import core.Decorators.toMessage

class ScalaSettingsTests:

  @Test def `A setting with aliases is accepted`: Unit =
    class MySettings extends SettingGroup:
      val classpath: Setting[String] = PathSetting(RootSetting, "classpath", "Specify where to find user class files.", ".", aliases = List("--class-path", "-cp"))
    val settings = MySettings()
    val args = tokenize("-cp path/to/classes1:other/path/to/classes2")
    val summary = ArgsSummary(settings.defaultState, args, errors = Nil, warnings = Nil)
    val res  = settings.processArguments(summary, processAll = true, skipped = Nil)
    val classpath = settings.classpath.valueIn(res.sstate)
    assertEquals(2, args.length)
    assertTrue(s"found warnings: ${res.warnings}", res.warnings.isEmpty)
    assertTrue(s"found errors: ${res.errors}", res.errors.isEmpty)
    assertTrue("wrong classpath", classpath == "path/to/classes1:other/path/to/classes2")

  @Test def `A multistring setting is multivalued`: Unit =
    class SUT extends SettingGroup:
      val language: Setting[List[String]] = MultiStringSetting(RootSetting, "language", "feature", "Enable one or more language features.")
    val sut  = SUT()
    val args = tokenize("-language:implicitConversions,dynamics")
    val sumy = ArgsSummary(sut.defaultState, args, errors = Nil, warnings = Nil)
    val res  = sut.processArguments(sumy, processAll = true, skipped = Nil)
    val set  = sut.language.valueIn(res.sstate)
    assertEquals(1, args.length)
    assertTrue("No warnings!", res.warnings.isEmpty)
    assertTrue("No errors!", res.errors.isEmpty)
    assertTrue("Has the feature", set.contains("implicitConversions"))
    assertTrue("Has the feature", set.contains("dynamics"))

  @Test def `t9719 Apply -language more than once`: Unit =
    class SUT extends SettingGroup:
      val language: Setting[List[String]] = MultiStringSetting(RootSetting, "language", "feature", "Enable one or more language features.")
    val sut  = SUT()
    val args = tokenize("-language:implicitConversions -language:dynamics")
    val sumy = ArgsSummary(sut.defaultState, args, errors = Nil, warnings = Nil)
    val res  = sut.processArguments(sumy, processAll = true, skipped = Nil)
    val set  = sut.language.valueIn(res.sstate)
    assertEquals(2, args.length)
    assertTrue("No warnings!", res.warnings.isEmpty)
    assertTrue("No errors!", res.errors.isEmpty)
    assertTrue("Has the feature", set.contains("implicitConversions"))
    assertTrue("Has the feature", set.contains("dynamics"))

  @Test def `Warn if multistring element is supplied multiply`: Unit =
    class SUT extends SettingGroup:
      val language: Setting[List[String]] = MultiStringSetting(RootSetting, "language", "feature", "Enable one or more language features.")
    val sut  = SUT()
    val args = tokenize("-language:dynamics -language:implicitConversions -language:dynamics")
    val sumy = ArgsSummary(sut.defaultState, args, errors = Nil, warnings = Nil)
    val res  = sut.processArguments(sumy, processAll = true, skipped = Nil)
    val set  = sut.language.valueIn(res.sstate)
    assertEquals(3, args.length)
    assertEquals("Must warn", 1, res.warnings.length)
    assertTrue("No errors!", res.errors.isEmpty)
    assertTrue("Has the feature", set.contains("implicitConversions"))
    assertTrue("Has the feature", set.contains("dynamics"))

  @Test def `WConf setting is parsed`: Unit =
    import reporting.{Action, Diagnostic, NoExplanation}
    val sets = ScalaSettings
    val args = List("-Wconf:cat=deprecation:s,cat=feature:e", "-Wconf:msg=a problem\\.:s")
    val sumy = ArgsSummary(sets.defaultState, args, errors = Nil, warnings = Nil)
    val proc = sets.processArguments(sumy, processAll = true, skipped = Nil)
    val conf = sets.Wconf.valueIn(proc.sstate)
    val sut  = reporting.WConf.fromSettings(conf).getOrElse(???)
    val msg  = "There was a problem!".toMessage
    val depr = new Diagnostic.DeprecationWarning(msg, util.NoSourcePosition)
    assertEquals(Action.Silent, sut.action(depr))
    val feat = new Diagnostic.FeatureWarning(msg, util.NoSourcePosition)
    assertEquals(Action.Error, sut.action(feat))
    val warn = new Diagnostic.Warning(msg, util.NoSourcePosition)
    assertEquals(Action.Warning, sut.action(warn))
    val nowr = new Diagnostic.Warning("This is a problem.".toMessage, util.NoSourcePosition)
    assertEquals(Action.Silent, sut.action(nowr))

  @Test def `i18367 rightmost WConf flags take precedence over flags to the left`: Unit =
    import reporting.{Action, Diagnostic}
    val sets = ScalaSettings
    val args = List("-Wconf:cat=deprecation:e", "-Wconf:cat=deprecation:s")
    val sumy = ArgsSummary(sets.defaultState, args, errors = Nil, warnings = Nil)
    val proc = sets.processArguments(sumy, processAll = true, skipped = Nil)
    val conf = sets.Wconf.valueIn(proc.sstate)
    val msg  = "Don't use that!".toMessage
    val depr = new Diagnostic.DeprecationWarning(msg, util.NoSourcePosition)
    val sut  = reporting.WConf.fromSettings(conf).getOrElse(???)
    assertEquals(Action.Silent, sut.action(depr))

  @Test def `Treat parameterless -W as help`: Unit = 
    val sets = config.ScalaSettings
    val args = List("-W")
    val sumy = ArgsSummary(sets.defaultState, args, errors = Nil, warnings = Nil)
    val proc = sets.processArguments(sumy, processAll = true, skipped = Nil)
    assertTrue(sets.W.valueIn(sumy.sstate).contains("help"))

  @Test def `Enable default warns with -W default`: Unit = 
    val sets = config.ScalaSettings
    val args = List("-W", "default")
    val sumy = ArgsSummary(sets.defaultState, args, errors = Nil, warnings = Nil)
    val proc = sets.processArguments(sumy, processAll = true, skipped = Nil)
    assertTrue(sets.Wunused.valueIn(sumy.sstate).contains("linted"))
    assertTrue(sets.WNonUnitStatement.valueIn(sumy.sstate))
    assertTrue(sets.WvalueDiscard.valueIn(sumy.sstate))

end ScalaSettingsTests
