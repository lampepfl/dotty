/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */

package dotty.tools.dotc.classpath

import dotty.tools.io.ClassPath
import dotty.tools.backend.jvm.AsmUtils

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import dotty.tools.dotc.config.PathResolver
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.classpath.ClassPathFactory

@RunWith(classOf[JUnit4])
class JrtClassPathTest {

  @Test def lookupJavaClasses(): Unit = {
    given Context = new ContextBase().initialCtx
    val specVersion = scala.util.Properties.javaSpecVersion
    // Run the test using the JDK8 or 9 provider for rt.jar depending on the platform the test is running on.
    val cp: ClassPath =
      if (specVersion == "" || specVersion == "1.8") {
        val resolver = new PathResolver
        val elements = (new ClassPathFactory).classesInPath(resolver.Calculated.javaBootClassPath)
        AggregateClassPath(elements)
      }
      else JrtClassPath(None).get

    assertEquals(Nil, cp.classes(""))
    assertTrue(cp.packages("java").toString, cp.packages("java").exists(_.name == "java.lang"))
    assertTrue(cp.classes("java.lang").exists(_.name == "Object"))
    val jl_Object = cp.classes("java.lang").find(_.name == "Object").get
    assertEquals("java/lang/Object", AsmUtils.readClass(jl_Object.file.toByteArray).name)
    assertTrue(cp.list("java.lang").packages.exists(_.name == "java.lang.annotation"))
    assertTrue(cp.list("java.lang").classesAndSources.exists(_.name == "Object"))
    assertTrue(cp.findClassFile("java.lang.Object").isDefined)
  }
}
