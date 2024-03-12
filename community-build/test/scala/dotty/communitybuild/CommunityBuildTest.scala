package dotty.communitybuild

import java.nio.file._
import java.io.{PrintWriter, File}
import java.nio.charset.StandardCharsets.UTF_8
import org.junit.{Ignore, Test}
import org.junit.Assert.{assertEquals, fail}
import org.junit.experimental.categories.Category

import CommunityBuildRunner.run

class TestCategory

given testRunner: CommunityBuildRunner with
  override def failWith(msg: String) = { fail(msg); ??? }

@Category(Array(classOf[TestCategory]))
class CommunityBuildTestA:
  @Test def izumiReflect = projects.izumiReflect.run()
  @Test def scalaSTM = projects.scalaSTM.run()
  @Test def scalatest = projects.scalatest.run()
  @Test def scalatestplusTestNG = projects.scalatestplusTestNG.run()
  // 'Sciss/Lucre' dependencies:
  // @Test def scissEqual      = projects.scissEqual     .run()
  // @Test def scissFingerTree = projects.scissFingerTree.run()
  // @Test def scissLog        = projects.scissLog       .run()
  // @Test def scissModel      = projects.scissModel     .run()
  // @Test def scissNumbers    = projects.scissNumbers   .run()
  // @Test def scissSerial     = projects.scissSerial    .run()
  // @Test def scissAsyncFile  = projects.scissAsyncFile .run()
  // @Test def scissSpan       = projects.scissSpan      .run()
  @Test def scissLucre = projects.scissLucre.run()
  @Test def zio = projects.zio.run()
end CommunityBuildTestA

@Category(Array(classOf[TestCategory]))
class CommunityBuildTestB:
  @Test def cats = projects.cats.run()
  @Test def catsEffect3 = projects.catsEffect3.run()
  @Test def catsMtl = projects.catsMtl.run()
  @Test def coop = projects.coop.run()
  @Test def discipline = projects.discipline.run()
  @Test def disciplineMunit = projects.disciplineMunit.run()
  @Test def disciplineSpecs2 = projects.disciplineSpecs2.run()
  @Test def fs2 = projects.fs2.run()
  @Test def monocle = projects.monocle.run()
  @Test def munit = projects.munit.run()
  @Test def munitCatsEffect = projects.munitCatsEffect.run()
  @Test def perspective = projects.perspective.run()
  @Test def scalacheckEffect = projects.scalacheckEffect.run()
  @Test def scodec = projects.scodec.run()
  @Test def scodecBits = projects.scodecBits.run()
  @Test def simulacrumScalafixAnnotations = projects.simulacrumScalafixAnnotations.run()
  @Test def spire = projects.spire.run()
  @Test def http4s = projects.http4s.run()
end CommunityBuildTestB

@Category(Array(classOf[TestCategory]))
class CommunityBuildTestC:
  @Test def akka = projects.akka.run()
  @Test def betterfiles = projects.betterfiles.run()
  @Test def cask = projects.cask.run()
  // Temporarily disabled until problem discovered in comments to #9449 is fixed
  // @Test def dottyCpsAsync = projects.dottyCpsAsync.run()
  @Test def effpi = projects.effpi.run()
  @Test def endpoints4s = projects.endpoints4s.run()
  @Test def fansi = projects.fansi.run()
  @Test def fastparse = projects.fastparse.run()
  @Test def geny = projects.geny.run()
  @Test def intent = projects.intent.run()
  @Test def jacksonModuleScala = projects.jacksonModuleScala.run()
  @Test def libretto = projects.libretto.run()
  @Test def minitest = projects.minitest.run()
  @Test def onnxScala = projects.onnxScala.run()
  @Test def oslib = projects.oslib.run()
  // @Test def oslibWatch = projects.oslibWatch.run()
  @Test def parboiled2 = projects.parboiled2.run()
  @Test def playJson = projects.playJson.run()
  @Test def pprint = projects.pprint.run()
  @Test def protoquill = projects.protoquill.run()
  @Test def requests = projects.requests.run()
  @Test def scalacheck = projects.scalacheck.run()
  @Test def scalaCollectionCompat = projects.scalaCollectionCompat.run()
  @Test def scalaJava8Compat = projects.scalaJava8Compat.run()
  @Test def scalap = projects.scalap.run()
  @Test def scalaParallelCollections = projects.scalaParallelCollections.run()
  @Test def scalaParserCombinators = projects.scalaParserCombinators.run()
  @Test def scalaPB = projects.scalaPB.run()
  @Test def scalatestplusScalacheck = projects.scalatestplusScalacheck.run()
  @Test def scalaXml = projects.scalaXml.run()
  @Test def scalaz = projects.scalaz.run()
  @Test def scas = projects.scas.run()
  @Test def sconfig = projects.sconfig.run()
  @Test def shapeless3 = projects.shapeless3.run()
  @Test def sourcecode = projects.sourcecode.run()
  @Test def specs2 = projects.specs2.run()

  @Test def ujson = projects.ujson.run()
  @Test def upickle = projects.upickle.run()
  @Test def utest = projects.utest.run()
  @Test def verify = projects.verify.run()
  @Test def xmlInterpolator = projects.xmlInterpolator.run()
end CommunityBuildTestC
