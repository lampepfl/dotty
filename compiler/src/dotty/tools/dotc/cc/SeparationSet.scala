package dotty.tools
package dotc
package cc

import core.*
import Types.*, Symbols.*, Flags.*, Contexts.*, Decorators.*
import config.Printers.capt
import reporting.trace
import Annotations.Annotation
import annotation.threadUnsafe
import annotation.constructorOnly
import annotation.internal.sharable
import reporting.trace
import printing.{Showable, Printer}
import printing.Texts.*
import util.{SimpleIdentitySet, Property}
import util.common.alwaysTrue
import scala.collection.mutable
import config.Config.ccAllowUnsoundMaps
import CaptureSet.*

object SeparationSet:

  def isReader(x: CaptureRef)(using Context): Boolean =
    x.singletonCaptureSet.subCaptures(CaptureSet.universalReader, frozen = true).isOK

  def ofType(tp: Type)(using Context): CaptureSet =
    def recur(tp: Type): CaptureSet = trace(i"ofType($tp)", show = true) {
      tp.dealias match
        case tp: TermRef =>
          recur(tp.underlying)
        case tp: TermParamRef => recur(tp.underlying)
        case CapturingType(parent, refs) =>
          tp.separationSet
        case _ => CaptureSet.empty
    }
    recur(tp)

  def checkSeparation(cs1: CaptureSet, cs2: CaptureSet, frozen: Boolean)(using Context): Boolean =
    def check(x: CaptureRef, y: CaptureRef): Boolean =
      !(x.singletonCaptureSet.isUniversal || y.singletonCaptureSet.isUniversal) && {
        def maybeWiden(x: CaptureRef, seps: CaptureSet) =
          if seps.isAlwaysEmpty then
            val cs = x.captureSetOfInfo
            if cs.isUniversal then (x.singletonCaptureSet, false)
            else (cs, true)
          else (x.singletonCaptureSet, false)

        def tryReader =
          isReader(x) && isReader(y)

        val (sepx, sepy) = (ofType(x), ofType(y))
        def tryLeftSep: Boolean = !sepx.isAlwaysEmpty && y.singletonCaptureSet.subCaptures(sepx, frozen = frozen).isOK
        def tryRightSep: Boolean = !sepy.isAlwaysEmpty && x.singletonCaptureSet.subCaptures(sepy, frozen = frozen).isOK
        def tryWidening: Boolean =
          val (xw, w1) = maybeWiden(x, sepx)
          val (yw, w2) = maybeWiden(y, sepy)
          if w1 || w2 then
            recur(xw, yw)
          else false
        tryReader || tryLeftSep || tryRightSep || tryWidening
      }
    def recur(cs1: CaptureSet, cs2: CaptureSet): Boolean = trace(i"$cs1 ⋈ $cs2") {
      val problems = cs1.elems.toList.flatMap { x1 => cs2.elems.toList.map(x2 => (x1, x2)) }
      problems.forall { (x1, x2) => check(x1, x2) }
    }
    recur(cs1, cs2)
  end checkSeparation

