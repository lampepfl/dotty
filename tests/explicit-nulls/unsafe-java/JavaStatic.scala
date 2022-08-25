import language.experimental.unsafeJavaReturn

import java.math.MathContext, MathContext._

val x: MathContext = DECIMAL32
val y: MathContext = MathContext.DECIMAL32

import java.io.File

val s: String = File.separator
import java.time.ZoneId

val zids: java.util.Set[String] = ZoneId.getAvailableZoneIds
val zarr: Array[String] = ZoneId.getAvailableZoneIds.toArray(Array.empty[String | Null])
