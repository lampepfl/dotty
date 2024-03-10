
// Verify that enum values aren't nullified.
class S {
  val d: Day = Day.MON
  val p: Planet = Planet.MARS
  val p2: Planet = p.next() // error: expected Planet but got Planet|Null
}
