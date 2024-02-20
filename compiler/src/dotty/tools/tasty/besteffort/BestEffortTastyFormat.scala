package dotty.tools.tasty.besteffort

import dotty.tools.tasty.TastyFormat

/*************************************************************************************
Best Effort TASTy (.betasty) format extends the TASTy grammar with additional
terminal symbols and productions. Grammar notation is kept from the regular TASTy.
However, the lowercase prefixes describing the semantics (but otherwise not affecting
the grammar) may not always hold.

The following are the added terminal Symbols to the grammar:
  * `ERRORtype` - representing an error from a previous compilation

The following are the added productions to the grammar:

  Standard-Section: "ASTs"
```none
  Type          = ERRORtype
  Path          = ERRORtype
```
**************************************************************************************/
object BestEffortTastyFormat {
  export TastyFormat.{astTagToString => _, *}

  /** First four bytes of a best effort TASTy file, used instead of the regular header.
   *  Signifies that the TASTy can only be consumed by the compiler in the best effort mode.
   *  Other than that, versioning works as usual, disallowing Best Effort Tasty from older minor versions.
   */
  final val bestEffortHeader: Array[Int] = Array(0x5C, 0xA1, 0xAB, 0x20)

  /** Natural number. Along with MajorVersion, MinorVersion and ExperimentalVersion
   *  numbers specifies the Best Effort TASTy format. For now, Best Effort TASTy holds
   *  no compatibility guarantees, making this a reserved space for when this would have
   *  to be changed.
   */
  final val PatchVersion: Int = 0

  // added AST tag - Best Effort TASTy only
  final val ERRORtype = 50

  def astTagToString(tag: Int) = tag match {
    case ERRORtype => "ERRORtype"
    case _ => TastyFormat.astTagToString(tag)
  }
}
