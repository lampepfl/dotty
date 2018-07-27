package scala.tasty

trait FlagSet {
  def isProtected: Boolean
  def isAbstract: Boolean
  def isFinal: Boolean
  def isSealed: Boolean
  def isCase: Boolean
  def isImplicit: Boolean
  def isErased: Boolean
  def isLazy: Boolean
  def isOverride: Boolean
  def isTransparent: Boolean
  def isStatic: Boolean                // mapped to static Java member
  def isObject: Boolean                // an object or its class (used for a ValDef or a ClassDef extends Modifier respectively)
  def isTrait: Boolean                 // a trait (used for a ClassDef)
  def isLocal: Boolean                 // used in conjunction with Private/private[Type] to mean private[this] extends Modifier proctected[this]
  def isSynthetic: Boolean             // generated by Scala compiler
  def isArtifact: Boolean              // to be tagged Java Synthetic
  def isMutable: Boolean               // when used on a ValDef: a var
  def isLabel: Boolean                 // method generated as a label
  def isFieldAccessor: Boolean         // a getter or setter
  def isCaseAcessor: Boolean           // getter for class parameter
  def isCovariant: Boolean             // type parameter marked “+”
  def isContravariant: Boolean         // type parameter marked “-”
  def isScala2X: Boolean               // Imported from Scala2.x
  def isDefaultParameterized: Boolean  // Method with default parameters
  def isStable: Boolean                // Method that is assumed to be stable
  def isParam: Boolean
  def isParamAccessor: Boolean
}
