package dotty.tools.scaladoc.signatures

class GenericSignatures extends SignatureTest("genericSignatures", Seq("class"))

class ObjectSignatures extends SignatureTest("objectSignatures", Seq("object"))

class TraitSignatures extends SignatureTest("traitSignatures", Seq("trait"))

// We do not support companion objects properly in tests
class ClassSignatureTestSourceTest extends SignatureTest("classSignatureTestSource",
  SignatureTest.all diff Seq("val", "var", "object"))

// TODO we still cannot filter out all constructor-based fields
class SignatureTestSourceTest extends SignatureTest("signatureTestSource", SignatureTest.all)

class ModifiersSignatureTest extends SignatureTest("modifiersSignatureTestSource", SignatureTest.all)

class VisibilityTest extends SignatureTest("visibility", SignatureTest.all)


class GenericMethodsTest extends SignatureTest("genericMethods", Seq("def"))

class MethodsAndConstructors extends SignatureTest("methodsAndConstructors", Seq("def"))

class TypesSignatures extends SignatureTest("typesSignatures", SignatureTest.all)

class FieldsSignatures extends SignatureTest("fieldsSignatures", SignatureTest.all.filterNot(_ == "object"))

class NestedSignatures extends SignatureTest("nested", SignatureTest.all)

class TypeAppliacneSignatures extends SignatureTest("typeAppliance", SignatureTest.all)

class CompanionObjectSignatures extends SignatureTest("companionObjectSignatures", SignatureTest.all)

class PackageSymbolSignatures extends SignatureTest("packageSymbolSignatures", SignatureTest.all)

class PackageObjectSymbolSignatures extends SignatureTest("packageObjectSymbolSignatures", SignatureTest.all.filterNot(_ == "object"))

class MergedPackageSignatures extends SignatureTest("mergedPackage", SignatureTest.all.filterNot(_ == "object"),
  sourceFiles = List("mergedPackage1", "mergedPackage2", "mergedPackage3"))

class ExtensionMethodSignature extends SignatureTest("extensionMethodSignatures", SignatureTest.all.filterNot(_ == "extension"))

class ExtensionMethodParamsSignature extends SignatureTest("extensionParams", SignatureTest.all)

class ClassModifiers extends SignatureTest("classModifiers", SignatureTest.classlikeKinds)

class EnumSignatures extends SignatureTest("enumSignatures", "case" +: SignatureTest.all)

class StructuralTypes extends SignatureTest("structuralTypes", SignatureTest.members)

class OpaqueTypes extends SignatureTest("opaqueTypes", SignatureTest.all)

class GivenSignatures extends SignatureTest("givenSignatures", SignatureTest.all)

class Annotations extends SignatureTest("annotations", SignatureTest.all)

class InheritanceLoop extends SignatureTest("inheritanceLoop", SignatureTest.all)

class InheritedMembers extends SignatureTest("inheritedMembers2", SignatureTest.all.filter(_ != "class"),
  sourceFiles = List("inheritedMembers1", "inheritedMembers2"))

class InheritedFromHiddenClasslike extends SignatureTest("inheritedMembersFromHidden", SignatureTest.all)

class ComplexNames extends SignatureTest("complexNames", Seq("def", "class"))

class WrongDocumentationLinks extends SignatureTest("links", Seq("def"))

class ImplicitConversionsTest1 extends SignatureTest(
  "implicitConversions2",
  SignatureTest.all,
  sourceFiles = List("implicitConversions2"),
  filterFunc = _.toString.endsWith("ClassWithConversionWithOneParam.html")
)

class ImplicitConversionsTest2 extends SignatureTest(
  "implicitConversions2",
  SignatureTest.all,
  sourceFiles = List("implicitConversions2"),
  filterFunc = _.toString.endsWith("ClassWithConversionFromVal.html")
)

class ImplicitConversionsTest3 extends SignatureTest(
  "implicitConversions2",
  SignatureTest.all,
  sourceFiles = List("implicitConversions2"),
  filterFunc = _.toString.endsWith("ClassWithConversionWithProperType.html")
)

class SpecializedSignature extends SignatureTest("specializedSignature", SignatureTest.all)

class ContextBounds extends SignatureTest("contextBounds", SignatureTest.all)

class FBoundedTypeParameters extends SignatureTest("fboundedTypeParameters", SignatureTest.all)

class Exports extends SignatureTest("exports", SignatureTest.all, filterFunc = _.toString.endsWith("B.html"))

class ContextFunctions extends SignatureTest("contextfunctions", SignatureTest.all)

class MarkdownCode extends SignatureTest("markdowncode", SignatureTest.all)

class FunctionTypeSignatures extends SignatureTest("functionTypeSignatures", SignatureTest.all)

class ImplicitMembers extends SignatureTest(
  "implicitMembers",
  Seq("def"),
  filterFunc = _.toString.endsWith("OuterClass$ImplicitMemberTarget.html")
)

class NonScala3Parent extends SignatureTest("nonScala3Parent", SignatureTest.all)

class SupertypeParamsSubstitution extends SignatureTest("supertypeParamsSubstitution", SignatureTest.all)

class ThisType extends SignatureTest("thisType", SignatureTest.all)

class PathDependentTypes extends SignatureTest("pathDependentTypes", SignatureTest.all)

class MatchTypeTuple extends SignatureTest("matchTypeTuple", SignatureTest.all)

class InfixTypes extends SignatureTest("infixTypes", SignatureTest.all)

class ExtendsCall extends SignatureTest("extendsCall", SignatureTest.all)
