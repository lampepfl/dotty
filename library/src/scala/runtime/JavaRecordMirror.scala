package scala.runtime

import java.lang.Record
import java.lang.reflect.Constructor
import scala.reflect.ClassTag

// TODO: Rename to JavaRecordReflectMirror
object JavaRecordMirror:

  def apply[T <: Record](clazz: Class[T]): JavaRecordMirror[T] =
    val components = clazz.getRecordComponents.nn
    val constructorTypes = components.map(_.nn.getType.nn)
    val constr = clazz.getDeclaredConstructor(constructorTypes*).nn
    new JavaRecordMirror(components.length, constr)

  def of[T <: Record : ClassTag]: JavaRecordMirror[T] =
    JavaRecordMirror(summon[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])

// TODO: Is a constructor serializable?
final class JavaRecordMirror[T] private(arity: Int, constr: Constructor[T]) extends scala.deriving.Mirror.Product with Serializable:

  override type MirroredMonoType <: Record

  final def fromProduct(product: Product): MirroredMonoType =
    if product.productArity != arity  then
    throw IllegalArgumentException(s"expected Product with $arity elements, got ${product.productArity}")
    else
      // TODO: Check this byte code, we want to unroll to give a happy medium between JIT'ing and having tons of extra classes
      val t = arity match
        case 0 => constr.newInstance()
        case 1 => constr.newInstance(product.productElement(0))
        case 2 => constr.newInstance(product.productElement(0), product.productElement(1))

      t.nn.asInstanceOf[MirroredMonoType]
