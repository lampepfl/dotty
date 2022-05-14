package scala.runtime;

import java.util.Arrays;
import java.util.Map;
import java.util.List;
import java.util.HashMap;
import java.lang.invoke.ConstantCallSite;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;

import static java.lang.invoke.MethodType.methodType;

/** Exposes bootstrap method for abstracting away boilerplate implementations
  * of case class methods.
  */
public final class CaseClassMethods {

  // Method handles for hashing
  private static final MethodHandle STRINGHASHCODE_HANDLE;
  private static final MethodHandle PRODUCTPREFIX_HANDLE;
  private static final MethodHandle MIX_HANDLE;
  private static final MethodHandle FINALIZEHASH_HANDLE;
  private static final MethodHandle ANYHASH_HANDLE;
  private static final Map<Class<?>, MethodHandle> PRIMITIVE_HASHER_HANDLES;
  private static final MethodHandle INITIALHASH_HANDLE;

  // Method handles for equality
  private static final MethodHandle FALSE_HANDLE;
  private static final MethodHandle TRUE_HANDLE;
  private static final MethodHandle OBJECTEQUALS_HANDLE;
  private static final MethodHandle OBJECTEQ_HANDLE;
  private static final MethodHandle ISINSTANCE_HANDLE;
  private static final Map<Class<?>, MethodHandle> PRIMITIVE_EQ_HANDLES;


  // Method handles for product elements extraction
  private static final MethodHandle INDEXOUTOFBOUNDS_HANDLE;

  // MethodHandles for making MethodHandles (polyfill for older JVMs)
  private static final MethodHandle TABLESWITCH_HANDLE;
  private static final MethodHandle INRANGE_HANDLE;

  static {
    final MethodHandles.Lookup lookup = MethodHandles.lookup();
    final Class<?> STATICS_CLASS = Statics.class;

    try {
      STRINGHASHCODE_HANDLE = lookup.findVirtual(
        String.class,
        "hashCode",
        methodType(int.class)
      );
      PRODUCTPREFIX_HANDLE = lookup.findVirtual(
        scala.Product.class,
        "productPrefix",
        methodType(String.class)
      );
      MIX_HANDLE = lookup.findStatic(
        Statics.class,
        "mix",
        methodType(int.class, int.class, int.class)
      );
      FINALIZEHASH_HANDLE = lookup.findStatic(
        Statics.class,
        "finalizeHash",
        methodType(int.class, int.class, int.class)
      );
      ANYHASH_HANDLE = lookup.findStatic(
        Statics.class,
        "anyHash",
        methodType(int.class, Object.class)
      );
      INITIALHASH_HANDLE = MethodHandles.constant(int.class, 0xcafebabe);

      PRIMITIVE_HASHER_HANDLES = new HashMap<Class<?>, MethodHandle>();
      final var primHashTypes = Arrays.asList(
        byte.class, short.class, char.class, int.class, boolean.class,
        BoxedUnit.class, Null$.class
      );
      for (final var primType : primHashTypes) {
        PRIMITIVE_HASHER_HANDLES.put(
          primType,
          lookup.findStatic(CaseClassMethods.class, "hash", methodType(int.class, primType))
        );
      }
      PRIMITIVE_HASHER_HANDLES.put(
        long.class,
        lookup.findStatic(Statics.class, "longHash", methodType(int.class, long.class))
      );
      PRIMITIVE_HASHER_HANDLES.put(
        double.class,
        lookup.findStatic(Statics.class, "doubleHash", methodType(int.class, double.class))
      );
      PRIMITIVE_HASHER_HANDLES.put(
        float.class,
        lookup.findStatic(Statics.class, "floatHash", methodType(int.class, float.class))
      );

      FALSE_HANDLE = MethodHandles.constant(boolean.class, false);
      TRUE_HANDLE = MethodHandles.constant(boolean.class, true);
      OBJECTEQUALS_HANDLE = lookup.findVirtual(Object.class, "equals", methodType(boolean.class, Object.class));
      OBJECTEQ_HANDLE = lookup.findStatic(CaseClassMethods.class, "eq", methodType(boolean.class, Object.class, Object.class));
      ISINSTANCE_HANDLE = lookup.findVirtual(Class.class, "isInstance", methodType(boolean.class, Object.class));

      PRIMITIVE_EQ_HANDLES = new HashMap<Class<?>, MethodHandle>();
      final var primEqTypes = Arrays.asList(
        byte.class, short.class, char.class, int.class,
        long.class, float.class, double.class, boolean.class
      );
      for (final var primType : primEqTypes) {
        PRIMITIVE_EQ_HANDLES.put(
          primType,
          lookup.findStatic(CaseClassMethods.class, "eq", methodType(boolean.class, primType, primType))
        );
      }

      INRANGE_HANDLE = lookup.findStatic(
        CaseClassMethods.class,
        "checkInRange",
        methodType(boolean.class, int.class, int.class)
      );

      INDEXOUTOFBOUNDS_HANDLE = lookup.findConstructor(
        IndexOutOfBoundsException.class,
        methodType(void.class, int.class)
      );
    } catch (ReflectiveOperationException e) {
      throw new RuntimeException(e);
    }

    // If the runtime JVM has `tableSwitch`, use that
    MethodHandle tableSwitchHandle = null;
    try {
      tableSwitchHandle = lookup.findStatic(
        MethodHandles.class,
        "tableSwitch",
        methodType(MethodHandle.class, MethodHandle.class, MethodHandle[].class)
      );
    } catch (ReflectiveOperationException e) {
      // Ignore - this is expected to fail for <17
    }
    TABLESWITCH_HANDLE = tableSwitchHandle;
  }

  private CaseClassMethods() { }

  public static ConstantCallSite bootstrap(
    MethodHandles.Lookup lookup,
    String methodName,
    MethodType methodType,
    Class<?> caseClass,
    MethodHandle... fieldHandles
  ) throws Throwable {
    final MethodHandle methodHandle;
    switch (methodName) {
      case "equals":
        checkMethod(methodName, methodType, methodType(boolean.class, caseClass, Object.class));
        // TODO: skip `canEqual` when possible
        methodHandle = caseClassEqualsHandle(
          caseClass,
          java.util.Arrays.asList(fieldHandles),
          methodType,
          lookup.findVirtual(caseClass, "canEqual", methodType(boolean.class, Object.class))
        );
        break;

      case "hashCode":
        checkMethod(methodName, methodType, methodType(int.class, caseClass));
        // TODO: pass in actual `productPrefix` when class is final
        methodHandle = caseClassHashCodeHandle(
          caseClass,
          java.util.Arrays.asList(fieldHandles),
          methodType,
          lookup.findVirtual(caseClass, "productPrefix", methodType(String.class))
        );
        break;

      case "productElement":
        checkMethod(methodName, methodType, methodType(Object.class, caseClass, int.class));
        methodHandle = caseClassProductElementHandle(
          caseClass,
          Arrays.asList(fieldHandles),
          methodType
        );
        break;

      case "productElementName":
      default:
        throw new IllegalArgumentException("Invalid method name: " + methodName);
    }

    return new ConstantCallSite(methodHandle);
  }

  // Stubs
  public static int caseHashCode(scala.Product thiz) {
    return scala.util.hashing.MurmurHash3$.MODULE$.productHash(thiz);
  }
  public static boolean caseEquals(scala.Product thiz, Object other) {
    throw new AssertionError();
  }
  public static Object caseProductElement(scala.Product thiz, int index) {
    throw new AssertionError();
  }

  /**
   * Assert the method type is the correct one.
   *
   * @param methodName name of the method
   * @param found user provided method type
   * @param expected expected type
   */
  private static void checkMethod(String name, MethodType found, MethodType expected) {
    if (!found.equals(expected)) {
      throw new IllegalArgumentException("Invalid type for " + name + ": " + found);
    }
  }

  /**
   * Produce a method handle implementing `equals` for a case class.
   *
   * @param caseClass left hand side of the handle
   * @param fieldHandles handles for extracting each of the case fields
   * @param methodType type of the output method
   * @param canEqualHandle if not null, include a call to this too
   * @return handle implementing case class equality check
   */
  private static MethodHandle caseClassEqualsHandle(
    Class<?> caseClass,
    List<MethodHandle> fieldHandles,
    MethodType methodType,
    MethodHandle canEqualHandle
  ) {

    final var shortCutFalse = MethodHandles.dropArguments(FALSE_HANDLE, 0, caseClass, caseClass);
    final var shortCutTrue = MethodHandles.dropArguments(TRUE_HANDLE, 0, caseClass, caseClass);

    // Accumulator of type `(LMyCaseClass;LMyCaseClass;)Z`
    var accEqual = shortCutTrue;

    // Invoke `canEqual`
    if (canEqualHandle != null) {
      accEqual = MethodHandles.guardWithTest(
        canEqualHandle.asType(accEqual.type()),
        accEqual,
        shortCutFalse
      );
    }

    // Scan over reference fields
    for (MethodHandle fieldHandle : fieldHandles) {
      final Class<?> fieldType = fieldHandle.type().returnType();
      if (fieldType.isPrimitive()) {
        continue;
      }
      var fieldEq = OBJECTEQUALS_HANDLE.asType(methodType(boolean.class, fieldType, fieldType));

      // Fold this field's equality back into the accumulator
      accEqual = MethodHandles.guardWithTest(
        MethodHandles.filterArguments(fieldEq, 0, fieldHandle, fieldHandle),
        accEqual,
        shortCutFalse
      );
    }

    // Scan over primitive fieaccEqual = lds
    for (MethodHandle fieldHandle : fieldHandles) {
      final Class<?> fieldType = fieldHandle.type().returnType();
      final var fieldEq = PRIMITIVE_EQ_HANDLES.get(fieldType);
      if (fieldEq == null) {
        continue;
      }

      // Fold this field's equality back into the accumulator
      accEqual = MethodHandles.guardWithTest(
        MethodHandles.filterArguments(fieldEq, 0, fieldHandle, fieldHandle),
        accEqual,
        shortCutFalse
      );
    }

    // Check the second argument has the right type
    final var typeCheckedEquality = MethodHandles.guardWithTest(
      MethodHandles.dropArguments(ISINSTANCE_HANDLE.bindTo(caseClass), 0, caseClass),
      accEqual.asType(methodType),
      shortCutFalse.asType(methodType)
    );

    // Finally, short-cut on object equality
    final var shortCuttingEquality = MethodHandles.guardWithTest(
      OBJECTEQ_HANDLE.asType(methodType),
      shortCutTrue.asType(methodType),
      typeCheckedEquality
    );

    return shortCuttingEquality;
  }

  /**
   * Produce a method handle implementing `hashCode` for a case class.
   *
   * TODO: for final classes, pass in and pre-compute the hash on `productPrefix`
   *
   * @param caseClass left hand side of the handle
   * @param fieldHandles handles for extracting each of the case fields
   * @param methodType type of the output method
   * @param productPrefixHandle handle to `productPrefix`
   * @return handle implementing case class hashing
   */
  private static MethodHandle caseClassHashCodeHandle(
    Class<?> caseClass,
    List<MethodHandle> fieldHandles,
    MethodType methodType,
    MethodHandle productPrefixHandle
  ) {

    // Accumulator hasher of type `(LMyCaseClass;)I`
    var accHash = MethodHandles.filterReturnValue(
      MethodHandles.filterReturnValue(productPrefixHandle, STRINGHASHCODE_HANDLE),
      MethodHandles.insertArguments(MIX_HANDLE, 0, 0xcafebabe)
    );
    final var accHashType = accHash.type();

    // Fold over fields, adding each field into the `accHash` handle
    for (final var fieldHandle : fieldHandles) {
      final Class<?> fieldType = fieldHandle.type().returnType();

      // Handle of type `(LMyCaseClass;)I` for hashing just this field
      final var fieldHash = MethodHandles.filterReturnValue(
        fieldHandle,
        fieldHashHandle(fieldType)
      );

      // Mix this field's hasher back into the accumulator hasher
      accHash = MethodHandles.permuteArguments(
        MethodHandles.filterArguments(MIX_HANDLE, 0, accHash, fieldHash),
        accHashType,
        0,
        0
      );
    }

    // Finalize the hash using the number of fields
    final var finalizedHash = MethodHandles.filterReturnValue(
      accHash,
      MethodHandles.insertArguments(FINALIZEHASH_HANDLE, 1, fieldHandles.size())
    );

    return finalizedHash;
  }

  /**
   * Hash a single field of the specified type.
   *
   * @param fieldType field to hash
   * @return handle taking the field type as input and returning an `int`
   */
  private static MethodHandle fieldHashHandle(Class<?> fieldType) {
    final var hasher = PRIMITIVE_HASHER_HANDLES.get(fieldType);
    if (hasher != null) {
      return hasher;
    } else {
      return ANYHASH_HANDLE.asType(methodType(int.class, fieldType));
    }
  }

  /**
   * Produce a method handle implementing `productElement` for a case class.
   *
   * @param caseClass left hand side of the handle
   * @param fieldHandles handles for extracting each of the case fields
   * @param methodType type of the output method
   * @return handle implementing case class product element selection
   */
  private static MethodHandle caseClassProductElementHandle(
    Class<?> caseClass,
    List<MethodHandle> fieldHandles,
    MethodType methodType
  ) {

    // Prepare an array of handles for extracting boxed versions of fields
    final var boxedFieldHandles = new MethodHandle[fieldHandles.size()];
    final var boxedFieldHandleType = methodType.dropParameterTypes(1, 2);
    for (int i = 0; i < boxedFieldHandles.length; i++) {
      boxedFieldHandles[i] = fieldHandles.get(i).asType(boxedFieldHandleType);
    }

    // Fallback case - throws the right exception
    final var indexOutOfBoundsHandle = MethodHandles.filterReturnValue(
      MethodHandles.dropArguments(INDEXOUTOFBOUNDS_HANDLE, 1, caseClass),
      MethodHandles.throwException(Object.class, IndexOutOfBoundsException.class)
    );

    return MethodHandles.permuteArguments(
      tableSwitch(indexOutOfBoundsHandle, boxedFieldHandles),
      methodType,
      new int[] { 1, 0 }
    );
  }

  /**
   * Almost `MethodHandle.tableSwitch`, but only the fallback case takes a
   * leading `int` argument and this will fallback to an array based
   * implementation on oldee JVMs.
   *
   * All of `targets` and `fallback` must take an `int` as their first argument.
   *
   * @param fallback method handle to invoke if the switch index falls outside targets
   * @param targets method handles to invoke for various indices
   * @return handle dispatching to the right target based on a leading `int` argument
   */
  private static MethodHandle tableSwitch(MethodHandle fallback, MethodHandle[] targets) {
    if (TABLESWITCH_HANDLE != null) {
      try {
        final var adaptedTargets = new MethodHandle[targets.length];
        for (int i = 0; i < adaptedTargets.length; i++) {
          adaptedTargets[i] = MethodHandles.dropArguments(targets[i], 0, int.class);
        }
        return (MethodHandle)TABLESWITCH_HANDLE.invoke(fallback, adaptedTargets);
      } catch (Throwable e) {
        throw new RuntimeException(e);
      }
    }

    // `(I)Ljava/lang/invoke/MethodHandle;`
    final var getHandle = MethodHandles
      .arrayElementGetter(MethodHandle[].class)
      .bindTo(targets);

    // `(I A1 A2 ...)R`, but only works for valid indices
    final var uncheckedIndexHandle = MethodHandles.filterArguments(
      MethodHandles.exactInvoker(fallback.type().dropParameterTypes(0, 1)),
      0,
      getHandle
    );

    return MethodHandles.guardWithTest(
      MethodHandles.insertArguments(INRANGE_HANDLE, 1, targets.length),
      uncheckedIndexHandle,
      fallback
    );
  }

  /**
   * Check if an index is between zero and a specified length.
   *
   * @param index what to check
   * @param length (non-inclusive) upper bound
   * @return whether the index is in bounds
   */
  private static boolean checkInRange(int index, int length) {
    return 0 <= index && index < length;
  }

  /* Equality helper methods (only ever used through method handle lookups).
   *
   * These should match the expected semantics of comparing two fields in a
   * derived implementation of `equals` for a case class.
   */
  private static boolean eq(Object a, Object b) {
    return a == b;
  }
  private static boolean eq(byte a, byte b) {
    return a == b;
  }
  private static boolean eq(short a, short b) {
    return a == b;
  }
  private static boolean eq(char a, char b) {
    return a == b;
  }
  private static boolean eq(int a, int b) {
    return a == b;
  }
  private static boolean eq(long a, long b) {
    return a == b;
  }
  private static boolean eq(float a, float b) {
    return a == b;
  }
  private static boolean eq(double a, double b) {
    return a == b;
  }
  private static boolean eq(boolean a, boolean b) {
    return a == b;
  }

  /* Hashing helper methods (only ever used through method handle lookups).
   *
   * These should match the expected semantics of hashing a fields in a
   * derived implementation of `hashCode` for a case class.
   */
  private static int hash(boolean a) {
    return a ? 1231 : 1237;
  }
  private static int hash(byte a) {
    return a;
  }
  private static int hash(short a) {
    return a;
  }
  private static int hash(char a) {
    return a;
  }
  private static int hash(int a) {
    return a;
  }
  private static int hash(BoxedUnit a) {
    return 0;
  }
  private static int hash(Null$ a) {
    return 0;
  }
}
