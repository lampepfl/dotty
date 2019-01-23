/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction2$mcVJD$sp extends JFunction2 {
    abstract void apply$mcVJD$sp(long v1, double v2);

    default Object apply(Object v1, Object v2) { apply$mcVJD$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToDouble(v2)); return scala.runtime.BoxedUnit.UNIT; }
}
