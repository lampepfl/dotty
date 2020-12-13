
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.runtime.function;

@FunctionalInterface
public interface JFunction2$mcFDI$sp extends JFunction2<Object, Object, Object> {
    abstract float apply$mcFDI$sp(double v1, int v2);

    default Object apply(Object v1, Object v2) { return (Float) apply$mcFDI$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToInt(v2)); }
}
