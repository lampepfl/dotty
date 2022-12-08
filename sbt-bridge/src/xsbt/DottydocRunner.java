/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt;

import xsbti.Logger;
import xsbti.Severity;


import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.ArrayList;

import dotty.tools.dotc.core.Contexts.ContextCls;
import dotty.tools.dotc.core.Contexts.ContextBase;
import dotty.tools.dotc.reporting.Reporter;
import dotty.tools.xsbt.InterfaceCompileFailed;
import dotty.tools.xsbt.DelegatingReporter;

public class DottydocRunner {
  private final String[] args0;
  private final Logger log;
  private final xsbti.Reporter delegate;

  public DottydocRunner(String[] args0, Logger log, xsbti.Reporter delegate) {
    super();
    this.args0 = args0;
    this.log = log;
    this.delegate = delegate;
  }

  public void run() {
    log.debug(() -> {
      StringBuilder msg =
        new StringBuilder("Calling Dottydoc with arguments  (ScaladocInterface):");
      for (String arg : args0) {
        msg.append("\n\t");
        msg.append(arg);
      }
      return msg.toString();
    });

    String[] args;

    ArrayList<String> retained = new ArrayList<>(args0.length);
    for (String arg : args0) {
      if (!((arg.endsWith(".scala") || arg.endsWith(".java")) && Files.exists(Paths.get(arg))))
        retained.add(arg);
    }
    args = retained.toArray(new String[retained.size()]);

    ContextCls ctx = new ContextBase().initialCtx().fresh()
      .setReporter(new DelegatingReporter(delegate));

    try {
      Class<?> dottydocMainClass = Class.forName("dotty.tools.dottydoc.Main");
      Method processMethod = dottydocMainClass.getMethod("process", args.getClass(), ContextCls.class); // args.getClass() is String[]
      Reporter reporter = (Reporter) processMethod.invoke(null, args, ctx);
      if (reporter.hasErrors())
        throw new InterfaceCompileFailed(args, new xsbti.Problem[0], "DottyDoc Compilation Failed");
    } catch (ClassNotFoundException | NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
      throw new RuntimeException(e);
    }
  }
}
