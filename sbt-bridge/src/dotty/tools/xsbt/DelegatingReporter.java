/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package dotty.tools.xsbt;

import scala.Tuple2;
import scala.collection.mutable.HashMap;

import dotty.tools.dotc.core.Contexts.ContextCls;
import dotty.tools.dotc.reporting.AbstractReporter;
import dotty.tools.dotc.reporting.Diagnostic;
import dotty.tools.dotc.reporting.Message;
import dotty.tools.dotc.util.SourceFile;
import dotty.tools.dotc.util.SourcePosition;
import xsbti.Position;
import xsbti.Severity;

final public class DelegatingReporter extends AbstractReporter {
  private xsbti.Reporter delegate;

  public DelegatingReporter(xsbti.Reporter delegate) {
    super();
    this.delegate = delegate;
  }

  public void dropDelegate() {
    delegate = null;
  }

  @Override
  public void printSummary(ContextCls ctx) {
    delegate.printSummary();
  }

  public void doReport(Diagnostic dia, ContextCls ctx) {
    Severity severity = severityOf(dia.level());
    Position position = positionOf(dia.pos().nonInlined());

    StringBuilder rendered = new StringBuilder();
    rendered.append(messageAndPos(dia, ctx));
    Message message = dia.msg();
    String diagnosticCode = String.valueOf(message.errorId().errorNumber());
    boolean shouldExplain = Diagnostic.shouldExplain(dia, ctx);
    if (shouldExplain && !message.explanation().isEmpty()) {
      rendered.append(explanation(message, ctx));
    }

    delegate.log(new Problem(position, message.message(), severity, rendered.toString(), diagnosticCode));
  }

  private static Severity severityOf(int level) {
    Severity severity;
    switch (level) {
      case dotty.tools.dotc.interfaces.Diagnostic.ERROR: severity = Severity.Error; break;
      case dotty.tools.dotc.interfaces.Diagnostic.WARNING: severity = Severity.Warn; break;
      case dotty.tools.dotc.interfaces.Diagnostic.INFO: severity = Severity.Info; break;
      default:
        throw new IllegalArgumentException(String.format("Bad diagnostic level: %s", level));
    }
    return severity;
  }

  private static Position positionOf(SourcePosition pos) {
    if (pos.exists()){
      return new PositionBridge(pos, pos.source());
    } else {
      return PositionBridge.noPosition;
    }
  }

  @SuppressWarnings("unchecked")
  // [warn] sbt-bridge/src/dotty/tools/xsbt/DelegatingReporter.java:18:1: dotty$tools$dotc$reporting$UniqueMessagePositions$$positions() in dotty.tools.dotc.reporting.AbstractReporter implements dotty$tools$dotc$reporting$UniqueMessagePositions$$positions() in dotty.tools.dotc.reporting.UniqueMessagePositions
  // [warn]   return type requires unchecked conversion from scala.collection.mutable.HashMap to scala.collection.mutable.HashMap<scala.Tuple2<dotty.tools.dotc.util.SourceFile,java.lang.Integer>,dotty.tools.dotc.reporting.Diagnostic>
  public HashMap<Tuple2<SourceFile, Integer>, Diagnostic> dotty$tools$dotc$reporting$UniqueMessagePositions$$positions() {
    return (HashMap<Tuple2<SourceFile, Integer>, Diagnostic>) super.dotty$tools$dotc$reporting$UniqueMessagePositions$$positions();
  }
}
