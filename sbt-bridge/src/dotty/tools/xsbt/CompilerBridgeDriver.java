/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 */

package dotty.tools.xsbt;

import dotty.tools.dotc.Compiler;
import dotty.tools.dotc.Driver;
import dotty.tools.dotc.ScalacCommand;
import dotty.tools.dotc.config.Properties;
import dotty.tools.dotc.core.Contexts;
import dotty.tools.dotc.util.SourceFile;
import dotty.tools.io.AbstractFile;
import dotty.tools.io.PlainFile;
import dotty.tools.io.Path;
import dotty.tools.io.Streamable;
import scala.collection.mutable.ListBuffer;
import scala.jdk.javaapi.CollectionConverters;
import java.util.concurrent.ConcurrentHashMap;
import scala.io.Codec;
import xsbti.Problem;
import xsbti.*;
import xsbti.compile.Output;
import xsbti.compile.CompileProgress;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Comparator;
import java.util.Collections;
import java.util.stream.Collectors;
import java.util.HashMap;
import java.util.Map;
import java.util.Arrays;

public class CompilerBridgeDriver extends Driver {
  private final String[] scalacOptions;
  private final String[] args;

  public CompilerBridgeDriver(String[] scalacOptions, Output output) {
    super();
    this.scalacOptions = scalacOptions;

    if (!output.getSingleOutputAsPath().isPresent())
      throw new IllegalArgumentException("output should be a SingleOutput, was a " + output.getClass().getName());

    this.args = new String[scalacOptions.length + 2];
    System.arraycopy(scalacOptions, 0, args, 0, scalacOptions.length);
    args[scalacOptions.length] = "-d";
    args[scalacOptions.length + 1] = output.getSingleOutputAsPath().get().toAbsolutePath().toString();
  }

  private static final String StopInfoError =
    "Compiler option supplied that disabled Zinc compilation.";

  /**
   * `sourcesRequired` is set to false because the context is set up with no sources
   * The sources are passed programmatically to the compiler in the form of AbstractFiles
   */
  @Override
  public boolean sourcesRequired() {
    return false;
  }

  private static VirtualFile asVirtualFile(SourceFile sourceFile, DelegatingReporter reporter,
      ConcurrentHashMap<AbstractFile, VirtualFile> lookup) {
    // !!!! MUST BE CONCURRENT HASH MAP FOR PARALLEL OUTLINE SECOND PASS !!!!
    return lookup.computeIfAbsent(sourceFile.file(), path -> {
      reportMissingFile(reporter, sourceFile);
      if (sourceFile.file().jpath() != null)
        return new FallbackPathBasedFile(sourceFile);
      else
        return new FallbackVirtualFile(sourceFile);
    });
  }

  private static void reportMissingFile(DelegatingReporter reporter, SourceFile sourceFile) {
    String underline = String.join("", Collections.nCopies(sourceFile.path().length(), "^"));
    String message =
      sourceFile.path() + ": Missing Zinc virtual file\n" +
      underline + "\n" +
      "    Falling back to placeholder for the given source file (of class " + sourceFile.getClass().getName() + ")\n" +
      "    This is likely a bug in incremental compilation for the Scala 3 compiler.\n" +
      "    Please report it to the Scala 3 maintainers at https://github.com/lampepfl/dotty/issues.";
    reporter.reportBasicWarning(message);
  }

  synchronized public void run(
      VirtualFile[] sources, AnalysisCallback callback, Logger log, Reporter delegate, CompileProgress progress) {
    VirtualFile[] sortedSources = new VirtualFile[sources.length];
    System.arraycopy(sources, 0, sortedSources, 0, sources.length);
    Arrays.sort(sortedSources, (x0, x1) -> x0.id().compareTo(x1.id()));

    ListBuffer<AbstractFile> sourcesBuffer = new ListBuffer<>();

    // !!!! MUST BE CONCURRENT HASH MAP FOR PARALLEL OUTLINE SECOND PASS !!!!
    ConcurrentHashMap<AbstractFile, VirtualFile> lookup = new ConcurrentHashMap<>(sources.length, 0.25f);

    for (int i = 0; i < sources.length; i++) {
      VirtualFile source = sortedSources[i];
      AbstractFile abstractFile = asDottyFile(source);
      sourcesBuffer.append(abstractFile);
      lookup.put(abstractFile, source);
    }

    DelegatingReporter reporter = new DelegatingReporter(delegate, sourceFile -> {
      // TODO: possible situation here where we use -from-tasty and TASTy source files but
      // the reporter log is associated to a Scala source file?

      // Zinc will use the output of this function to possibly lookup a mapped virtual file,
      // e.g. convert `${ROOT}/Foo.scala` to `/path/to/Foo.scala` if it exists in the lookup map.
      VirtualFile vf = lookup.get(sourceFile.file());
      if (vf != null)
        return vf.id();
      else
        // follow Zinc, which uses the path of the source file as a fallback.
        return sourceFile.path();
    });

    ProgressCallbackImpl progressCallback = new ProgressCallbackImpl(progress);

    IncrementalCallback incCallback = new IncrementalCallback(callback, sourceFile ->
      asVirtualFile(sourceFile, reporter, lookup)
    );

    try {
      log.debug(this::infoOnCachedCompiler);

      Contexts.Context initialCtx = initCtx()
        .fresh()
        .setReporter(reporter)
        .setIncCallback(incCallback)
        .setProgressCallback(progressCallback);

      Contexts.Context context = setup(args, initialCtx).map(t -> t._2).getOrElse(() -> initialCtx);

      if (ScalacCommand.isHelpFlag(context.settings(), context.settingsState())) {
        throw new InterfaceCompileFailed(args, new Problem[0], StopInfoError);
      }

      if (!delegate.hasErrors()) {
        log.debug(this::prettyPrintCompilationArguments);

        doCompile(sourcesBuffer.toList(), context);

        for (xsbti.Problem problem: delegate.problems()) {
          try {
            AnalysisCallback2 callback2 = (AnalysisCallback2)callback;
            callback2.problem2(
              problem.category(),
              problem.position(),
              problem.message(),
              problem.severity(),
              true, // reported
              problem.rendered(),
              problem.diagnosticCode(),
              problem.diagnosticRelatedInformation(),
              problem.actions()
            );
          } catch (NoClassDefFoundError e) {
            callback.problem(problem.category(), problem.position(), problem.message(), problem.severity(),
              true);
          }
        }
      } else {
        delegate.printSummary();
      }

      if (delegate.hasErrors()) {
        log.debug(() -> "Compilation failed");
        throw new InterfaceCompileFailed(args, delegate.problems(), "Compilation failed");
      }
    } finally {
      reporter.dropDelegate();
    }
  }

  private static AbstractFile asDottyFile(VirtualFile virtualFile) {
    if (virtualFile instanceof PathBasedFile) {
      java.nio.file.Path path = ((PathBasedFile) virtualFile).toPath();
      return new PlainFile(new Path(path));
    }

    try {
      return new dotty.tools.io.VirtualFile(virtualFile.name(), virtualFile.id()) {
        {
          // fill in the content
          try (OutputStream output = output()) {
            try (InputStream input = virtualFile.input()) {
              Streamable.Bytes bytes = new Streamable.Bytes() {
                @Override
                public InputStream inputStream() {
                  return input;
                }
              };
              output.write(bytes.toByteArray());
            }
          }
        }
      };
    } catch (IOException e) {
      throw new IllegalArgumentException("invalid file " + virtualFile.name(), e);
    }
  }

  private String infoOnCachedCompiler() {
    String compilerId = Integer.toHexString(hashCode());
    String compilerVersion;
    try {
      compilerVersion = Properties.versionString();
    } catch (Throwable t) {
      if (scala.util.control.NonFatal.apply(t)) {
        compilerVersion = "unknown";
      } else {
        throw t;
      }
    };
    return String.format("[zinc] Running cached compiler %s for Scala Compiler %s", compilerId, compilerVersion);
  }

  private String prettyPrintCompilationArguments() {
    StringBuilder builder = new StringBuilder();
    builder.append("[zinc] The Scala compiler is invoked with:");
    for (String opt: scalacOptions) {
      builder.append("\n\t");
      builder.append(opt);
    }
    return builder.toString();
  }

}
