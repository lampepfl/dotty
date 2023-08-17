package dotty.tools.xsbt;

import dotty.tools.dotc.util.SourceFile;
import java.util.function.Function;
import java.util.Optional;

import java.io.File;

/** To be compatible with the Zinc 1.3 API */
public final class OldIncrementalCallback implements dotty.tools.dotc.sbt.interfaces.IncrementalCallback {

  private final xsbti.AnalysisCallback delegate;

  public OldIncrementalCallback(xsbti.AnalysisCallback delegate) {
    this.delegate = delegate;
  }

  private static File asJavaFile(SourceFile sourceFile) {
    File jfileOrNull = sourceFile.file().file();
    if (jfileOrNull != null) return jfileOrNull;
    throw new IllegalArgumentException("SourceFile " + sourceFile + " is not backed by a java.io.File");
  }

  @SuppressWarnings("deprecation")
  @Override
  public void api(SourceFile sourceFile, xsbti.api.ClassLike classApi) {
    delegate.api(asJavaFile(sourceFile), classApi);
  }

  @SuppressWarnings("deprecation")
  @Override
  public void startSource(SourceFile sourceFile) {
    delegate.startSource(asJavaFile(sourceFile));
  }

  @SuppressWarnings("deprecation")
  @Override
  public void mainClass(SourceFile sourceFile, String className) {
    delegate.mainClass(asJavaFile(sourceFile), className);
  }

  @Override
  public boolean enabled() {
    return delegate.enabled();
  }

  @Override
  public void usedName(String className, String name, java.util.EnumSet<xsbti.UseScope> useScopes) {
    delegate.usedName(className, name, useScopes);
  }

  @SuppressWarnings("deprecation")
  @Override
  public void binaryDependency(java.nio.file.Path onBinaryEntry, String onBinaryClassName, String fromClassName, SourceFile fromSourceFile, xsbti.api.DependencyContext context) {
    delegate.binaryDependency(onBinaryEntry.toFile(), onBinaryClassName, fromClassName, asJavaFile(fromSourceFile), context);
  }

  @Override
  public void classDependency(String onClassName, String sourceClassName, xsbti.api.DependencyContext context) {
    delegate.classDependency(onClassName, sourceClassName, context);
  }

  @SuppressWarnings("deprecation")
  @Override
  public void generatedLocalClass(SourceFile source, java.nio.file.Path classFile) {
    delegate.generatedLocalClass(asJavaFile(source), classFile.toFile());
  }

  @SuppressWarnings("deprecation")
  @Override
  public void generatedNonLocalClass(SourceFile source, java.nio.file.Path classFile, String binaryClassName, String srcClassName) {
    delegate.generatedNonLocalClass(asJavaFile(source), classFile.toFile(), binaryClassName, srcClassName);
  }

  @Override
  public void apiPhaseCompleted() {
    delegate.apiPhaseCompleted();
  }

  @Override
  public void dependencyPhaseCompleted() {
    delegate.dependencyPhaseCompleted();
  }
}
