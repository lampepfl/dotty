package dotty.tools.xsbt;

import dotty.tools.dotc.util.SourceFile;
import java.util.function.Function;

public final class IncrementalCallback implements dotty.tools.dotc.sbt.interfaces.IncrementalCallback {

  private final xsbti.AnalysisCallback delegate;
  private final Function<SourceFile, xsbti.VirtualFile> asVirtualFile;

  public IncrementalCallback(xsbti.AnalysisCallback delegate, Function<SourceFile, xsbti.VirtualFile> asVirtualFile) {
    this.delegate = delegate;
    this.asVirtualFile = asVirtualFile;
  }

  @Override
  public void api(SourceFile sourceFile, xsbti.api.ClassLike classApi) {
    delegate.api(asVirtualFile.apply(sourceFile), classApi);
  }

  @Override
  public void startSource(SourceFile sourceFile) {
    delegate.startSource(asVirtualFile.apply(sourceFile));
  }

  @Override
  public void mainClass(SourceFile sourceFile, String className) {
    delegate.mainClass(asVirtualFile.apply(sourceFile), className);
  }

  @Override
  public boolean enabled() {
    return delegate.enabled();
  }

  @Override
  public void usedName(String className, String name, java.util.EnumSet<xsbti.UseScope> useScopes) {
    delegate.usedName(className, name, useScopes);
  }

  @Override
  public void binaryDependency(java.nio.file.Path onBinaryEntry, String onBinaryClassName, String fromClassName, SourceFile fromSourceFile, xsbti.api.DependencyContext context) {
    delegate.binaryDependency(onBinaryEntry, onBinaryClassName, fromClassName, asVirtualFile.apply(fromSourceFile), context);
  }

  @Override
  public void classDependency(String onClassName, String sourceClassName, xsbti.api.DependencyContext context) {
    delegate.classDependency(onClassName, sourceClassName, context);
  }

  @Override
  public void generatedLocalClass(SourceFile source, java.nio.file.Path classFile) {
    delegate.generatedLocalClass(asVirtualFile.apply(source), classFile);
  }

  @Override
  public void generatedNonLocalClass(SourceFile source, java.nio.file.Path classFile, String binaryClassName, String srcClassName) {
    delegate.generatedNonLocalClass(asVirtualFile.apply(source), classFile, binaryClassName, srcClassName);
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
