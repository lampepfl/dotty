package dotty.tools.dotc.sbt.interfaces;

import dotty.tools.dotc.util.SourceFile;

import java.util.EnumSet;
import java.nio.file.Path;

/* User code should not implement this interface, it is intended to be a wrapper around xsbti.AnalysisCallback. */
public interface IncrementalCallback {

  default void api(SourceFile sourceFile, xsbti.api.ClassLike classApi) {
  }

  default void startSource(SourceFile sourceFile) {
  }

  default void mainClass(SourceFile sourceFile, String className) {
  }

  default boolean enabled() {
    return false;
  }

  default void usedName(String className, String name, EnumSet<xsbti.UseScope> useScopes) {
  }

  default void binaryDependency(Path onBinaryEntry, String onBinaryClassName, String fromClassName,
      SourceFile fromSourceFile, xsbti.api.DependencyContext context) {
  }

  default void classDependency(String onClassName, String sourceClassName, xsbti.api.DependencyContext context) {
  }

  default void generatedLocalClass(SourceFile source, Path classFile) {
  }

  default void generatedNonLocalClass(SourceFile source, Path classFile, String binaryClassName,
      String srcClassName) {
  }

  default void apiPhaseCompleted() {
  }

  default void dependencyPhaseCompleted() {
  }
}
