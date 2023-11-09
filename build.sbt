ThisBuild / usePipelining := true

val scala3 = Build.scala3
val `scala3-bootstrapped` = Build.`scala3-bootstrapped`
val `scala3-interfaces` = Build.`scala3-interfaces`
val `scala3-compiler` = Build.`scala3-compiler`
val `scala3-compiler-bootstrapped` = Build.`scala3-compiler-bootstrapped`
val `scala3-library` = Build.`scala3-library`
val `scala3-library-bootstrapped` = Build.`scala3-library-bootstrapped`
val `scala3-library-bootstrappedJS` = Build.`scala3-library-bootstrappedJS`
val `scala3-sbt-bridge` = Build.`scala3-sbt-bridge`
val `scala3-sbt-bridge-tests` = Build.`scala3-sbt-bridge-tests`
val `scala3-staging` = Build.`scala3-staging`
val `scala3-tasty-inspector` = Build.`scala3-tasty-inspector`
val `scala3-language-server` = Build.`scala3-language-server`
val `scala3-bench` = Build.`scala3-bench`
val `scala3-bench-bootstrapped` = Build.`scala3-bench-bootstrapped`
val `scala3-bench-micro` = Build.`scala3-bench-micro`
val `scala2-library-bootstrapped` = Build.`scala2-library-bootstrapped`
val `scala2-library-tasty` = Build.`scala2-library-tasty`
val `scala2-library-tasty-tests` = Build.`scala2-library-tasty-tests`
val `scala2-library-cc` = Build.`scala2-library-cc`
val `scala2-library-cc-tasty` = Build.`scala2-library-cc-tasty`
val `tasty-core` = Build.`tasty-core`
val `tasty-core-bootstrapped` = Build.`tasty-core-bootstrapped`
val `tasty-core-scala2` = Build.`tasty-core-scala2`
val scaladoc = Build.scaladoc
val `scaladoc-testcases` = Build.`scaladoc-testcases`
val `scaladoc-js-common` = Build.`scaladoc-js-common`
val `scaladoc-js-main` = Build.`scaladoc-js-main`
val `scaladoc-js-contributors` = Build.`scaladoc-js-contributors`
val `scala3-bench-run` = Build.`scala3-bench-run`
val dist = Build.dist
val `community-build` = Build.`community-build`
val `sbt-community-build` = Build.`sbt-community-build`
val `scala3-presentation-compiler` = Build.`scala3-presentation-compiler`
val `scala3-presentation-compiler-bootstrapped` = Build.`scala3-presentation-compiler-bootstrapped`

val sjsSandbox = Build.sjsSandbox
val sjsJUnitTests = Build.sjsJUnitTests
val sjsCompilerTests = Build.sjsCompilerTests

val `sbt-test` = Build.`sbt-test`

inThisBuild(Build.thisBuildSettings)
inScope(Global)(Build.globalSettings)
