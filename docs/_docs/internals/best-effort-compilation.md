---
layout: doc-page
title: Best Effort Compilation
---

Best-effort compilation is a compilation mode introduced with the aim of improving IDE integration. It allows to generate
tasty-like artifacts and semanticdb files in erroring programs.

It is composed of two experimental compiler options:
* `-Ybest-effort` produces Best Effort TASTy (`.betasty`) files to the `META-INF/best-effort` directory
* `-Ywith-best-effort-tasty` allows to read Best Effort TASTy files, and if such file is read from the classpath then
limits compilation to the frontend phases

This feature aims to force through to the typer phase regardless of errors, and then serialize tasty-like files
obtained from the error trees into the best effort directory (`META-INF/best-effort`) and also serialize semanticdb as normal.

The exact execution pattern is as follows:

```none
Parser
    │
    │ regardless of errors
    ˅
TyperPhase ────────────────────────────────────┐
    │                                          │
    │                                          │
    │ with errors                              │ no errors
    │                                          │
    │                                          ˅
    │         Every following frontend pass until semanticdb.ExtractSemanticDB (interrupted in the case of errors)
    │                                          │
    │                                          │ regardless of errors
    ˅                                          ˅
semanticdb.ExtractSemanticDB ──────────────────┐
    │                                          │
    │ with errors                              │ no errors
    │                                          │
    │                                          ˅
    │         Every following frontend pass until Pickler (interrupted in the case of errors)
    │                                          │
    │                                          │ regardless of errors
    ˅                                          ˅
Pickler (with added printing of best effort tasty to the best effort target directory)
    │                                          │
    │ with errors                              │ no errors
    ˅                                          ˅
End compilation                      Execute latter passes
```

This is because the IDE is able to retrieve useful info even when skipping phases like PostTyper.

This execution structure where we skip phases depending on the errors found is motivated by the desire
to avoid additionally handling errored trees in as many phases as possible, therefore also decreasing
maintenance load. This way phases like PostTyper do not have to be continually adjusted to handle trees
with errors from typer and usually the IDE is able to retrieve enough information with just the typer phase.

An unfortunate consequence of this structure is the fact that we lose access to phases allowing for incremental
compilation, which is something that could be adressed in the future.

`-Ywith-best-effort-tasty` option allows reading Best Effort TASTy files from classpath. If such file is read, then
the compiler is disallowed from proceeding to any non-frontend phase. This is to be used either in combination with
`-Ybest-effort` option to produce Best Effort TASTy using failing dependencies, or in the Presentation Compiler
to access symbols derived from failing projects.

## Best Effort TASTy format

The Best Effort TASTy (`.betasty`) format is a file format produced by the compiler when the `-Ybest-effort` option
is used. It is characterised by a different header and an addition of the `ERRORtype` type, which represents errored types in
the compiler. The Best Effort TASTy format also extends the regular TASTy grammar to allow the handling of as
large amount of incorrect trees produced by the compiler as possible. The format is defined as part of the
`dotty.tools.besteffort.BestEffortTastyFormat` object.

Since currently the format holds an experimental status, no compatibility rules are defined for now, and the specification
may change between the patch compiler versions, if need be.

For performance reasons, if no errors are detected in the frontend phases, a betasty file mey be serialized in the format of
regular TASTy file, characterized by the use of Tasty header instead of Best Effort TASTy header in the `.betasty` file.

## Testing

The testing procedure reuses the `tests/neg` negative tests that are usually meant to produce errors. First they are compiled
with the `-Ybest-effort` option (testing the TreePickler for errored trees), then later, the tree is reconstructed using
the previously created Best Effort TASTy, with `-Yread-tasty` and `-Ywith-best-effort-tasty` options. This is to test the
TreeUnpickler for those Best Effort TASTy files.

One of the goals of this feature is to keep the maintainance cost low, and to not let this feature hinder the pace of the
overall development of the compiler. Because of that, the tests can be freely disabled in `compiler/neg-best-effort.blacklist`
(testing TreePickler) and `compiler/neg-best-effort-from-tasty.blacklist` (testing TreeUnpickler).
