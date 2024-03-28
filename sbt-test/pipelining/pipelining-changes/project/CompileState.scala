// This is necessary because tests are run in batch mode
object CompileState {
  @volatile var previousIterations: Int = -1
}
