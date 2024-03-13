package dotty.tools.pc.completions

/**
 * @param suffixes which we should insert
 * @param snippet which suffix should we insert the snippet $0
 */
case class CompletionAffix(
    suffixes: Set[SuffixKind],
    prefix: PrefixKind,
    snippet: SuffixKind,
):
  def addLabelSnippet = suffixes.contains(SuffixKind.Bracket)
  def hasSnippet = snippet != SuffixKind.NoSuffix
  def chain(copyFn: CompletionAffix => CompletionAffix) = copyFn(this)
  def withNewSuffix(kind: SuffixKind) = this.copy(suffixes = suffixes + kind)
  def withNewPrefix(kind: PrefixKind) = this.copy(prefix = kind)
  def withNewSuffixSnippet(kind: SuffixKind) =
    this.copy(suffixes = suffixes + kind, snippet = kind)
  def toSuffix: String =
    def loop(suffixes: List[SuffixKind]): String =
      def cursor = if suffixes.head == snippet then "$0" else ""
      suffixes match
        case SuffixKind.Brace :: tail => s"($cursor)" + loop(tail)
        case SuffixKind.Bracket :: tail => s"[$cursor]" + loop(tail)
        case SuffixKind.Template :: tail => s" {$cursor}" + loop(tail)
        case _ => ""
    loop(suffixes.toList)
  def toSuffixOpt: Option[String] =
    val edit = toSuffix
    if edit.nonEmpty then Some(edit) else None

  def toPrefix: String = prefix match
    case PrefixKind.New => "new "
    case PrefixKind.NoPrefix => ""

end CompletionAffix

object CompletionAffix:
  val empty = CompletionAffix(
    suffixes = Set.empty,
    prefix = PrefixKind.NoPrefix,
    snippet = SuffixKind.NoSuffix,
  )

enum SuffixKind:
  case Brace, Bracket, Template, NoSuffix

enum PrefixKind:
  case New, NoPrefix
