package dotty.tools.dotc.reporting

/** Message kinds that can be used in a Message.
 *  NOTE: Keep in mind that if you have a new message or a new ErrorMessageID
 *  that doesn't fit well into an existing kind, create a new one.
 */
enum MessageKind:
  case NoKind
  case Syntax
  case Type
  case TypeMismatch
  case Naming
  case Declaration
  case NotFound
  case PatternMatch
  case Cyclic
  case Reference
  case DocComment
  case LossyConversion
  case PatternMatchExhaustivity
  case MatchCaseUnreachable
  case Compatibility
  case PotentialIssue
  case CustomCompiletime

  /** Human readable message that will end up being shown to the user.
   *  NOTE: This is only used in the situation where you have multiple words
   *  and don't want to rely on the default toString of the enum.
   */
  def message: String =
    this match
      case NoKind => "No Kind"
      case TypeMismatch => "Type Mismatch"
      case NotFound => "Not Found"
      case PatternMatch => "Pattern Match"
      case DocComment => "Doc Comment"
      case LossyConversion => "Lossy Conversion"
      case PatternMatchExhaustivity => "Pattern Match Exhaustivity"
      case MatchCaseUnreachable => "Match case Unreachable"
      case PotentialIssue => "Potential Issue"
      case kind => kind.toString
end MessageKind
