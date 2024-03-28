package dotty.tools.dotc.util

object EnumFlags:

  opaque type FlagSet[E <: reflect.Enum] = Int

  object FlagSet:

    extension [E <: reflect.Enum](set: FlagSet[E])
      def is(flag: E): Boolean = (set & (1 << flag.ordinal)) != 0
      def |(flag: E): FlagSet[E] = (set | (1 << flag.ordinal))

    def empty[E <: reflect.Enum]: FlagSet[E] =
      0
