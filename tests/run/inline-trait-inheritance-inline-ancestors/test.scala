import normaltraits.normalValues
import inlinetraits.inlineValues

@main def Test =
  assert(normalValues == inlineValues)