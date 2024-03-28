class S {
  locally {
    // OfType Implicits

    import java.nio.charset.StandardCharsets
    import scala.io.Codec

    val c: Codec = StandardCharsets.UTF_8 // error
  }
}