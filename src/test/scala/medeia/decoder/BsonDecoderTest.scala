package medeia.decoder


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.mongodb.scala.bson.BsonString
import scala.language.postfixOps

class BsonDecoderTest extends AnyFlatSpec with Matchers:
  behavior of "BsonDecoder"

  it should "decode Strings" in {
    import Instances.given BsonDecoder[?]
    val decoder = summon[BsonDecoder[String]]
    decoder.decode(BsonString("Foo")) should be(Right("Foo"))
  }

