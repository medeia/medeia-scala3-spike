package medeia.decoder

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.mongodb.scala.bson.{BsonDocument, BsonString}
import BsonDecoderError.FieldParseError

import scala.language.postfixOps

class BsonDecoderTest extends AnyFlatSpec with Matchers:
  behavior of "BsonDecoder"

  it should "decode Strings" in {
    import BsonDecoder.given BsonDecoder[?]
    val decoder = summon[BsonDecoder[String]]
    decoder.decode(BsonString("Foo")) should be(Right("Foo"))
  }

  it should "decode case classes" in {
    case class Foo(a: String, b: String) derives BsonDecoder
    import Foo.derived$BsonDecoder

    BsonDocument("a" -> "foo", "b" -> "bar").fromBson should be(Right(Foo("foo", "bar")))
  }

  it should "return error on failure" in {
    case class Foo(a: String, b: String) derives BsonDecoder
    import Foo.derived$BsonDecoder

    BsonDocument().fromBson should be(Left(FieldParseError("a", None)))
  }

  it should "decode sealed traits" in {
    sealed trait T derives BsonDecoder
    case class Foo(a: String) extends T derives BsonDecoder
    case class Bar(x: String) extends T derives BsonDecoder

    import T.derived$BsonDecoder

    val t: T = Foo("s")
    BsonDocument("type" -> "Foo", "a" -> "s").fromBson should be(Right(Foo("s")))
  }

  it should "do error handling on borken type tag" in {
    sealed trait T derives BsonDecoder
    case class Foo(a: String) extends T derives BsonDecoder
    case class Bar(x: String) extends T derives BsonDecoder

    import T.derived$BsonDecoder

    val t: T = Foo("s")
    BsonDocument("type" -> "Borken", "a" -> "s").fromBson should be(Left(FieldParseError("sealed")))
  }
