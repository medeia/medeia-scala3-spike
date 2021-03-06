package medeia.encoder

import org.mongodb.scala.bson.{BsonDocument, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps

class BsonEncoderTest extends AnyFlatSpec with Matchers:
  behavior of "BsonEncoder"

  it should "encode Strings" in {
    import BsonEncoder.given BsonEncoder[?]
    "foo".toBson should ===(BsonString("foo"))
  }

  it should "encode case classes" in {
    case object Bar derives BsonEncoder
    case class Foo(a: String, b: Bar.type) derives BsonEncoder

    Foo("foo", Bar).toBson should ===(BsonDocument("a" -> "foo", "b" -> "Bar"))
  }

  it should "encode sealed traits" in {
    sealed trait T derives BsonEncoder
    case class Foo(a: String) extends T derives BsonEncoder
    case class Bar(x: String) extends T derives BsonEncoder

    val t: T = Foo("s")
    t.toBson should ===(BsonDocument("type" -> "Foo", "a" -> "s"))
  }
