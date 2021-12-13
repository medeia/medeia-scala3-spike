package medeia.encoder

import org.bson.BsonString
import org.mongodb.scala.bson.BsonValue
import shapeless3.deriving._

trait BsonEncoder[A]:
  def encode(value: A): BsonValue
  extension (value: A) def toBson = encode(value)

object BsonEncoder:
  given BsonEncoder[String] with
    def encode(value: String): BsonValue = BsonString(value)

  given productEncoder[A](using inst: => K0.ProductInstances[BsonEncoder, A], labelling: Labelling[A]): BsonEncoder[A] with
    def encode(value: A): BsonValue =
      print(labelling.label)
      print(labelling.elemLabels)
      BsonString(value.toString)

  given coproductEncoder[A](using inst: => K0.CoproductInstances[BsonEncoder, A]): BsonEncoder[A] with
    def encode(value: A): BsonValue = BsonString(value.toString)

  inline def derived[A](using gen: K0.Generic[A]): BsonEncoder[A] = gen.derive(productEncoder, coproductEncoder)
