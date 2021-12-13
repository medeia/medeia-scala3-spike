package medeia.encoder

import org.bson.BsonString
import org.mongodb.scala.bson.{BsonDocument, BsonValue}
import shapeless3.deriving.*

trait BsonEncoder[A]:
  def encode(value: A): BsonValue
  extension (value: A) def toBson = encode(value)

object BsonEncoder:
  given BsonEncoder[String] with
    def encode(value: String): BsonValue = BsonString(value)

  given productEncoder[A](using inst: => K0.ProductInstances[BsonEncoder, A], labelling: Labelling[A]): BsonEncoder[A] with
    def encode(value: A): BsonValue =
      if(labelling.elemLabels.isEmpty)
        BsonString(labelling.label.stripSuffix("$"))
      else
        BsonDocument(labelling.elemLabels.zipWithIndex.map(
          (label, i) =>
            label -> inst.project(value)(i)([t] => (encoder: BsonEncoder[t], pt: t) => encoder.encode(pt))
        ))

  given coproductEncoder[A](using inst: => K0.CoproductInstances[BsonEncoder, A]): BsonEncoder[A] with
    def encode(value: A): BsonValue = BsonString(value.toString)

  inline def derived[A](using gen: K0.Generic[A]): BsonEncoder[A] = gen.derive(productEncoder, coproductEncoder)
