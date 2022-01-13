package medeia.decoder

import medeia.decoder.BsonDecoderError.{FieldParseError, TypeMismatch}
import medeia.encoder.BsonEncoder
import org.bson.{BsonString, BsonType}
import org.mongodb.scala.bson.{BsonDocument, BsonValue}
import shapeless3.deriving.{K0, Labelling}

import scala.deriving.Mirror

@FunctionalInterface
trait BsonDecoder[A]:
  def decode(bson: BsonValue): Either[BsonDecoderError, A]
  extension (bson: BsonValue) def fromBson: Either[BsonDecoderError, A] = decode(bson)

object BsonDecoder:
  given BsonDecoder[String] with
    def decode(bsonValue: BsonValue): Either[BsonDecoderError, String] = withType(BsonType.STRING)(_.asString().getValue)(bsonValue)

  private def withType[A](expectedType: BsonType)(f: BsonValue => A)(bson: BsonValue): Either[BsonDecoderError, A] =
    bson.getBsonType match
      case bsonType if bsonType == expectedType => Right(f(bson))
      case _                                    => Left(TypeMismatch(bson.getBsonType, expectedType))

  given productDecoder[A](using inst: => K0.ProductInstances[BsonDecoder, A], labelling: Labelling[A]): BsonDecoder[A] with
    def decode(value: BsonValue): Either[BsonDecoderError, A] =
      type Acc = (IndexedSeq[String], List[FieldParseError])
      val (acc, t) = inst.unfold[Acc]((labelling.elemLabels, List.empty))(
        [t] =>
          (acc: Acc, rt: BsonDecoder[t]) => {
            val (labels, errors) = acc
            val label = labels.head
            val bsonValue = Option(value.asDocument().get(label))
            bsonValue match {
              case Some(value) =>
                rt.decode(value) match {
                  case Left(value) =>
                    val acc: Acc = (labels.tail, FieldParseError(label, None) :: errors)
                    (acc, None)
                  case Right(value) =>
                    val acc: Acc = (labels.tail, errors)
                    (acc, Some(value))
                }
              case None =>
                val acc: Acc = (labels.tail, FieldParseError(label, None) :: errors)
                (acc, None)
            }
        }
      )
      t match {
        case Some(value) => Right(value)
        case None        => Left(FieldParseError(acc._2.map(_.message).mkString(" "), None))
      }

  given coproductDecoder[A](using inst: => K0.CoproductInstances[BsonDecoder, A], labelling: Labelling[A]): BsonDecoder[A] with
    def decode(value: BsonValue): Either[BsonDecoderError, A] =
      val result = labelling.elemLabels.zipWithIndex
        .map((p: (String, Int)) => {
          val (label, i) = p
          if (Option(value.asDocument().get("type")).map(_.asString().getValue).contains(label)) {
            inst.inject[Option[Either[BsonDecoderError, A]]](i)(
              [t <: A] => (rt: BsonDecoder[t]) => Some(rt.decode(value))
            )
          } else None
        })
        .find(_.isDefined)
        .flatten
      result.getOrElse(Left(FieldParseError("sealed", None)))

  inline def derived[A](using gen: K0.Generic[A]): BsonDecoder[A] = gen.derive(productDecoder, coproductDecoder)
