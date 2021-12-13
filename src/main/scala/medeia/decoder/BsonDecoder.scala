package medeia.decoder

import medeia.decoder.BsonDecoderError.TypeMismatch
import org.bson.BsonType
import org.mongodb.scala.bson.BsonValue

@FunctionalInterface
trait BsonDecoder[A]:
  def decode(bson: BsonValue): Either[BsonDecoderError, A]

object Instances:
  given BsonDecoder[String] with
    def decode(bsonValue: BsonValue): Either[BsonDecoderError, String] = withType(BsonType.STRING)(_.asString().getValue)(bsonValue)

  private def withType[A](expectedType: BsonType)(f: BsonValue => A)(bson: BsonValue): Either[BsonDecoderError, A] =
    bson.getBsonType match
      case bsonType if bsonType == expectedType => Right(f(bson))
      case _              => Left(TypeMismatch(bson.getBsonType, expectedType))
