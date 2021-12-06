package medeia.decoder

import org.bson.BsonType

enum BsonDecoderError(msg: String, cause: Option[Throwable]) extends Exception(msg, cause.orNull):
  case TypeMismatch(actual: BsonType, expected: BsonType)
      extends BsonDecoderError(s"expected: ${expected.toString}, actual: ${actual.toString}", None)
  case FieldParseError(message: String, cause: Option[Throwable]) extends BsonDecoderError(message, cause)
