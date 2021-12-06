package medeia.encoder

import org.mongodb.scala.bson.BsonValue

trait BsonEncoder[A]:
  def encode(value: A): BsonValue
