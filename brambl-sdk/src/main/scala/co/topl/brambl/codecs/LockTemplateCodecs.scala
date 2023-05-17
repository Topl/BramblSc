package co.topl.brambl.codecs

import cats.Monad
import co.topl.brambl.builders.locks.{LockTemplate, PropositionTemplate}
import co.topl.brambl.builders.locks.LockTemplate.PredicateTemplate
import co.topl.brambl.codecs.PropositionTemplateCodecs.propositionTemplateToJson
import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}

object LockTemplateCodecs {
  def encodeLockTemplate[F[_]: Monad](template: LockTemplate[F]): Json = template.asJson

  def decodeLockTemplate[F[_]: Monad](template: Json): Either[DecodingFailure, LockTemplate[F]] =
    template.as[LockTemplate[F]]

  /**
   * JSON encoder for a Generic Lock Template
   */
  implicit def lockTemplateToJson[F[_]: Monad]: Encoder[LockTemplate[F]] = new Encoder[LockTemplate[F]] {

    override def apply(a: LockTemplate[F]): Json =
      Json
        .obj("type" -> Json.fromString(a.lockType.label))
        .deepMerge(a match {
          case predicate: PredicateTemplate[F] => predicate.asJson
          case _                               => Json.Null
        })
  }

  /**
   * JSON decoder for a generic Lock Template
   */
  implicit def lockTemplateFromJson[F[_]: Monad]: Decoder[LockTemplate[F]] = new Decoder[LockTemplate[F]] {

    override def apply(c: HCursor): Decoder.Result[LockTemplate[F]] =
      c.downField("type").as[String] match {
        case Right(LockTemplate.types.Predicate.label) => c.as[PredicateTemplate[F]]
        case _                                         => Left(DecodingFailure("Unknown Lock Type", c.history))
      }
  }

  /**
   * JSON encoder for a Predicate Lock Template
   */
  implicit def predicateTemplateToJson[F[_]: Monad]: Encoder[PredicateTemplate[F]] = new Encoder[PredicateTemplate[F]] {

    override def apply(a: PredicateTemplate[F]): Json =
      Json.obj(
        "threshold"      -> Json.fromInt(a.threshold),
        "innerTemplates" -> Json.fromValues(a.innerTemplates.map(_.asJson))
      )

  }

  /**
   * JSON decoder for a Predicate Lock Template
   */
  implicit def predicateTemplateFromJson[F[_]: Monad]: Decoder[PredicateTemplate[F]] =
    new Decoder[PredicateTemplate[F]] {

      implicit private val decodePropositionTemplateSeq: Decoder[Seq[PropositionTemplate[F]]] =
        Decoder[Seq[PropositionTemplate[F]]].prepare(
          _.downField("innerTemplates")
        )

      override def apply(c: HCursor): Decoder.Result[PredicateTemplate[F]] =
        for {
          threshold      <- c.downField("threshold").as[Int]
          innerTemplates <- c.downField("innerTemplates").as[Seq[PropositionTemplate[F]]]
        } yield PredicateTemplate[F](innerTemplates, threshold)
    }
}
