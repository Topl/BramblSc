package co.topl.brambl.codecs

import cats.Monad
import cats.implicits.catsSyntaxEitherId
import co.topl.brambl.builders.locks.PropositionTemplate
import co.topl.brambl.builders.locks.PropositionTemplate._
import com.google.protobuf.ByteString
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import org.bouncycastle.util.Strings
import quivr.models.{Data, Digest}

object PropositionTemplateCodecs {
  def encodePropositionTemplate[F[_]: Monad](template: PropositionTemplate[F]): Json = template.asJson

  def decodePropositionTemplate[F[_]: Monad](template: Json): Either[DecodingFailure, PropositionTemplate[F]] =
    template.as[PropositionTemplate[F]]

  /**
   * JSON encoder for a Generic Proposition Template
   */
  implicit def propositionTemplateToJson[F[_]: Monad]: Encoder[PropositionTemplate[F]] =
    new Encoder[PropositionTemplate[F]] {

      override def apply(a: PropositionTemplate[F]): Json =
        Json
          .obj("type" -> Json.fromString(a.propositionType.label))
          .deepMerge(a match {
            case locked: LockedTemplate[F]       => locked.asJson
            case height: HeightTemplate[F]       => height.asJson
            case tick: TickTemplate[F]           => tick.asJson
            case digest: DigestTemplate[F]       => digest.asJson
            case signature: SignatureTemplate[F] => signature.asJson
            case and: AndTemplate[F]             => and.asJson
            case or: OrTemplate[F]               => or.asJson
            case not: NotTemplate[F]             => not.asJson
            case threshold: ThresholdTemplate[F] => threshold.asJson
            case _                               => Json.Null
          })
    }

  /**
   * JSON decoder for a generic Proposition Template
   */
  implicit def propositionTemplateFromJson[F[_]: Monad]: Decoder[PropositionTemplate[F]] =
    new Decoder[PropositionTemplate[F]] {

      override def apply(c: HCursor): Decoder.Result[PropositionTemplate[F]] =
        c.downField("type").as[String] match {
          case Right(PropositionTemplate.types.Locked.label)    => c.as[LockedTemplate[F]]
          case Right(PropositionTemplate.types.Height.label)    => c.as[HeightTemplate[F]]
          case Right(PropositionTemplate.types.Tick.label)      => c.as[TickTemplate[F]]
          case Right(PropositionTemplate.types.Digest.label)    => c.as[DigestTemplate[F]]
          case Right(PropositionTemplate.types.Signature.label) => c.as[SignatureTemplate[F]]
          case Right(PropositionTemplate.types.And.label)       => c.as[AndTemplate[F]]
          case Right(PropositionTemplate.types.Or.label)        => c.as[OrTemplate[F]]
          case Right(PropositionTemplate.types.Not.label)       => c.as[NotTemplate[F]]
          case Right(PropositionTemplate.types.Threshold.label) => c.as[ThresholdTemplate[F]]
          case _ => Left(DecodingFailure("Unknown Proposition Type", c.history))
        }
    }

  /**
   * JSON encoder for a Locked Proposition Template
   */
  implicit def lockedTemplateToJson[F[_]: Monad]: Encoder[LockedTemplate[F]] = new Encoder[LockedTemplate[F]] {

    override def apply(a: LockedTemplate[F]): Json =
      if (a.data.isEmpty) Json.obj()
      else Json.obj("data" -> Json.fromString(a.data.get.value.toString))
  }

  /**
   * JSON decoder for a Locked Proposition Template
   */
  implicit def lockedTemplateFromJson[F[_]: Monad]: Decoder[LockedTemplate[F]] = new Decoder[LockedTemplate[F]] {

    override def apply(c: HCursor): Decoder.Result[LockedTemplate[F]] = c.downField("data").as[String] match {
      case Left(_)     => LockedTemplate[F](None).asRight
      case Right(data) => LockedTemplate[F](Some(Data(ByteString.copyFrom(data.getBytes)))).asRight
    }
  }

  /**
   * JSON encoder for a Height Proposition Template
   */
  implicit def heightTemplateToJson[F[_]: Monad]: Encoder[HeightTemplate[F]] = new Encoder[HeightTemplate[F]] {

    override def apply(a: HeightTemplate[F]): Json = Json.obj(
      "chain" -> Json.fromString(a.chain),
      "min"   -> Json.fromLong(a.min),
      "max"   -> Json.fromLong(a.max)
    )
  }

  /**
   * JSON decoder for a Height Proposition Template
   */
  implicit def heightTemplateFromJson[F[_]: Monad]: Decoder[HeightTemplate[F]] = new Decoder[HeightTemplate[F]] {

    override def apply(c: HCursor): Decoder.Result[HeightTemplate[F]] =
      for {
        chain <- c.downField("chain").as[String]
        min   <- c.downField("min").as[Long]
        max   <- c.downField("max").as[Long]
      } yield HeightTemplate[F](chain, min, max)
  }

  /**
   * JSON encoder for a Tick Proposition Template
   */
  implicit def tickTemplateToJson[F[_]: Monad]: Encoder[TickTemplate[F]] = new Encoder[TickTemplate[F]] {

    override def apply(a: TickTemplate[F]): Json = Json.obj(
      "min" -> Json.fromLong(a.min),
      "max" -> Json.fromLong(a.max)
    )
  }

  /**
   * JSON decoder for a Tick Proposition Template
   */
  implicit def tickTemplateFromJson[F[_]: Monad]: Decoder[TickTemplate[F]] = new Decoder[TickTemplate[F]] {

    override def apply(c: HCursor): Decoder.Result[TickTemplate[F]] =
      for {
        min <- c.downField("min").as[Long]
        max <- c.downField("max").as[Long]
      } yield TickTemplate[F](min, max)
  }

  /**
   * JSON encoder for a Digest Proposition Template
   */
  implicit def digestTemplateToJson[F[_]: Monad]: Encoder[DigestTemplate[F]] = new Encoder[DigestTemplate[F]] {

    override def apply(a: DigestTemplate[F]): Json =
      Json.obj(
        "routine" -> Json.fromString(a.routine),
        "digest"  -> Json.fromString(Strings.fromByteArray(a.digest.value.toByteArray))
      )
  }

  /**
   * JSON decoder for a Digest Proposition Template
   */
  implicit def digestTemplateFromJson[F[_]: Monad]: Decoder[DigestTemplate[F]] = new Decoder[DigestTemplate[F]] {

    override def apply(c: HCursor): Decoder.Result[DigestTemplate[F]] =
      for {
        routine <- c.downField("routine").as[String]
        digest  <- c.downField("digest").as[String]
      } yield DigestTemplate[F](routine, Digest(ByteString.copyFrom(Strings.toByteArray(digest))))
  }

  /**
   * JSON encoder for a Signature Proposition Template
   */
  implicit def signatureTemplateToJson[F[_]: Monad]: Encoder[SignatureTemplate[F]] = new Encoder[SignatureTemplate[F]] {

    override def apply(a: SignatureTemplate[F]): Json = Json.obj(
      "routine"   -> Json.fromString(a.routine),
      "entityIdx" -> Json.fromInt(a.entityIdx)
    )
  }

  /**
   * JSON decoder for a Signature Proposition Template
   */
  implicit def signatureTemplateFromJson[F[_]: Monad]: Decoder[SignatureTemplate[F]] =
    new Decoder[SignatureTemplate[F]] {

      override def apply(c: HCursor): Decoder.Result[SignatureTemplate[F]] =
        for {
          routine   <- c.downField("routine").as[String]
          entityIdx <- c.downField("entityIdx").as[Int]
        } yield SignatureTemplate[F](routine, entityIdx)
    }

  /**
   * JSON encoder for a And Proposition Template
   */
  implicit def andTemplateToJson[F[_]: Monad]: Encoder[AndTemplate[F]] = new Encoder[AndTemplate[F]] {

    override def apply(a: AndTemplate[F]): Json = Json.obj(
      "left"  -> a.leftTemplate.asJson,
      "right" -> a.rightTemplate.asJson
    )
  }

  /**
   * JSON decoder for a And Proposition Template
   */
  implicit def andTemplateFromJson[F[_]: Monad]: Decoder[AndTemplate[F]] = new Decoder[AndTemplate[F]] {

    override def apply(c: HCursor): Decoder.Result[AndTemplate[F]] =
      for {
        left  <- c.downField("left").as[PropositionTemplate[F]]
        right <- c.downField("right").as[PropositionTemplate[F]]
      } yield AndTemplate[F](left, right)
  }

  /**
   * JSON encoder for a Or Proposition Template
   */
  implicit def orTemplateToJson[F[_]: Monad]: Encoder[OrTemplate[F]] = new Encoder[OrTemplate[F]] {

    override def apply(a: OrTemplate[F]): Json = Json.obj(
      "left"  -> a.leftTemplate.asJson,
      "right" -> a.rightTemplate.asJson
    )
  }

  /**
   * JSON decoder for a Or Proposition Template
   */
  implicit def orTemplateFromJson[F[_]: Monad]: Decoder[OrTemplate[F]] = new Decoder[OrTemplate[F]] {

    override def apply(c: HCursor): Decoder.Result[OrTemplate[F]] =
      for {
        left  <- c.downField("left").as[PropositionTemplate[F]]
        right <- c.downField("right").as[PropositionTemplate[F]]
      } yield OrTemplate[F](left, right)
  }

  /**
   * JSON encoder for a Not Proposition Template
   */
  implicit def notTemplateToJson[F[_]: Monad]: Encoder[NotTemplate[F]] = new Encoder[NotTemplate[F]] {

    override def apply(a: NotTemplate[F]): Json = Json.obj(
      "innerTemplate" -> a.innerTemplate.asJson
    )
  }

  /**
   * JSON decoder for a Not Proposition Template
   */
  implicit def notTemplateFromJson[F[_]: Monad]: Decoder[NotTemplate[F]] = new Decoder[NotTemplate[F]] {

    override def apply(c: HCursor): Decoder.Result[NotTemplate[F]] =
      for {
        innerTemplate <- c.downField("innerTemplate").as[PropositionTemplate[F]]
      } yield NotTemplate[F](innerTemplate)
  }

  /**
   * JSON encoder for a Threshold Proposition Template
   */
  implicit def thresholdTemplateToJson[F[_]: Monad]: Encoder[ThresholdTemplate[F]] = new Encoder[ThresholdTemplate[F]] {

    override def apply(a: ThresholdTemplate[F]): Json = Json.obj(
      "threshold"      -> Json.fromInt(a.threshold),
      "innerTemplates" -> a.innerTemplates.asJson
    )
  }

  /**
   * JSON decoder for a Threshold Proposition Template
   */
  implicit def thresholdTemplateFromJson[F[_]: Monad]: Decoder[ThresholdTemplate[F]] =
    new Decoder[ThresholdTemplate[F]] {

      override def apply(c: HCursor): Decoder.Result[ThresholdTemplate[F]] =
        for {
          threshold      <- c.downField("threshold").as[Int]
          innerTemplates <- c.downField("innerTemplates").as[Seq[PropositionTemplate[F]]]
        } yield ThresholdTemplate[F](innerTemplates, threshold)
    }
}
