package co.topl.quivr

import cats.Id
import cats.implicits._
import co.topl.brambl.models.{Datum, Event}
import co.topl.common.ParsableDataInterface
import co.topl.crypto.hash.Blake2b256
import co.topl.quivr.algebras.{DigestVerifier, SignatureVerifier}
import co.topl.quivr.runtime.{DynamicContext, QuivrRuntimeError, QuivrRuntimeErrors}
import com.google.protobuf.ByteString
import quivr.models._

trait MockHelpers {

  val signableBytes = SignableBytes(ByteString.copyFromUtf8("someSignableBytes"))

  def dynamicContext(proposition: Proposition, proof: Proof): DynamicContext[Id, String, Datum] =
    new DynamicContext[Id, String, Datum] {

      private val mapOfDatums: Map[String, Datum] = Map("height" -> Datum().withHeader(Datum.Header(Event.Header(999))))

      private val mapOfInterfaces: Map[String, ParsableDataInterface] = Map()

      private val mapOfSigningRoutines: Map[String, SignatureVerifier[Id]] = Map(
        "VerySecure" -> new SignatureVerifier[Id] {

          /**
           * Validate a signature
           *
           * This mock implementation only supports Ed25519 SignatureVerification (does not support ExtendedEd25519 Keys).
           * If an unsupported key type is passed in, it will return a UserProvidedInterfaceFailure.
           */
          override def validate(
            t: SignatureVerification
          ): Id[Either[QuivrRuntimeError, SignatureVerification]] = t.verificationKey.vk match {
            case VerificationKey.Vk.Ed25519(vk) =>
              if (
                VerySecureSignatureRoutine
                  .verify(t.signature.value.toByteArray, t.message.value.toByteArray, vk.value.toByteArray)
              )
                Right(t)
              else Left(QuivrRuntimeErrors.ValidationError.MessageAuthorizationFailed(proof))
            case _ => Left(QuivrRuntimeErrors.ValidationError.UserProvidedInterfaceFailure)
          }
        }
      )

      private val mapOfHashingRoutines: Map[String, DigestVerifier[Id]] = Map("blake2b256" -> new DigestVerifier[Id] {

        override def validate(v: DigestVerification): Either[QuivrRuntimeError, DigestVerification] = {
          val test = (new Blake2b256).hash(v.preimage.input.toByteArray ++ v.preimage.salt.toByteArray)
          if (v.digest.value.toByteArray.sameElements(test)) Right(v)
          else Left(QuivrRuntimeErrors.ValidationError.LockedPropositionIsUnsatisfiable)
        }
      })

      override val datums = mapOfDatums.get _

      override val interfaces: Map[String, ParsableDataInterface] = mapOfInterfaces

      override val signingRoutines: Map[String, SignatureVerifier[Id]] = mapOfSigningRoutines

      override val hashingRoutines: Map[String, DigestVerifier[Id]] = mapOfHashingRoutines

      override def signableBytes: Id[SignableBytes] = SignableBytes(ByteString.copyFromUtf8("someSignableBytes"))

      override def currentTick: Id[Long] = 999

      override def heightOf(label: String): Id[Option[Long]] =
        mapOfDatums
          .get(label)
          .flatMap(_.value match {
            case Datum.Value.Header(Datum.Header(Event.Header(height, _), _)) => height.some
            case _                                                            => None
          })
    }
}
