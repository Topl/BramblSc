package co.topl.crypto.generation

import cats.implicits._
import co.topl.crypto.generation.mnemonic.{Entropy, EntropyFailure, Language}
import co.topl.crypto.signing._
import scodec.bits.{BitVector, ByteVector}
import simulacrum.typeclass

import java.util.UUID
import scala.annotation.unused

trait KeyInitializer {
  self =>

  /**
   * Creates a random secret key
   */
  def random(): ByteVector

  /**
   * Creates a secret key from the given seed
   */
  def fromEntropy(entropy: Entropy, password: Option[String] = None): ByteVector

  /**
   * Creates an instance of a secret key given a byte vector
   *
   * @param bytes bytes of the secret key
   * @return
   */
  def fromBytes(bytes: ByteVector): ByteVector

  /**
   * Create a secret key from a mnemonic string.
   *
   * @param mnemonicString The mnemonic string from which to create the key
   * @param language       The language of the mnemonic string
   * @param password       An optional password to use in creating the key.
   * @return The created secret key.
   */
  def fromMnemonicString(
    mnemonicString: String
  )(language: Language = Language.English, password: Option[String] = None): Either[InitializationFailure, ByteVector] =
    Entropy
      .fromMnemonicString(mnemonicString, language)
      .map(fromEntropy(_, password))
      .leftMap(e => InitializationFailures.FailedToCreateEntropy(e))

  @unused
  def fromBase58String(base58String: String): Either[InitializationFailure, ByteVector] =
    Either
      .fromOption(BitVector.fromBase58(base58String), InitializationFailures.InvalidBase58String)
      .map(bits => fromBytes(bits.toByteVector))

  def fromBase16String(base16String: String): Either[InitializationFailure, ByteVector] =
    Either
      .fromOption(BitVector.fromHex(base16String), InitializationFailures.InvalidBase16String)
      .map(bits => fromBytes(bits.toByteVector))
}

object KeyInitializer {

  trait Instances {

    implicit def ed25519Initializer(implicit ed25519: Ed25519): KeyInitializer =
      new KeyInitializer {

        override def random(): ByteVector =
          fromEntropy(Entropy.fromUuid(UUID.randomUUID()), password = Some(""))

        override def fromEntropy(entropy: Entropy, password: Option[String]): ByteVector =
          ByteVector(
            ed25519.deriveKeyPairFromEntropy(entropy, password).signingKey.bytes
          ) // TODO: Remove Scodec.ByteVector from this file

        override def fromBytes(bytes: ByteVector): ByteVector = bytes
      }
  }

  object Instances extends Instances
}

sealed abstract class InitializationFailure

object InitializationFailures {
  case object InvalidBase58String extends InitializationFailure
  case object InvalidBase16String extends InitializationFailure
  case class FailedToCreateEntropy(entropyFailure: EntropyFailure) extends InitializationFailure
}
