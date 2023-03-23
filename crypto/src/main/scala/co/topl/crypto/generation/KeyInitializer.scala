package co.topl.crypto.generation

import cats.implicits._
import co.topl.crypto.generation.mnemonic.{Entropy, EntropyFailure, Language}
import co.topl.crypto.signing._

import java.util.UUID

/**
 * Provides functionality for creating secret keys
 * @tparam SK type of secret key
 */
trait KeyInitializer[SK <: SigningKey] {
  self =>

  /**
   * Creates a random secret key
   */
  def random(): SK

  /**
   * Creates a secret key from the given seed
   */
  def fromEntropy(entropy: Entropy, password: Option[String] = None): SK

  /**
   * Creates an instance of a secret key given a byte vector
   *
   * @param bytes bytes of the secret key
   * @return
   */
  def fromBytes(bytes: Array[Byte]): SK

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
  )(language: Language = Language.English, password: Option[String] = None): Either[InitializationFailure, SK] =
    Entropy
      .fromMnemonicString(mnemonicString, language)
      .map(fromEntropy(_, password))
      .leftMap(e => InitializationFailures.FailedToCreateEntropy(e))
}

object KeyInitializer {

  trait Instances {

    implicit def ed25519Initializer(implicit ed25519: Ed25519): KeyInitializer[Ed25519.SecretKey] =
      new KeyInitializer[Ed25519.SecretKey] {

        override def random(): Ed25519.SecretKey =
          fromEntropy(Entropy.fromUuid(UUID.randomUUID()), password = Some(""))

        override def fromEntropy(entropy: Entropy, password: Option[String]): Ed25519.SecretKey =
          ed25519.deriveKeyPairFromEntropy(entropy, password).signingKey

        override def fromBytes(bytes: Array[Byte]): Ed25519.SecretKey = Ed25519.SecretKey(bytes)
      }

    implicit def extendedEd25519Initializer(implicit
      extendedEd25519: ExtendedEd25519
    ): KeyInitializer[ExtendedEd25519.SecretKey] =
      new KeyInitializer[ExtendedEd25519.SecretKey] {

        def random(): ExtendedEd25519.SecretKey =
          fromEntropy(Entropy.fromUuid(UUID.randomUUID()), password = Some(""))

        def fromEntropy(entropy: Entropy, password: Option[String]): ExtendedEd25519.SecretKey =
          extendedEd25519.deriveKeyPairFromEntropy(entropy, password).signingKey

        def fromBytes(bytes: Array[Byte]): ExtendedEd25519.SecretKey =
          ExtendedEd25519.SecretKey(bytes.slice(0, 32), bytes.slice(32, 64), bytes.slice(64, 96))
      }
  }

  object Instances extends Instances
}

sealed abstract class InitializationFailure

object InitializationFailures {
  case class FailedToCreateEntropy(entropyFailure: EntropyFailure) extends InitializationFailure
}
