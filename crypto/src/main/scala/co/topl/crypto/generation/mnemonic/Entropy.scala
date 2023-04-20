package co.topl.crypto.generation.mnemonic

import cats.implicits._
import co.topl.crypto.generation.mnemonic.EntropyFailures.InvalidByteSize
import co.topl.crypto.generation.mnemonic.Language.LanguageWordList

import java.util.UUID

/**
 * Wrapper around the entropy contained represented by an array of bytes
 * @param value the underlying bytes of entropy
 */
case class Entropy(value: Array[Byte])

object Entropy {

  /**
   * Generate an Entropy of the specified size.
   * @param size one of the values defined in [[MnemonicSizes]]
   * @return the generated Entropy
   */
  def generate(size: MnemonicSize = MnemonicSizes.words12): Entropy = {
    val numBytes = size.entropyLength / byteLen
    val r = new Array[Byte](numBytes)
    new java.security.SecureRandom().nextBytes(r) // overrides r
    Entropy(r)
  }

  /**
   * Generate an mnemonic string from an `Entropy` value
   *
   * @param entropy The entropy value from which to compute the mnemonic
   * @param language The language of the mnemonic string
   * @return
   */
  def toMnemonicString(
    entropy:  Entropy,
    language: Language = Language.English
  ): Either[EntropyFailure, IndexedSeq[String]] =
    for {
      size   <- sizeFromEntropyLength(entropy.value.length.toInt)
      phrase <- Phrase.fromEntropy(entropy, size, language).leftMap(EntropyFailures.PhraseToEntropyFailure)
    } yield phrase.value

  /**
   * Instantiates an 'Entropy' value from a string by validating the string to a mnemonic phrase and then deriving
   * the entropy of the string according to the BIP-39 wordlists
   * @param mnemonic string to be decoded to a mnemonic
   * @param language applicable language to pull the wordlist for
   * @return either an entropy encode failure or the entropy for use in key derivation
   */
  def fromMnemonicString(
    mnemonic: String,
    language: Language = Language.English
  ): Either[EntropyFailure, Entropy] =
    Phrase
      .validated(mnemonic, language)
      .map(Entropy.unsafeFromPhrase)
      .leftMap(EntropyFailures.PhraseToEntropyFailure)

  /**
   * Instantiates a 128-bit `Entropy` (12 word) value from a given `UUID`.
   * @param uuid a UUID to convert into `Entropy`
   * @return an `Entropy` value
   */
  def fromUuid(uuid: UUID): Entropy =
    Entropy(
      uuid.toString
        .filterNot("-".toSet)
        .grouped(2)
        .map(Integer.parseInt(_, 16).toByte)
        .toArray
    )

  /**
   * Instantiates an `Entropy` value from byte data for an expected mnemonic size.
   * @param bytes the byte data to convert into entropy
   * @return either a `ValidationFailure` if the byte data is invalid or `Entropy` if it is valid
   */
  def fromBytes(bytes: Array[Byte]): Either[EntropyFailure, Entropy] = for {
    _ <- sizeFromEntropyLength(bytes.length)
    entropy = Entropy(bytes)
  } yield entropy

  /**
   * Instantiates an `Entropy` value from a `Phrase`, `LanguageWordList`, and `MnemonicSize`.
   *
   * Note: the phrase is not re-validated for the given `LanguageWordList` and `MnemonicSize`
   *
   * @param phrase the mnemonic phrase to get entropy from
   * @return the underlying entropy of the mnemonic phrase
   */
  private[mnemonic] def unsafeFromPhrase(phrase: Phrase): Entropy =
    Entropy(
      Phrase
        .toBinaryString(phrase)
        ._1 // extract the entropy from the Phrase
        .grouped(byteLen) // group into bytes
        .map(Integer.parseInt(_, 2).toByte) // interpret the binary string as a List[Byte]
        .toArray
    )

  private[mnemonic] def sizeFromEntropyLength(entropyByteLength: Int): Either[EntropyFailure, MnemonicSize] =
    entropyByteLength match {
      case 16 => Right(MnemonicSizes.words12)
      case 20 => Right(MnemonicSizes.words15)
      case 24 => Right(MnemonicSizes.words18)
      case 28 => Right(MnemonicSizes.words21)
      case 32 => Right(MnemonicSizes.words24)
      case _  => Left(InvalidByteSize)
    }
}

sealed trait EntropyFailure extends RuntimeException

/**
 * Enumeration of errors that can be produced when creating a mnemonic from entropy.
 */
object EntropyFailures {
  case object InvalidByteSize extends EntropyFailure
  case class PhraseToEntropyFailure(failure: PhraseFailure) extends EntropyFailure
  case class WordListFailure(failure: LanguageWordList.ValidationFailure) extends EntropyFailure
  case object InvalidSizeMismatch extends EntropyFailure
}
