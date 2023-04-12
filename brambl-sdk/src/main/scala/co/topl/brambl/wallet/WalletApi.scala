package co.topl.brambl.wallet

import co.topl.crypto.generation.mnemonic.{Entropy, EntropyFailure, MnemonicSize, MnemonicSizes}
import cats.Monad
import cats.implicits.{toFlatMapOps, toFunctorOps}
import co.topl.brambl.dataApi.DataApi
import co.topl.crypto.generation.{Bip32Index, Bip32Indexes, KeyInitializer}
import KeyInitializer.Instances.extendedEd25519Initializer
import co.topl.crypto.encryption.{Mac, VaultStore}
import co.topl.crypto.encryption.kdf.Kdf
import co.topl.crypto.encryption.kdf.SCrypt
import co.topl.crypto.encryption.cipher.Cipher
import co.topl.crypto.encryption.cipher.Aes
import co.topl.crypto.signing.ExtendedEd25519
import co.topl.crypto.signing
import com.google.protobuf.ByteString
import quivr.models._
import quivr.models.VerificationKey._
import quivr.models.SigningKey._
import cats.implicits._

import scala.language.implicitConversions

/**
 * Defines a Wallet API.
 * A Wallet is responsible for managing the user's keys
 */
trait WalletApi[F[_]] {

  /**
   * Create a new wallet
   *
   * @param password   The password to encrypt the wallet with
   * @param passphrase The passphrase to use to generate the main key from the mnemonic
   * @param mLen       The length of the mnemonic to generate
   * @param name       A name used to identify a wallet in the DataApi. Defaults to "default". Most commonly, only one
   *                   wallet identity will be used. It is the responsibility of the dApp to keep track of the names of
   *                   the wallet identities if multiple will be used.
   * @return The mnemonic and VaultStore of the newly created wallet, if successful. Else an error
   */
  def createNewWallet(
    password:   Array[Byte],
    passphrase: Option[String] = None,
    mLen:       MnemonicSize = MnemonicSizes.words12,
    name:       String = "default"
  ): F[Either[WalletApi.FailedToInitializeWallet, WalletApi.NewWalletResult[F]]]

}

object WalletApi {

  /**
   * Create an instance of the WalletAPI
   *
   * @note The wallet uses ExtendedEd25519 to generate the main secret key
   * @note The wallet uses SCrypt as the KDF
   * @note The wallet uses AES as the cipher
   *
   * @param dataApi The DataApi to use to store the generate wallet
   * @return A new WalletAPI instance
   */
  def make[F[_]: Monad](
    dataApi: DataApi[F]
  )(implicit extendedEd25519Instance: ExtendedEd25519 = new ExtendedEd25519): WalletApi[F] = new WalletApi[F] {
    final val Purpose = 1852
    final val CoinType = 7091
    val kdf: Kdf[F] = SCrypt.make[F](SCrypt.SCryptParams(SCrypt.generateSalt))
    val cipher: Cipher[F] = Aes.make[F](Aes.AesParams(Aes.generateIv))

    override def createNewWallet(
      password:   Array[Byte],
      passphrase: Option[String] = None,
      mLen:       MnemonicSize = MnemonicSizes.words12,
      name:       String = "default"
    ): F[Either[FailedToInitializeWallet, NewWalletResult[F]]] = for {
      entropy: Entropy                          <- Monad[F].pure(Entropy.generate(mLen))
      mainKey: Array[Byte]                      <- Monad[F].pure(entropyToMainKey(entropy, passphrase).toByteArray)
      vaultStore: VaultStore[F]                 <- buildMainKeyVaultStore(mainKey, password)
      saveRes: Option[DataApi.DataApiException] <- dataApi.saveMainKeyVaultStore(vaultStore, name).map(_.swap.toOption)
      mnemonic: Either[EntropyFailure, IndexedSeq[String]] <- Monad[F].pure(Entropy.toMnemonicString(entropy))
    } yield saveRes match {
      case Some(saveErr) => FailedToInitializeWallet(mnemonic.swap.toList :+ saveErr).asLeft
      case None => mnemonic.leftMap(mErr => FailedToInitializeWallet(List(mErr))).map(NewWalletResult(_, vaultStore))
    }

    private def buildMainKeyVaultStore(mainKey: Array[Byte], password: Array[Byte]): F[VaultStore[F]] = for {
      derivedKey: Array[Byte] <- kdf.deriveKey(password)
      cipherText: Array[Byte] <- cipher.encrypt(mainKey, derivedKey)
      mac: Array[Byte] = Mac.make(derivedKey, cipherText).value
    } yield VaultStore[F](kdf, cipher, cipherText, mac)

    private def entropyToMainKey(entropy: Entropy, passphrase: Option[String]): KeyPair = {
      val rootKey: ExtendedEd25519.SecretKey = extendedEd25519Initializer.fromEntropy(entropy, passphrase)
      val purpose: Bip32Index = Bip32Indexes.HardenedIndex(Purpose) // following CIP-1852
      val coinType: Bip32Index = Bip32Indexes.HardenedIndex(CoinType) // Topl coin type registered with SLIP-0044
      extendedEd25519Instance.deriveKeyPairFromChildPath(rootKey, List(purpose, coinType))
    }

    implicit private def cryptoToPbKeyPair(
      keyPair: signing.KeyPair[ExtendedEd25519.SecretKey, ExtendedEd25519.PublicKey]
    ): KeyPair = {
      val skCrypto = keyPair.signingKey
      val sk = ExtendedEd25519Sk(
        ByteString.copyFrom(skCrypto.leftKey),
        ByteString.copyFrom(skCrypto.rightKey),
        ByteString.copyFrom(skCrypto.chainCode)
      )
      val vkCrypto = keyPair.verificationKey
      val vk = ExtendedEd25519Vk(
        Ed25519Vk(ByteString.copyFrom(vkCrypto.vk.bytes)),
        ByteString.copyFrom(vkCrypto.chainCode)
      )
      KeyPair(
        VerificationKey(VerificationKey.Vk.ExtendedEd25519(vk)),
        SigningKey(SigningKey.Sk.ExtendedEd25519(sk))
      )
    }
  }

  case class NewWalletResult[F[_]](mnemonic: IndexedSeq[String], mainKeyVaultStore: VaultStore[F])

  trait WalletApiFailure
  case class FailedToInitializeWallet(errs: List[Throwable] = List()) extends WalletApiFailure
}
