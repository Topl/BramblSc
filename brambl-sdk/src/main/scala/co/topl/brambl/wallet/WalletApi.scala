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
import cats._

import scala.language.implicitConversions
import cats.data.EitherT
import cats.arrow.FunctionK

/**
 * Defines a Wallet API.
 * A Wallet is responsible for managing the user's keys
 */
trait WalletApi[F[_]] {

  type ToMonad[G[_]] = FunctionK[F, G]

  /**
   * Create a new wallet
   *
   * @param password   The password to encrypt the wallet with
   * @param passphrase The passphrase to use to generate the main key from the mnemonic
   * @param mLen       The length of the mnemonic to generate
   * @return The mnemonic and VaultStore of the newly created wallet, if successful. Else an error
   */
  def createNewWallet(
    password:   Array[Byte],
    passphrase: Option[String] = None,
    mLen:       MnemonicSize = MnemonicSizes.words12
  ): F[Either[WalletApi.WalletApiFailure, WalletApi.NewWalletResult[F]]]

  /**
   * Save a wallet
   *
   * @param vaultStore The VaultStore of the wallet to save
   * @param name A name used to identify a wallet in the DataApi. Defaults to "default". Most commonly, only one
   *             wallet identity will be used. It is the responsibility of the dApp to keep track of the names of
   *             the wallet identities if multiple will be used.
   * @return An error if unsuccessful.
   */
  def saveWallet(vaultStore: VaultStore[F], name: String = "default"): F[Either[WalletApi.WalletApiFailure, Unit]]

  /**
   * Create a new wallet and then save it
   *
   * @param password   The password to encrypt the wallet with
   * @param passphrase The passphrase to use to generate the main key from the mnemonic
   * @param mLen       The length of the mnemonic to generate
   * @param name       A name used to identify a wallet in the DataApi. Defaults to "default". Most commonly, only one
   *                   wallet identity will be used. It is the responsibility of the dApp to keep track of the names of
   *                   the wallet identities if multiple will be used.
   * @return The mnemonic and VaultStore of the newly created wallet, if creation and save successful. Else an error
   */
  def createAndSaveNewWallet[G[_]: Monad: ToMonad](
    password:   Array[Byte],
    passphrase: Option[String] = None,
    mLen:       MnemonicSize = MnemonicSizes.words12,
    name:       String = "default"
  ): G[Either[WalletApi.WalletApiFailure, WalletApi.NewWalletResult[F]]] = {
    val toMonad = implicitly[ToMonad[G]]
    (for {
      walletRes <- EitherT(toMonad(createNewWallet(password, passphrase, mLen)))
      createAndSaveRes <-
        EitherT(toMonad(saveWallet(walletRes.mainKeyVaultStore)))
    } yield walletRes).value
  }

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
      mLen:       MnemonicSize = MnemonicSizes.words12
    ): F[Either[WalletApiFailure, NewWalletResult[F]]] = for {
      entropy    <- Monad[F].pure(Entropy.generate(mLen))
      mainKey    <- Monad[F].pure(entropyToMainKey(entropy, passphrase).toByteArray)
      vaultStore <- buildMainKeyVaultStore(mainKey, password)
      mnemonic   <- Monad[F].pure(Entropy.toMnemonicString(entropy))
    } yield mnemonic.leftMap(FailedToInitializeWallet(_)).map(NewWalletResult(_, vaultStore))

    override def saveWallet(vaultStore: VaultStore[F], name: String = "default"): F[Either[WalletApiFailure, Unit]] =
      dataApi.saveMainKeyVaultStore(vaultStore, name).map(res => res.leftMap(FailedToSaveWallet(_)))

    private def buildMainKeyVaultStore(mainKey: Array[Byte], password: Array[Byte]): F[VaultStore[F]] = for {
      derivedKey <- kdf.deriveKey(password)
      cipherText <- cipher.encrypt(mainKey, derivedKey)
      mac = Mac.make(derivedKey, cipherText).value
    } yield VaultStore[F](kdf, cipher, cipherText, mac)

    private def entropyToMainKey(entropy: Entropy, passphrase: Option[String]): KeyPair = {
      val rootKey = extendedEd25519Initializer.fromEntropy(entropy, passphrase)
      val purpose = Bip32Indexes.HardenedIndex(Purpose) // following CIP-1852
      val coinType = Bip32Indexes.HardenedIndex(CoinType) // Topl coin type registered with SLIP-0044
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

  abstract class WalletApiFailure(err: Throwable = null) extends RuntimeException(err)
  case class FailedToInitializeWallet(err: Throwable = null) extends WalletApiFailure(err)
  case class FailedToSaveWallet(err: Throwable = null) extends WalletApiFailure(err)
}
