package co.topl.brambl.wallet

import cats._
import cats.implicits.catsSyntaxApplicativeId
import cats.implicits.toFlatMapOps
import co.topl.crypto.generation.mnemonic.{Entropy, MnemonicSize, MnemonicSizes}
import cats.Monad
import cats.implicits.{toFlatMapOps, toFunctorOps}
import co.topl.brambl.dataApi.DataApi
import co.topl.crypto.generation.{Bip32Indexes, KeyInitializer}
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
import cats.data.EitherT
import cats.arrow.FunctionK
import co.topl.brambl.models.Indices

import scala.util.Try

/**
 * Defines a Wallet API.
 * A Wallet is responsible for managing the user's keys
 */
trait WalletApi[F[_]] {

  type ToMonad[G[_]] = FunctionK[F, G]

  /**
   * Build a VaultStore for the wallet from a main key encrypted with a password
   *
   * @param mainKey    The main key to use to generate the wallet
   * @param password   The password to encrypt the wallet with
   * @return The mnemonic and VaultStore of the newly created wallet, if successful. Else an error
   */
  def buildMainKeyVaultStore(mainKey: Array[Byte], password: Array[Byte]): F[VaultStore[F]]

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
   * Extract the Main Key Pair from a wallet.
   *
   * @param vaultStore The VaultStore of the wallet to extract the keys from
   * @return The protobuf encoded keys of the wallet, if successful. Else an error
   */
  def extractMainKey(
    vaultStore: VaultStore[F],
    password:   Array[Byte]
  ): F[Either[WalletApi.WalletApiFailure, KeyPair]]

  /**
   * Derive a child key pair from a Main Key Pair.
   *
   * @param keyPair The Main Key Pair to derive the child key pair from
   * @param idx     The path indices of the child key pair to derive
   * @return        The protobuf encoded keys of the child key pair, if successful. Else an error
   */
  def deriveChildKeys(
    keyPair: KeyPair,
    idx:     Indices
  ): F[KeyPair]

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
   * Save a wallet
   *
   * @param name       A name used to identify a wallet in the DataApi. Defaults to "default". Most commonly, only one
   *                   wallet identity will be used. It is the responsibility of the dApp to keep track of the names of
   *                   the wallet identities if multiple will be used.
   * @return The wallet's VaultStore if successful. An error if unsuccessful.
   */
  def loadWallet(name: String = "default"): F[Either[WalletApi.WalletApiFailure, VaultStore[F]]]

  /**
   * Update a wallet
   *
   * @param newWallet The new VaultStore of the wallet
   * @param name A name used to identify a wallet in the DataApi. Defaults to "default". Most commonly, only one
   *             wallet identity will be used. It is the responsibility of the dApp to keep track of the names of
   *             the wallet identities if multiple will be used.
   * @return The wallet's VaultStore if update was successful. An error if unsuccessful.
   */
  def updateWallet(newWallet: VaultStore[F], name: String = "default"): F[Either[WalletApi.WalletApiFailure, Unit]]

  /**
   * Update the password of a wallet
   *
   * @param oldPassword The old password of the wallet
   * @param newPassword The new password to encrypt the wallet with
   * @param name A name used to identify a wallet in the DataApi. Defaults to "default". Most commonly, only one
   *             wallet identity will be used. It is the responsibility of the dApp to keep track of the names of
   *             the wallet identities if multiple will be used.
   * @return The wallet's new VaultStore if creation and save was successful. An error if unsuccessful.
   */
  def updateWalletPassword[G[_]: Monad: ToMonad](
    oldPassword: Array[Byte],
    newPassword: Array[Byte],
    name:        String = "default"
  ): G[Either[WalletApi.WalletApiFailure, VaultStore[F]]] = {
    val toMonad = implicitly[ToMonad[G]]
    (for {
      oldWallet <- EitherT(toMonad(loadWallet(name)))
      mainKey   <- EitherT(toMonad(extractMainKey(oldWallet, oldPassword)))
      newWallet <- EitherT(
        toMonad(buildMainKeyVaultStore(mainKey.toByteArray, newPassword)).map(_.asRight[WalletApi.WalletApiFailure])
      )
      updateRes <- EitherT(toMonad(updateWallet(newWallet, name)).map(_.map(_ => newWallet)))
    } yield updateRes).value
  }

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
        EitherT(toMonad(saveWallet(walletRes.mainKeyVaultStore, name)))
    } yield walletRes).value
  }

  /**
   * Load a wallet and then extract the main key pair
   *
   * @param password The password to decrypt the wallet with
   * @param name     A name used to identify a wallet in the DataApi. Defaults to "default". Most commonly, only one
   *                 wallet identity will be used. It is the responsibility of the dApp to keep track of the names of
   *                 the wallet identities if multiple will be used.
   * @return The main key pair of the wallet, if successful. Else an error
   */
  def loadAndExtractMainKey[G[_]: Monad: ToMonad](
    password: Array[Byte],
    name:     String = "default"
  ): G[Either[WalletApi.WalletApiFailure, KeyPair]] = {
    val toMonad = implicitly[ToMonad[G]]
    (for {
      walletRes <- EitherT(toMonad(loadWallet(name)))
      keyPair   <- EitherT(toMonad(extractMainKey(walletRes, password)))
    } yield keyPair).value
  }

  /**
   * Import a wallet from a mnemonic.
   *
   * @note This method does not persist the imported wallet. It simply generates and returns the VaultStore
   *       corresponding to the mnemonic. See [[importWalletAndSave]]
   *
   * @param mnemonic The mnemonic to import
   * @param password The password to encrypt the wallet with
   * @param passphrase The passphrase to use to generate the main key from the mnemonic
   * @return The wallet's VaultStore if import and save was successful. An error if unsuccessful.
   */
  def importWallet(
    mnemonic:   IndexedSeq[String],
    password:   Array[Byte],
    passphrase: Option[String] = None
  ): F[Either[WalletApi.WalletApiFailure, VaultStore[F]]]

  /**
   * Recover and persist wallet information from the VaultStore and optionally a snapshot.
   *
   * @note Without the snapshot, the wallet can only be partially recovered. The recoverable data includes only address
   *       and fund information for simple 1-of-1 signature transactions. For more complex transactions, the snapshot is
   *       required.
   *
   * @param mainKey The main key of the wallet to recover
   * @param name A name used to identify a wallet in the DataApi. Defaults to "default". Most commonly, only one
   *             wallet identity will be used. It is the responsibility of the dApp to keep track of the names of
   *             the wallet identities if multiple will be used.
   * @param snapshot The snapshot to recover the wallet from
   * @return The wallet's VaultStore if import and save was successful. An error if unsuccessful.
   */
  def recoverWallet(
    mainKey:  KeyPair,
    name:     String = "default",
    snapshot: Option[Any] = None
  ): F[Either[WalletApi.WalletApiFailure, Unit]]

  /**
   * Import a wallet from a mnemonic and save it.
   *
   * @param mnemonic   The mnemonic to import
   * @param password   The password to encrypt the wallet with
   * @param passphrase The passphrase to use to generate the main key from the mnemonic
   * @param name       A name used to identify a wallet in the DataApi. Defaults to "default". Most commonly, only one
   *                   wallet identity will be used. It is the responsibility of the dApp to keep track of the names of
   *                   the wallet identities if multiple will be used.
   * @return The wallet's VaultStore if import and save was successful. An error if unsuccessful.
   */
  def importWalletAndSave[G[_]: Monad: ToMonad](
    mnemonic:   IndexedSeq[String],
    password:   Array[Byte],
    passphrase: Option[String] = None,
    name:       String = "default"
  ): G[Either[WalletApi.WalletApiFailure, VaultStore[F]]] = {
    val toMonad = implicitly[ToMonad[G]]
    (for {
      walletRes  <- EitherT(toMonad(importWallet(mnemonic, password, passphrase)))
      saveRes    <- EitherT(toMonad(saveWallet(walletRes, name)))
      recoverRes <- EitherT(toMonad(recoverWallet(name)))
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

    override def extractMainKey(
      vaultStore: VaultStore[F],
      password:   Array[Byte]
    ): F[Either[WalletApi.WalletApiFailure, KeyPair]] =
      (for {
        decoded <- EitherT[F, WalletApi.WalletApiFailure, Array[Byte]](
          VaultStore.decodeCipher[F](vaultStore, password).map(_.left.map(FailedToDecodeWallet(_)))
        )
        keyPair <- EitherT[F, WalletApi.WalletApiFailure, KeyPair](
          Monad[F].pure(Try(KeyPair.parseFrom(decoded)).toEither.leftMap(x => new FailedToDecodeWallet(x)))
        )
      } yield keyPair).value

    override def deriveChildKeys(
      keyPair: KeyPair,
      idx:     Indices
    ): F[KeyPair] = {
      require(keyPair.vk.vk.isExtendedEd25519, "keyPair must be an extended Ed25519 key")
      require(keyPair.sk.sk.isExtendedEd25519, "keyPair must be an extended Ed25519 key")
      for {
        xCoordinate <- Monad[F].pure(Bip32Indexes.HardenedIndex(idx.x))
        yCoordinate <- Monad[F].pure(Bip32Indexes.SoftIndex(idx.y))
        zCoordinate <- Monad[F].pure(Bip32Indexes.SoftIndex(idx.z))
      } yield extendedEd25519Instance.deriveKeyPairFromChildPath(
        keyPair.signingKey,
        List(xCoordinate, yCoordinate, zCoordinate)
      )
    }

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

    override def importWallet(
      mnemonic:   IndexedSeq[String],
      password:   Array[Byte],
      passphrase: Option[String] = None
    ): F[Either[WalletApiFailure, VaultStore[F]]] = (for {
      entropy <- EitherT(
        Monad[F].pure(Entropy.fromMnemonicString(mnemonic.mkString(" ")).leftMap(FailedToInitializeWallet(_)))
      )
      mainKey    <- EitherT(Monad[F].pure(entropyToMainKey(entropy, passphrase).toByteArray.asRight[WalletApiFailure]))
      vaultStore <- EitherT(buildMainKeyVaultStore(mainKey, password).map(_.asRight[WalletApiFailure]))
    } yield vaultStore).value

    override def recoverWallet(
      mainKey:  KeyPair,
      name:     String = "default",
      snapshot: Option[Any] = None
    ): F[Either[WalletApiFailure, Unit]] = (for {
      // recover 1-of-1 simple information (build lock addresses for each key pair in 0/0/z)
      snapshotRes <- EitherT(snapshot.map(recoverFromSnapshot(_, name)).getOrElse(().asRight[WalletApiFailure].pure[F]))
      // recover complex information from snapshot
      recoverRes <- EitherT(recoverFromMainKey(mainKey, name))
    } yield snapshotRes.combine(recoverRes)).value

    private def recoverFromSnapshot(snapshot: Any, name: String): F[Either[WalletApiFailure, Unit]] =
      ().asRight[WalletApiFailure].pure[F]

    private def recoverFromMainKey(mainKey: KeyPair, name: String): F[Either[WalletApiFailure, Unit]] = {
      require(mainKey.vk.vk.isExtendedEd25519, "keyPair must be an extended Ed25519 key")
      require(mainKey.sk.sk.isExtendedEd25519, "keyPair must be an extended Ed25519 key")
      ().asRight[WalletApiFailure].pure[F]
    }

    override def saveWallet(vaultStore: VaultStore[F], name: String = "default"): F[Either[WalletApiFailure, Unit]] =
      dataApi.saveMainKeyVaultStore(vaultStore, name).map(res => res.leftMap(FailedToSaveWallet(_)))

    override def loadWallet(name: String = "default"): F[Either[WalletApiFailure, VaultStore[F]]] =
      dataApi.getMainKeyVaultStore(name).map(res => res.leftMap(FailedToLoadWallet(_)))

    override def updateWallet(newWallet: VaultStore[F], name: String = "default"): F[Either[WalletApiFailure, Unit]] =
      dataApi.updateMainKeyVaultStore(newWallet, name).map(res => res.leftMap(FailedToUpdateWallet(_)))

    override def buildMainKeyVaultStore(mainKey: Array[Byte], password: Array[Byte]): F[VaultStore[F]] = for {
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
  }

  implicit def pbKeyPairToCryotoKeyPair(
    pbKeyPair: KeyPair
  ): signing.KeyPair[ExtendedEd25519.SecretKey, ExtendedEd25519.PublicKey] =
    signing.KeyPair(
      ExtendedEd25519.SecretKey(
        pbKeyPair.sk.sk.extendedEd25519.get.leftKey.toByteArray,
        pbKeyPair.sk.sk.extendedEd25519.get.rightKey.toByteArray,
        pbKeyPair.sk.sk.extendedEd25519.get.chainCode.toByteArray
      ),
      ExtendedEd25519.PublicKey(
        signing.Ed25519.PublicKey(
          pbKeyPair.vk.vk.extendedEd25519.get.vk.value.toByteArray
        ),
        pbKeyPair.vk.vk.extendedEd25519.get.chainCode.toByteArray
      )
    )

  implicit def cryptoToPbKeyPair(
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

  case class NewWalletResult[F[_]](mnemonic: IndexedSeq[String], mainKeyVaultStore: VaultStore[F])

  abstract class WalletApiFailure(err: Throwable = null) extends RuntimeException(err)
  case class FailedToInitializeWallet(err: Throwable = null) extends WalletApiFailure(err)
  case class FailedToSaveWallet(err: Throwable = null) extends WalletApiFailure(err)
  case class FailedToLoadWallet(err: Throwable = null) extends WalletApiFailure(err)
  case class FailedToUpdateWallet(err: Throwable = null) extends WalletApiFailure(err)
  case class FailedToDecodeWallet(err: Throwable = null) extends WalletApiFailure(err)
}
