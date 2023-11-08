package co.topl.brambl.wallet

import cats.implicits.catsSyntaxApplicativeId
import co.topl.crypto.generation.mnemonic.{Entropy, MnemonicSize, MnemonicSizes}
import cats.{Eq, Monad}
import cats.implicits.{toFlatMapOps, toFunctorOps}
import co.topl.brambl.dataApi.WalletKeyApiAlgebra
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
import co.topl.brambl.syntax.{cryptoToPbKeyPair, cryptoVkToPbVk, pbKeyPairToCryptoKeyPair, pbVkToCryptoVk}
import cats.implicits._

import scala.language.implicitConversions
import cats.data.EitherT
import cats.arrow.FunctionK
import cats.effect.IO.asyncForIO
import cats.effect.kernel.implicits._
import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.{IO, Resource}
import co.topl.brambl.models.Indices
import co.topl.brambl.utils.CatsUnsafeResource

import scala.util.Try

/**
 * Defines a Wallet API.
 * A Wallet is responsible for managing the user's keys
 */
trait WalletApi[F[_]] {

  type ToMonad[G[_]] = FunctionK[F, G]

  /**
   * Save a wallet
   *
   * @param vaultStore The VaultStore of the wallet to save
   * @param name       A name used to identify a wallet. Defaults to "default". Most commonly, only one
   *                   wallet identity will be used. It is the responsibility of the dApp to keep track of the names of
   *                   the wallet identities if multiple will be used.
   * @return An error if unsuccessful.
   */
  def saveWallet(vaultStore: VaultStore[F], name: String = "default"): F[Either[WalletApi.WalletApiFailure, Unit]]

  /**
   * Save a mnemonic
   *
   * @param mnemonic The mnemonic to save
   * @param mnemonicName A name used to identify the mnemonic. Defaults to "mnemonic".
   * @return
   */
  def saveMnemonic(mnemonic: IndexedSeq[String], mnemonicName: String): F[Either[WalletApi.WalletApiFailure, Unit]]

  /**
   * Save a wallet
   *
   * @param name A name used to identify a wallet. Defaults to "default". Most commonly, only one
   *             wallet identity will be used. It is the responsibility of the dApp to keep track of the names of
   *             the wallet identities if multiple will be used.
   * @return The wallet's VaultStore if successful. An error if unsuccessful.
   */
  def loadWallet(name: String = "default"): F[Either[WalletApi.WalletApiFailure, VaultStore[F]]]

  /**
   * Update a wallet
   *
   * @param newWallet The new VaultStore of the wallet
   * @param name      A name used to identify a wallet. Defaults to "default". Most commonly, only one
   *                  wallet identity will be used. It is the responsibility of the dApp to keep track of the names of
   *                  the wallet identities if multiple will be used.
   * @return An error if unsuccessful.
   */
  def updateWallet(newWallet: VaultStore[F], name: String = "default"): F[Either[WalletApi.WalletApiFailure, Unit]]

  /**
   * Delete a wallet
   *
   * @param name A name used to identify the wallet. Defaults to "default". Most commonly, only one
   *             wallet identity will be used. It is the responsibility of the dApp to keep track of the names of
   *             the wallet identities if multiple will be used.
   * @return  An error if unsuccessful.
   */
  def deleteWallet(name: String = "default"): F[Either[WalletApi.WalletApiFailure, Unit]]

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
   * Create a new wallet and then save it
   *
   * @param password   The password to encrypt the wallet with
   * @param passphrase The passphrase to use to generate the main key from the mnemonic
   * @param mLen       The length of the mnemonic to generate
   * @param name       A name used to identify a wallet. Defaults to "default". Most commonly, only one
   *                   wallet identity will be used. It is the responsibility of the dApp to keep track of the names of
   *                   the wallet identities if multiple will be used.
   * @param mnemonicName A name used to identify the mnemonic. Defaults to "mnemonic".
   * @return The mnemonic and VaultStore of the newly created wallet, if creation and save successful. Else an error
   */
  def createAndSaveNewWallet[G[_]: Monad: ToMonad](
    password:     Array[Byte],
    passphrase:   Option[String] = None,
    mLen:         MnemonicSize = MnemonicSizes.words12,
    name:         String = "default",
    mnemonicName: String = "mnemonic"
  ): G[Either[WalletApi.WalletApiFailure, WalletApi.NewWalletResult[F]]] = {
    val toMonad = implicitly[ToMonad[G]]
    (for {
      walletRes <- EitherT(toMonad(createNewWallet(password, passphrase, mLen)))
      saveWalletRes <-
        EitherT(toMonad(saveWallet(walletRes.mainKeyVaultStore, name)))
      saveMnemonicRes <-
        EitherT(toMonad(saveMnemonic(walletRes.mnemonic, mnemonicName)))
    } yield walletRes).value
  }

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
   * Derive a child key pair from a Main Key Pair from a partial path (x and y).
   *
   * @param keyPair The Main Key Pair to derive the child key pair from
   * @param xFellowship  The first path index of the child key pair to derive. Represents the fellowship index
   * @param yTemplate The second path index of the child key pair to derive. Represents the contract index
   * @return        The protobuf encoded keys of the child key pair
   */
  def deriveChildKeysPartial(
    keyPair:     KeyPair,
    xFellowship: Int,
    yTemplate:   Int
  ): F[KeyPair]

  /**
   * Derive a child verification key pair one step down from a parent verification key. Note that this is a Soft
   * Derivation.
   *
   * @param vk The verification to derive the child key pair from
   * @param idx     The index to perform soft derivation in order to derive the child verification
   * @return        The protobuf child verification key
   */
  def deriveChildVerificationKey(vk: VerificationKey, idx: Int): F[VerificationKey]

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
      walletRes <- EitherT(toMonad(importWallet(mnemonic, password, passphrase)))
      saveRes   <- EitherT(toMonad(saveWallet(walletRes, name)))
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
   * @param walletKeyApi The Api to use to handle wallet key persistence
   * @return A new WalletAPI instance
   */
  def make[F[_]: Async](walletKeyApi: WalletKeyApiAlgebra[F]): WalletApi[F] = new WalletApi[F] {
    final val Purpose = 1852
    final val CoinType = 7091
    val kdf: Kdf[F] = SCrypt.make[F](SCrypt.SCryptParams(SCrypt.generateSalt))
    val cipher: Cipher[F] = Aes.make[F](Aes.AesParams(Aes.generateIv))

    val extendedEd25519Resource: F[Resource[F, ExtendedEd25519]] =
      CatsUnsafeResource.make[F, ExtendedEd25519](new ExtendedEd25519, 1)

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
        xCoordinate             <- Monad[F].pure(Bip32Indexes.HardenedIndex(idx.x))
        yCoordinate             <- Monad[F].pure(Bip32Indexes.SoftIndex(idx.y))
        zCoordinate             <- Monad[F].pure(Bip32Indexes.SoftIndex(idx.z))
        extendedEd25519Instance <- extendedEd25519Resource
        res <- extendedEd25519Instance.use(instance =>
          Monad[F].pure(
            instance.deriveKeyPairFromChildPath(
              keyPair.signingKey,
              List(xCoordinate, yCoordinate, zCoordinate)
            )
          )
        )
      } yield res
    }

    override def deriveChildKeysPartial(
      keyPair:     KeyPair,
      xFellowship: Int,
      yTemplate:   Int
    ): F[KeyPair] = {
      require(keyPair.vk.vk.isExtendedEd25519, "keyPair must be an extended Ed25519 key")
      require(keyPair.sk.sk.isExtendedEd25519, "keyPair must be an extended Ed25519 key")
      for {
        xCoordinate             <- Monad[F].pure(Bip32Indexes.HardenedIndex(xFellowship))
        yCoordinate             <- Monad[F].pure(Bip32Indexes.SoftIndex(yTemplate))
        extendedEd25519Instance <- extendedEd25519Resource
        res <- extendedEd25519Instance.use(instance =>
          Monad[F].pure(
            instance.deriveKeyPairFromChildPath(
              keyPair.signingKey,
              List(xCoordinate, yCoordinate)
            )
          )
        )
      } yield res
    }

    override def deriveChildVerificationKey(
      vk:  VerificationKey,
      idx: Int
    ): F[VerificationKey] = {
      require(vk.vk.isExtendedEd25519, "verification key must be an extended Ed25519 key")
      for {
        extendedEd25519Instance <- extendedEd25519Resource
        res <- extendedEd25519Instance.use(instance =>
          Monad[F].pure(
            VerificationKey(
              VerificationKey.Vk.ExtendedEd25519(
                instance.deriveChildVerificationKey(vk.vk.extendedEd25519.get, Bip32Indexes.SoftIndex(idx))
              )
            )
          )
        )
      } yield res
    }

    override def createNewWallet(
      password:   Array[Byte],
      passphrase: Option[String] = None,
      mLen:       MnemonicSize = MnemonicSizes.words12
    ): F[Either[WalletApiFailure, NewWalletResult[F]]] = for {
      entropy    <- Monad[F].pure(Entropy.generate(mLen))
      mainKeyRaw <- entropyToMainKey(entropy, passphrase)
      mainKey    <- Monad[F].pure(mainKeyRaw.toByteArray)
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
      mainKeyRaw <- EitherT(entropyToMainKey(entropy, passphrase).map(_.asRight[WalletApiFailure]))
      mainKey    <- EitherT(Monad[F].pure(mainKeyRaw.toByteArray.asRight[WalletApiFailure]))
      vaultStore <- EitherT(buildMainKeyVaultStore(mainKey, password).map(_.asRight[WalletApiFailure]))
    } yield vaultStore).value

    override def saveWallet(vaultStore: VaultStore[F], name: String = "default"): F[Either[WalletApiFailure, Unit]] =
      walletKeyApi.saveMainKeyVaultStore(vaultStore, name).map(res => res.leftMap(FailedToSaveWallet(_)))

    override def saveMnemonic(
      mnemonic:     IndexedSeq[String],
      mnemonicName: String = "mnemonic"
    ): F[Either[WalletApi.WalletApiFailure, Unit]] =
      walletKeyApi.saveMnemonic(mnemonic, mnemonicName).map(res => res.leftMap(FailedToSaveMnemonic(_)))

    override def loadWallet(name: String = "default"): F[Either[WalletApiFailure, VaultStore[F]]] =
      walletKeyApi.getMainKeyVaultStore(name).map(res => res.leftMap(FailedToLoadWallet(_)))

    override def updateWallet(newWallet: VaultStore[F], name: String = "default"): F[Either[WalletApiFailure, Unit]] =
      walletKeyApi.updateMainKeyVaultStore(newWallet, name).map(res => res.leftMap(FailedToUpdateWallet(_)))

    override def deleteWallet(name: String = "default"): F[Either[WalletApiFailure, Unit]] =
      walletKeyApi.deleteMainKeyVaultStore(name).map(res => res.leftMap(FailedToDeleteWallet(_)))

    override def buildMainKeyVaultStore(mainKey: Array[Byte], password: Array[Byte]): F[VaultStore[F]] = for {
      derivedKey <- kdf.deriveKey(password)
      cipherText <- cipher.encrypt(mainKey, derivedKey)
      mac = Mac.make(derivedKey, cipherText).value
    } yield VaultStore[F](kdf, cipher, cipherText, mac)

    private def entropyToMainKey(entropy: Entropy, passphrase: Option[String]): F[KeyPair] = {
      val purpose = Bip32Indexes.HardenedIndex(Purpose) // following CIP-1852
      val coinType = Bip32Indexes.HardenedIndex(CoinType) // Topl coin type registered with SLIP-0044
      for {
        extendedEd25519Instance <- extendedEd25519Resource
        res <- extendedEd25519Instance.use(instance =>
          Monad[F].pure(
            instance.deriveKeyPairFromChildPath(
              extendedEd25519Initializer(instance).fromEntropy(entropy, passphrase),
              List(purpose, coinType)
            )
          )
        )
      } yield res
    }
  }

  case class NewWalletResult[F[_]](mnemonic: IndexedSeq[String], mainKeyVaultStore: VaultStore[F])

  abstract class WalletApiFailure(err: Throwable = null) extends RuntimeException(err)
  case class FailedToInitializeWallet(err: Throwable = null) extends WalletApiFailure(err)
  case class FailedToSaveWallet(err: Throwable = null) extends WalletApiFailure(err)
  case class FailedToSaveMnemonic(err: Throwable = null) extends WalletApiFailure(err)
  case class FailedToLoadWallet(err: Throwable = null) extends WalletApiFailure(err)
  case class FailedToUpdateWallet(err: Throwable = null) extends WalletApiFailure(err)
  case class FailedToDeleteWallet(err: Throwable = null) extends WalletApiFailure(err)
  case class FailedToDecodeWallet(err: Throwable = null) extends WalletApiFailure(err)
}
