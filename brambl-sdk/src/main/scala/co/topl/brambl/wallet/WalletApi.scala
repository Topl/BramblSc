package co.topl.brambl.wallet

import co.topl.crypto.generation.mnemonic.{EntropyFailure, MnemonicSize, MnemonicSizes}
import cats.{Applicative, Monad}
import cats.implicits.{catsSyntaxApplicativeId, toFlatMapOps, toFunctorOps}
import co.topl.brambl.dataApi.DataApi
import co.topl.crypto.generation.{Bip32Index, Bip32Indexes, KeyInitializer}
import co.topl.crypto.generation.mnemonic.{Entropy, EntropyFailure, MnemonicSize, MnemonicSizes}
import KeyInitializer.Instances.extendedEd25519Initializer
import co.topl.crypto.encryption.{Mac, VaultStore}
import co.topl.crypto.encryption.kdf.Kdf
import co.topl.crypto.encryption.kdf.SCrypt
import co.topl.crypto.encryption.cipher.Cipher
import co.topl.crypto.encryption.cipher.Aes
import co.topl.crypto.signing.ExtendedEd25519

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
   * @return The mnemonic of the newly created wallet
   */
  def createNewWallet(
    password:   Array[Byte],
    passphrase: Option[String],
    mLen:       MnemonicSize = MnemonicSizes.words12
  ): F[Either[EntropyFailure, String]]

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

    override def createNewWallet(
      password:   Array[Byte],
      passphrase: Option[String],
      mLen:       MnemonicSize = MnemonicSizes.words12
    ): F[Either[EntropyFailure, String]] = {
      val entropy = Entropy.generate(mLen)
      val mainKey: Array[Byte] = ((k: ExtendedEd25519.SecretKey) => k.leftKey ++ k.rightKey ++ k.chainCode)(
        entropyToMainSecretKey(entropy, passphrase)
      )

      val sCrypt = SCrypt.make[F](SCrypt.SCryptParams(SCrypt.generateSalt))
      val aes = Aes.make[F](Aes.AesParams(Aes.generateIv))

      for {
        derivedKey: Array[Byte] <- sCrypt.deriveKey(password)
        cipherText: Array[Byte] <- aes.encrypt(mainKey, derivedKey)
        mac: Array[Byte] = Mac.make(derivedKey, cipherText).value
      } yield VaultStore[F](sCrypt, aes, cipherText, mac)

      // TODO: Store VaultStore in DataApi

      Entropy.toMnemonicString(entropy).pure[F]
    }

    private def entropyToMainSecretKey(entropy: Entropy, passphrase: Option[String]): ExtendedEd25519.SecretKey = {
      val rootKey: ExtendedEd25519.SecretKey = extendedEd25519Initializer.fromEntropy(entropy, passphrase)
      val purpose: Bip32Index = Bip32Indexes.HardenedIndex(Purpose) // following CIP-1852
      val coinType: Bip32Index = Bip32Indexes.HardenedIndex(CoinType) // Topl coin type registered with SLIP-0044
      extendedEd25519Instance.deriveKeyPairFromChildPath(rootKey, List(purpose, coinType)).signingKey
    }
  }
}
