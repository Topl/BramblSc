package co.topl.brambl.wallet

import cats.arrow.FunctionK
import cats.implicits.catsSyntaxTuple8Parallel
import co.topl.brambl.models.Indices
import co.topl.brambl.{MockHelpers, MockWalletKeyApi}

class ExtendedEd25519ResourceSpec extends munit.CatsEffectSuite with MockHelpers {
  implicit val idToId: FunctionK[F, F] = FunctionK.id[F]

  test("Race condition") {
    val NumRuns = 1000000
    // a single WalletApi instance to call diff methods in parallel
    val walletApi = WalletApi.make[F](MockWalletKeyApi)

    // testing multiple functions in parallel.
    def runParallel(idx: Int): F[Unit] = (
      walletApi.deriveChildKeys(MockMainKeyPair, Indices(0, 0, 0)),
      walletApi.deriveChildKeys(MockMainKeyPair, Indices(idx, idx, idx)),
      walletApi.deriveChildKeysPartial(MockMainKeyPair, 0, 0),
      walletApi.deriveChildKeysPartial(MockMainKeyPair, idx, idx),
      walletApi.deriveChildVerificationKey(MockMainKeyPair.vk, 0),
      walletApi.deriveChildVerificationKey(MockMainKeyPair.vk, idx),
      walletApi.createNewWallet("password".getBytes),
      walletApi.createNewWallet("password".getBytes),
    ).parTupled.map(_ => ()) // the results do no matter. We are testing if an exception occurs

    0 to NumRuns foreach { idx =>
      assertIO_(runParallel(idx))
    }
  }

}
