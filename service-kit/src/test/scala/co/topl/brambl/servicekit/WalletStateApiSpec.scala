package co.topl.brambl.servicekit

import cats.effect.IO
import co.topl.brambl.builders.TransactionBuilderApi.implicits.lockAddressOps
import co.topl.brambl.builders.locks.{LockTemplate, PropositionTemplate}
import co.topl.brambl.common.ContainsEvidence.Ops
import co.topl.brambl.common.ContainsImmutable.instances.lockImmutable
import co.topl.brambl.models.{Indices, LockAddress, LockId}
import co.topl.brambl.models.box.Lock
import co.topl.brambl.utils.Encoding
import com.google.protobuf.ByteString
import munit.CatsEffectSuite
import quivr.models.Digest
import quivr.models.{Proposition, VerificationKey}
import co.topl.brambl.constants.NetworkConstants

class WalletStateApiSpec extends CatsEffectSuite with BaseSpec {

  testDirectory.test("initWalletState") { _ =>
    assertIO(
      for {
        init <- walletStateApi.initWalletState(
          NetworkConstants.PRIVATE_NETWORK_ID,
          NetworkConstants.MAIN_NETWORK_ID,
          mockMainKeyPair.vk
        )
        partyCount <- dbConnection.use { conn =>
          for {
            stmt <- IO.delay(conn.createStatement())
            rs <- IO.blocking(
              stmt.executeQuery("SELECT COUNT(*) as res FROM parties WHERE party IN ('noparty', 'self')")
            )
            count <- IO.delay(rs.getInt("res"))
          } yield count
        }
        contractCount <- dbConnection.use { conn =>
          for {
            stmt <- IO.delay(conn.createStatement())
            rs <- IO.blocking(
              stmt.executeQuery("SELECT COUNT(*) as res FROM contracts WHERE contract IN ('default', 'genesis')")
            )
            count <- IO.delay(rs.getInt("res"))
          } yield count
        }
        vkCount <- dbConnection.use { conn =>
          for {
            stmt  <- IO.delay(conn.createStatement())
            rs    <- IO.blocking(stmt.executeQuery("SELECT COUNT(*) as res FROM verification_keys"))
            count <- IO.delay(rs.getInt("res"))
          } yield count
        }
        cartesianCount <- dbConnection.use { conn =>
          for {
            stmt  <- IO.delay(conn.createStatement())
            rs    <- IO.blocking(stmt.executeQuery("SELECT COUNT(*) as res FROM cartesian"))
            count <- IO.delay(rs.getInt("res"))
          } yield count
        }
      } yield partyCount == 2 && contractCount == 2 && vkCount == 2 && cartesianCount == 2,
      true
    )
  }

  testDirectory.test("updateWalletState") { _ =>
    val testValue = "testValue"
    assertIO(
      for {
        init <- walletStateApi.initWalletState(
          NetworkConstants.PRIVATE_NETWORK_ID,
          NetworkConstants.MAIN_NETWORK_ID,
          mockMainKeyPair.vk
        )
        update <- walletStateApi.updateWalletState(
          testValue,
          testValue,
          Some(testValue),
          Some(testValue),
          Indices(9, 9, 9)
        )
        count <- dbConnection.use { conn =>
          for {
            stmt <- IO.delay(conn.createStatement())
            rs <- IO.blocking(
              stmt.executeQuery("SELECT COUNT(*) as res FROM cartesian")
            )
            count <- IO.delay(rs.getInt("res"))
          } yield count
        }
        rowValid <- dbConnection.use { conn =>
          for {
            stmt <- IO.delay(conn.createStatement())
            rs <- IO.blocking(
              stmt.executeQuery("SELECT * FROM cartesian WHERE x_party = 9 AND y_contract = 9 AND z_state = 9")
            )
            predicate <- IO.delay(rs.getString("lock_predicate"))
            address   <- IO.delay(rs.getString("address"))
            routine   <- IO.delay(rs.getString("routine"))
            vk        <- IO.delay(rs.getString("vk"))
          } yield predicate.equals(testValue) && address.equals(testValue) && routine.equals(testValue) && vk.equals(
            testValue
          )
        }
      } yield count == 3 && rowValid,
      true
    )
  }

  testDirectory.test("getIndicesBySignature") { _ =>
    val testValue = "testValue"
    val idx = Indices(9, 9, 9)
    val proposition = Proposition.DigitalSignature(testValue, mockMainKeyPair.vk)
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        update <- walletStateApi.updateWalletState(
          testValue,
          testValue,
          Some(testValue),
          Some(Encoding.encodeToBase58(proposition.verificationKey.toByteArray)),
          idx
        )
        indices <- walletStateApi.getIndicesBySignature(proposition)
      } yield indices.isDefined && indices.get == idx,
      true
    )
  }

  testDirectory.test("getLockByIndex") { _ =>
    val testValue = "testValue"
    val idx = Indices(9, 9, 9)
    val predicate = Lock.Predicate(Seq(), 1)
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        update <- walletStateApi.updateWalletState(
          Encoding.encodeToBase58Check(predicate.toByteArray),
          testValue,
          None,
          None,
          idx
        )
        lock <- walletStateApi.getLockByIndex(idx)
      } yield lock.isDefined && lock.get == predicate,
      true
    )
  }

  testDirectory.test("getLockByAddress") { _ =>
    val predicate = Lock.Predicate(Seq(), 1)
    val lockAddress = LockAddress(0, 0, LockId(Lock().withPredicate(predicate).sizedEvidence.digest.value))
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        update <- walletStateApi.updateWalletState(
          Encoding.encodeToBase58Check(predicate.toByteArray),
          lockAddress.toBase58(),
          None,
          None,
          Indices(9, 9, 9)
        )
        lock <- walletStateApi.getLockByAddress(lockAddress.toBase58())
      } yield lock.isDefined && lock.get == predicate,
      true
    )
  }

  testDirectory.test("getLockByAddress > LockAddress not known in Wallet State") { _ =>
    val predicate = Lock.Predicate(Seq(), 1)
    val lockAddress = LockAddress(0, 0, LockId(Lock().withPredicate(predicate).sizedEvidence.digest.value))
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        lock <- walletStateApi.getLockByAddress(lockAddress.toBase58())
      } yield lock.isEmpty,
      true
    )
  }

  testDirectory.test("getNextIndicesForFunds") { _ =>
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        idx <- walletStateApi.getNextIndicesForFunds("self", "default")
      } yield idx.isDefined && idx.get == Indices(1, 1, 2),
      true
    )
  }

  testDirectory.test("validateCurrentIndicesForFunds") { _ =>
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        idx <- walletStateApi.validateCurrentIndicesForFunds("self", "default", None)
      } yield idx.isValid && idx.toOption.get == Indices(1, 1, 1),
      true
    )
  }

  testDirectory.test("getAddress") { _ =>
    val testValue = "testValue"
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        update <- walletStateApi.updateWalletState(
          testValue,
          testValue,
          Some(testValue),
          Some(testValue),
          Indices(1, 1, 2)
        )
        addr <- walletStateApi.getAddress("self", "default", None)
      } yield addr.isDefined && addr.get.equals(testValue),
      true
    )
  }

  testDirectory.test("getCurrentIndicesForFunds") { _ =>
    val testValue = "testValue"
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        idx <- walletStateApi.getCurrentIndicesForFunds("self", "default", None)
      } yield idx.isDefined && idx.get == Indices(1, 1, 1),
      true
    )
  }

  testDirectory.test("getCurrentAddress") { _ =>
    val testValue = "testValue"
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        update <- walletStateApi.updateWalletState(
          testValue,
          testValue,
          Some(testValue),
          Some(testValue),
          Indices(1, 1, 2)
        )
        addr <- walletStateApi.getCurrentAddress
      } yield addr.equals(testValue),
      true
    )
  }

  testDirectory.test("getPreimage") { _ =>
    val proposition = Proposition.Digest("testValue", Digest(ByteString.copyFrom(Array.fill(32)(0: Byte))))
    assertIO(
      for {
        preimage <- walletStateApi.getPreimage(proposition)
      } yield preimage.isEmpty,
      true
    )
  }

  testDirectory.test("addEntityVks then getEntityVks") { _ =>
    val testValues = List("testValue1", "testValue2")
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        _   <- walletStateApi.addEntityVks("test", "default", testValues)
        vks <- walletStateApi.getEntityVks("test", "default")
      } yield vks.isDefined && vks.get == testValues,
      true
    )
  }

  testDirectory.test("addNewLockTemplate then getLockTemplate") { _ =>
    val lockTemplate: LockTemplate[IO] =
      LockTemplate.PredicateTemplate[IO](List(PropositionTemplate.HeightTemplate[IO]("chain", 0, 100)), 1)
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        _        <- walletStateApi.addNewLockTemplate("height", lockTemplate)
        template <- walletStateApi.getLockTemplate("height")
      } yield template.isDefined && template.get == lockTemplate,
      true
    )
  }

  testDirectory.test("getLock") { _ =>
    val lockTemplate: LockTemplate[IO] =
      LockTemplate.PredicateTemplate[IO](List(PropositionTemplate.SignatureTemplate[IO]("routine", 0)), 1)
    val entityVks = List(mockMainKeyPair.vk)
    assertIO(
      for {
        init <- walletStateApi
          .initWalletState(NetworkConstants.PRIVATE_NETWORK_ID, NetworkConstants.MAIN_NETWORK_ID, mockMainKeyPair.vk)
        _ <- walletStateApi.addNewLockTemplate("test", lockTemplate)
        _ <- walletStateApi.addEntityVks("self", "test", entityVks.map(vk => Encoding.encodeToBase58(vk.toByteArray)))
        lock <- walletStateApi.getLock("self", "test", 2)
      } yield lock.isDefined && lock.get.getPredicate.challenges.head.getRevealed.value
        .isInstanceOf[Proposition.Value.DigitalSignature],
      true
    )
  }

}
