package co.topl.brambl.codecs

import co.topl.brambl.models.LockAddress
import co.topl.brambl.constants.NetworkConstants
import co.topl.brambl.models.LockId
import com.google.protobuf.ByteString

trait AddressCodecTestCases {

  val testMainLockZeroLockAddress = LockAddress(
    NetworkConstants.MAIN_NETWORK_ID,
    NetworkConstants.MAIN_LEDGER_ID,
    LockId(
      ByteString.copyFrom(Array.fill(32)(0.toByte))
    )
  )

  val testMainLockZeroLockAddressEncoded = "mtetmain1y1Rqvj9PiHrsoF4VRHKscLPArgdWe44ogoiKoxwfevERNVgxLLh"

  val testTestLockZeroLockAddress = LockAddress(
    NetworkConstants.TEST_NETWORK_ID,
    NetworkConstants.MAIN_LEDGER_ID,
    LockId(
      ByteString.copyFrom(Array.fill(32)(0.toByte))
    )
  )

  val testTestLockZeroLockAddressEncoded = "vtetDGydU3EhwSbcRVFiuHmyP37Y57BwpmmutR7ZPYdD8BYssHEj3FRhr2Y8"

  val testPrivateLockZeroLockAddress = LockAddress(
    NetworkConstants.PRIVATE_NETWORK_ID,
    NetworkConstants.MAIN_LEDGER_ID,
    LockId(
      ByteString.copyFrom(Array.fill(32)(0.toByte))
    )
  )

  val testPrivateLockZeroLockAddressEncoded = "ptetP7jshHTuV9bmPmtVLm6PtUzBMZ8iYRvAxvbGTJ5VgiEPHqCCnZ8MLLdi"

  val testMainLockAllOneLockAddress = LockAddress(
    NetworkConstants.MAIN_NETWORK_ID,
    NetworkConstants.MAIN_LEDGER_ID,
    LockId(
      ByteString.copyFrom(Array.fill(32)(255.toByte))
    )
  )

  val testMainLockAllOneLockAddressEncoded = "mtetmain1y3Nb6xbRZiY6w4eCKrwsZeywmoFEHkugUSnS47dZeaEos36pZwb"

  val testTestLockAllOneLockAddress = LockAddress(
    NetworkConstants.TEST_NETWORK_ID,
    NetworkConstants.MAIN_LEDGER_ID,
    LockId(
      ByteString.copyFrom(Array.fill(32)(255.toByte))
    )
  )

  val testTestLockAllOneLockAddressEncoded = "vtetDGydU3Gegcq4TLgQ8RbZ5whA54WYbgtXc4pQGLGHERhZmGtjRjwruMj7"

  val testPrivateLockAllOneLockAddress = LockAddress(
    NetworkConstants.PRIVATE_NETWORK_ID,
    NetworkConstants.MAIN_LEDGER_ID,
    LockId(
      ByteString.copyFrom(Array.fill(32)(255.toByte))
    )
  )

  val testPrivateLockAllOneLockAddressEncoded = "ptetP7jshHVrEKqDRdKAZtuybPZoMWTKKM2ngaJ7L5iZnxP5BprDB3hGJEFr"

}
