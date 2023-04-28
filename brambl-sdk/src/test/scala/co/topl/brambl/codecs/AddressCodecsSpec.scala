package co.topl.brambl.codecs

import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.LockId
import com.google.protobuf.ByteString
import co.topl.brambl.utils.Encoding
import co.topl.brambl.constants.NetworkConstants

class AddressCodecsSpec extends munit.FunSuite {

  test("Main Network Main Ledger Zero Test") {
    assertEquals(
      AddressCodecs.encodeAddress(
        LockAddress(
          NetworkConstants.MAIN_NETWORK_ID,
          NetworkConstants.MAIN_LEDGER_ID,
          LockId(
            ByteString.copyFrom(Array.fill(32)(0.toByte))
          )
        )
      ),
      "mtetmain1y1Rqvj9PiHrsoF4VRHKscLPArgdWe44ogoiKoxwfevERNVgxLLh"
    )
  }

  test("Valhalla Network Main Ledger Zero Test") {
    assertEquals(
      AddressCodecs.encodeAddress(
        LockAddress(
          NetworkConstants.TEST_NETWORK_ID,
          NetworkConstants.MAIN_LEDGER_ID,
          LockId(
            ByteString.copyFrom(Array.fill(32)(0.toByte))
          )
        )
      ),
      "vtetDGydU3EhwSbcRVFiuHmyP37Y57BwpmmutR7ZPYdD8BYssHEj3FRhr2Y8"
    )
  }

  test("Private Network Main Ledger Zero Test") {
    assertEquals(
      AddressCodecs.encodeAddress(
        LockAddress(
          NetworkConstants.PRIVATE_NETWORK_ID,
          NetworkConstants.MAIN_LEDGER_ID,
          LockId(
            ByteString.copyFrom(Array.fill(32)(0.toByte))
          )
        )
      ),
      "ptetP7jshHTuV9bmPmtVLm6PtUzBMZ8iYRvAxvbGTJ5VgiEPHqCCnZ8MLLdi"
    )
  }

  test("Main Network Main Ledger All One Test") {
    assertEquals(
      AddressCodecs.encodeAddress(
        LockAddress(
          NetworkConstants.MAIN_NETWORK_ID,
          NetworkConstants.MAIN_LEDGER_ID,
          LockId(
            ByteString.copyFrom(Array.fill(32)(255.toByte))
          )
        )
      ),
      "mtetmain1y3Nb6xbRZiY6w4eCKrwsZeywmoFEHkugUSnS47dZeaEos36pZwb"
    )
  }

  test("Valhalla Network Main Ledger All One Test") {
    assertEquals(
      AddressCodecs.encodeAddress(
        LockAddress(
          NetworkConstants.TEST_NETWORK_ID,
          NetworkConstants.MAIN_LEDGER_ID,
          LockId(
            ByteString.copyFrom(Array.fill(32)(255.toByte))
          )
        )
      ),
      "vtetDGydU3Gegcq4TLgQ8RbZ5whA54WYbgtXc4pQGLGHERhZmGtjRjwruMj7"
    )
  }

  test("Private Network Main Ledger All One Test") {
    assertEquals(
      AddressCodecs.encodeAddress(
        LockAddress(
          NetworkConstants.PRIVATE_NETWORK_ID,
          NetworkConstants.MAIN_LEDGER_ID,
          LockId(
            ByteString.copyFrom(Array.fill(32)(255.toByte))
          )
        )
      ),
      "ptetP7jshHVrEKqDRdKAZtuybPZoMWTKKM2ngaJ7L5iZnxP5BprDB3hGJEFr"
    )
  }

}
