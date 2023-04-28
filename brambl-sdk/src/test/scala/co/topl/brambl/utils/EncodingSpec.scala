package co.topl.brambl.utils

class EncodingSpec extends munit.FunSuite {

  test("Main Network Main Ledger Zero Test") {
    assertEquals(
      Encoding.encodeToBase58Check(
        Encoding.decodeFromHex("8A11054CE7B07A00" + Array.fill(64)("0").mkString).toOption.get
      ),
      "mtetmain1y1Rqvj9PiHrsoF4VRHKscLPArgdWe44ogoiKoxwfevERNVgxLLh"
    )
  }

  test("Valhalla Network Main Ledger Zero Test") {
    assertEquals(
      Encoding.encodeToBase58Check(
        Encoding.decodeFromHex("A5BF4108E7B07A00" + Array.fill(64)("0").mkString).toOption.get
      ),
      "vtetDGydU3EhwSbcRVFiuHmyP37Y57BwpmmutR7ZPYdD8BYssHEj3FRhr2Y8"
    )
  }

  test("Private Network Main Ledger Zero Test") {
    assertEquals(
      Encoding.encodeToBase58Check(
        Encoding.decodeFromHex("934B1900E7B07A00" + Array.fill(64)("0").mkString).toOption.get
      ),
      "ptetP7jshHTuV9bmPmtVLm6PtUzBMZ8iYRvAxvbGTJ5VgiEPHqCCnZ8MLLdi"
    )
  }

  test("Main Network Main Ledger All One Test") {
    assertEquals(
      Encoding.encodeToBase58Check(
        Encoding.decodeFromHex("8A11054CE7B07A00" + Array.fill(64)("F").mkString).toOption.get
      ),
      "mtetmain1y3Nb6xbRZiY6w4eCKrwsZeywmoFEHkugUSnS47dZeaEos36pZwb"
    )
  }

  test("Main Network Main Ledger All One Test") {
    assertEquals(
      Encoding.encodeToBase58Check(
        Encoding.decodeFromHex("8A11054CE7B07A00" + Array.fill(64)("F").mkString).toOption.get
      ),
      "mtetmain1y3Nb6xbRZiY6w4eCKrwsZeywmoFEHkugUSnS47dZeaEos36pZwb"
    )
  }

  test("Valhalla Network Main Ledger All One Test") {
    assertEquals(
      Encoding.encodeToBase58Check(
        Encoding.decodeFromHex("A5BF4108E7B07A00" + Array.fill(64)("F").mkString).toOption.get
      ),
      "vtetDGydU3Gegcq4TLgQ8RbZ5whA54WYbgtXc4pQGLGHERhZmGtjRjwruMj7"
    )
  }

  test("Private Network Main Ledger All One Test") {
    assertEquals(
      Encoding.encodeToBase58Check(
        Encoding.decodeFromHex("934B1900E7B07A00" + Array.fill(64)("F").mkString).toOption.get
      ),
      "ptetP7jshHVrEKqDRdKAZtuybPZoMWTKKM2ngaJ7L5iZnxP5BprDB3hGJEFr"
    )
  }

  test("Encode decode") {
    assertEquals(
      new String(Encoding.decodeFromBase58(Encoding.encodeToBase58("Hello World!".getBytes())).toOption.get),
      "Hello World!"
    )
  }

}
