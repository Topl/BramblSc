package co.topl.crypto.encryption

import io.circe.Json
import io.circe.syntax._

case class KeyFile[F[_], KdfParams <: kdf.Params[F], CipherParams <: cipher.Params[F]](
  kdfParams:    KdfParams,
  cipherParams: CipherParams,
  cipherText:   Array[Byte],
  mac:          Array[Byte]
) {

  def asJson: Json = Json.obj(
    "kdf"        -> kdfParams.asJson,
    "cipher"     -> cipherParams.asJson,
    "cipherText" -> cipherText.asJson,
    "mac"        -> mac.asJson
  )
}

object KeyFile {
  def fromJson[F[_]](json: Json): KeyFile[F, kdf.Params[F], cipher.Params[F]] = ???
}
