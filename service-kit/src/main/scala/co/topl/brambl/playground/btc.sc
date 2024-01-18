import co.topl.brambl.playground._
import co.topl.brambl.utils.Encoding
import play.api.libs.json._

import java.security.MessageDigest

//val desc = handleCall(rpcCli.listDescriptors("testwallet")).get
//desc.map(Json.prettyPrint).foreach(println)
//setup()
handleCall(rpcCli.loadWallet("alice"))
handleCall(rpcCli.loadWallet("bridge"))

val alicePubKey = handleCall(rpcCli.getNewPublicKey("alice")).get

val secret = "topl-secret".getBytes("UTF-8")
val hash = MessageDigest.getInstance("SHA-256").digest(secret)
val secretHex = Encoding.encodeToHex(secret)
val hashHex = Encoding.encodeToHex(hash)

val descriptor = s"wsh(and_v(v:pk($alicePubKey),sha256($hashHex)))"
