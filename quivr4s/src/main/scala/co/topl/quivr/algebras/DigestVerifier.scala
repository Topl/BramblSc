package co.topl.quivr.algebras

import co.topl.common.ContextlessValidation
import co.topl.quivr.runtime.QuivrRuntimeError
import quivr.models.DigestVerification

/** A trait that provides Digest verification for use in a Dynamic Context */
trait DigestVerifier[F[_]] extends ContextlessValidation[F, QuivrRuntimeError, DigestVerification]
