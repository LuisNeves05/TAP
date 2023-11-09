package pj.domain.types

import pj.domain.Result
import pj.domain.DomainError.IllegalIdentifier

opaque type Identifier = String

object Identifier {
  def from(id: String): Result[Identifier] =
    if (id.nonEmpty) Right(id) else Left(IllegalIdentifier("Identifier cannot be empty"))

  extension (m: Identifier)
    def identifierToString: String = m
}