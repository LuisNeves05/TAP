package pj.domain

type Result[A] = Either[DomainError,A]

enum DomainError:
  case IOFileProblem(error: String)
  case XMLError(error: String)
  case IllegalIdentifier(error: String)
  case IllegalNonNegativeInteger(error: String)
  case IllegalPositiveInteger(error: String)
  case IllegalClassNumber(error: String)
  case RepeatedAircraftId(error: String)
  case RepeatedRunwayId(error: String)
  case NoRunwaysAvailable(error: String)
  case NoEmergencyAvailability(error: String)
  case MaximumTimeWindowExceeded(error: String)
  case UnknownError(error: String)
  
