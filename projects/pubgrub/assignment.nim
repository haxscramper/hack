import
  ./incompatibility,
  ./term,
  ./types


proc isDecision(self: Assignment): bool = self.cause.isNil()
proc decisition(package: PackageId, decisionLevel, index: int): Assignment =
  result = Assignment(
    decisionLevel: decisionLevel,
    index: index
  )

  Term(result)[] = newTerm(package.toRange(), true)[]

proc derivation(
    package: PackageId, isPositive: bool,
    decisionLevel, index: int): Assignment =

  result = Assignment(
    decisionLevel: decisionLevel,
    index: index
  )
