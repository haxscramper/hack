import
  ./package_name,
  ./types

import hmisc/base_errors

# import 'set_relation.dart'

{.this: this.}

proc getInverse*(this: Term): Term =
  ## A copy of this term with the opposite [isPositive] value.
  Term(package: package, isPositive: not isPositive)

proc newTerm*(package: PackageRange, isPositive: bool): Term =
  Term(package: package.withTerseConstraint(), isPositive: isPositive)

proc constraint*(this: Term): VersionConstraint =
  package.constraint

proc relation*(this, other: Term): SetRelation


proc satisfies*(this, other: Term): bool =
  ## Returns whether [this] satisfies [other].
  ##
  ## That is, whether [this] being true means that [other] must also be true.
  this.package.name == other.package.name and
  this.relation(other) == SetRelation.subset

proc compatiblePackage(this, other: PackageRange): bool =
  ## Returns whether [other] is compatible with [package].
  package.isRoot or other.isRoot or other.samePackage(package);


proc relation*(this, other: Term): SetRelation =
  ## Returns the relationship between the package versions allowed by [this]
  ## and by [other].
  ##
  ## Throws an [ArgumentError] if [other] doesn't refer to a package with the
  ## same name as [package].
  if package.name != other.package.name:
    raise newArgumentError("{other} should refer to package ${package.name}")

  var otherConstraint = other.constraint
  if (other.isPositive):
    if (isPositive):
      # foo from hosted is disjoint with foo from git
      if (not compatiblePackage(other.package)): return SetRelation.disjoint

      # foo ^1.5.0 is a subset of foo ^1.0.0
      if (otherConstraint.allowsAll(constraint)): return SetRelation.subset

      # foo ^2.0.0 is disjoint with foo ^1.0.0
      if (not constraint.allowsAny(otherConstraint)): return SetRelation.disjoint

      # foo >=1.5.0 <3.0.0 overlaps foo ^1.0.0
      return SetRelation.overlapping

    else:
      # not foo from hosted is a superset foo from git
      if (!_compatiblePackage(other.package)): return SetRelation.overlapping

      # not foo ^1.0.0 is disjoint with foo ^1.5.0
      if (constraint.allowsAll(otherConstraint)): return SetRelation.disjoint

      # not foo ^1.5.0 overlaps foo ^1.0.0
      # not foo ^2.0.0 is a superset of foo ^1.5.0
      return SetRelation.overlapping

  else:
    if (isPositive):
      # foo from hosted is a subset of not foo from git
      if (not compatiblePackage(other.package)): return SetRelation.subset

      # foo ^2.0.0 is a subset of not foo ^1.0.0
      if (not otherConstraint.allowsAny(constraint)): return SetRelation.subset

      # foo ^1.5.0 is disjoint with not foo ^1.0.0
      if (otherConstraint.allowsAll(constraint)): return SetRelation.disjoint

      # foo ^1.0.0 overlaps not foo ^1.5.0
      return SetRelation.overlapping

    else:
      # not foo from hosted overlaps not foo from git
      if (not compatiblePackage(other.package)): return SetRelation.overlapping

      # not foo ^1.0.0 is a subset of not foo ^1.5.0
      if (constraint.allowsAll(otherConstraint)): return SetRelation.subset

      # not foo ^2.0.0 overlaps not foo ^1.0.0
      # not foo ^1.5.0 is a superset of not foo ^1.0.0
      return SetRelation.overlapping


proc intersect*(Term other): Term =
  ## Returns a [Term] that represents the packages allowed by both [this] and
  ## [other].
  ##
  ## If there is no such single [Term], for example because [this] is
  ## incompatible with [other], returns `null`.
  ##
  ## Throws an [ArgumentError] if [other] doesn't refer to a package with the
  ## same name as [package].

  if (package.name != other.package.name):
    throw ArgumentError.value(
        other, 'other', 'should refer to package ${package.name}')

  if (_compatiblePackage(other.package)):
    if (isPositive != other.isPositive):
      # foo ^1.0.0 ∩ not foo ^1.5.0 → foo >=1.0.0 <1.5.0
      var positive = isPositive ? this : other
      var negative = isPositive ? other : this
      return _nonEmptyTerm(
          positive.constraint.difference(negative.constraint), true)

    else if (isPositive):
      # foo ^1.0.0 ∩ foo >=1.5.0 <3.0.0 → foo ^1.5.0
      return nonEmptyTerm(constraint.intersect(other.constraint), true)

    else:
      # not foo ^1.0.0 ∩ not foo >=1.5.0 <3.0.0 → not foo >=1.0.0 <3.0.0
      return nonEmptyTerm(constraint.union(other.constraint), false)

  else if (isPositive != other.isPositive):
    # foo from git ∩ not foo from hosted → foo from git
    return if isPositive: this else: other

  else:
    #     foo from git ∩     foo from hosted → empty
    # not foo from git ∩ not foo from hosted → no single term
    return null

proc difference*(this, other: Term): Term =
  ## Returns a [Term] that represents packages allowed by [this] and not by
  ## [other].
  ##
  ## If there is no such single [Term], for example because all packages
  ## allowed by [this] are allowed by [other], returns `null`.
  ##
  ## Throws an [ArgumentError] if [other] doesn't refer to a package with the
  intersect(other.inverse) # A ∖ B → A ∩ not B


proc nonEmptyTerm(constraint: VersionConstraint, isPositive: bool): Term =
  # Returns a new [Term] with the same package as [this] and with
  # [constraint], unless that would produce a term that allows no packages,
  # in which case this returns `null`
  if constraint.isEmpty:
    null

  else:
    newTerm(package.withConstraint(constraint), isPositive)

proc `$`(thsi: Term): string = "${isPositive ? '' : 'not '}$package"
