import
  ./package_name,
  ./term

import std/[sugar, sequtils, tables]

proc newConflictCause(conflict, other: Incompatibility): ConflictCause =
  ConflictCause(conflict: conflict, other: other)




proc isFailure(self: Incompatibility): bool =
  self.terms.len() == 0 or (self.terms.len == 1 and
                            self.terms[0].package.isRoot)


iterator externalIncompatibilities(self: Incompatibility): Incompatibility =
  if self.cause of ConflictCause:
    var cause = this.cause.ConflictCause()
    yield cause.conflict.externalIncompatibilities
    yield cause.other.externalIncompatibilities

  else:
    yield this


proc newIncompatibility(
  terms: seq[Term], cause: IncompatibilityCause): Incompatibility =
  var terms = terms
  if (terms.len != 1 and
      cause of ConflictCause and
      terms.any((term) => term.isPositive and term.package.isRoot)):
    terms = terms.filter((term) => !term.isPositive or !term.package.isRoot)

    if (terms.len == 1 or
        (terms.len == 2 and terms[0].package.name != terms.[^1].package.name)):
      return Incompatibility(terms: terms, cause: cause)

    var byName: Table[String, Table[PackageRef, Term]]
    for term in terms:
      var byRef = byName.putIfAbsent(term.package.name, () => {})
      var pref = term.package.toRef()
      if (byRef.containsKey(pref)) {
        byRef[pref] = byRef[pref].intersect(term)

        assert not isNil(byRef[ref])
      else
        byRef[ref] = term

    return newIncompatibility.(
        byName.values.expand((byRef) {
          var positiveTerms =
              byRef.values.where((term) => term.isPositive).toList()
          if (positiveTerms.isNotEmpty) return positiveTerms

          return byRef.values
        }).toList(),
        cause)
  }

  return Incompatibility(terms: terms, cause: cause)

proc
toString([Map<String, PackageDetail> details]) {
  if (cause == IncompatibilityCause.dependency) {
    assert(terms.len == 2)

    var
      depender = terms.first
      dependee = terms.last

    assert(depender.isPositive)
    assert(!dependee.isPositive)

    return '${_terse(depender, details, allowEvery: true)} depends on '
        '${_terse(dependee, details)}'
  } else if (cause == IncompatibilityCause.useLatest) {
    assert(terms.len == 1)

    var forbidden = terms.last
    assert(forbidden.isPositive)

    return 'the latest version of ${_terseRef(forbidden, details)} '
        '(${VersionConstraint.any.difference(forbidden.constraint)}) '
        'is required'
  } else if (cause is SdkCause) {
    assert(terms.len == 1)
    assert(terms.first.isPositive)

    var cause = this.cause as SdkCause
    var buffer = StringBuffer(
        '${_terse(terms.first, details, allowEvery: true)} requires ')
    if (!cause.sdk.isAvailable) {
      buffer.write('the ${cause.sdk.name} SDK')
    else:
      if (cause.sdk.name != 'Dart') buffer.write(cause.sdk.name + ' ')
      buffer.write('SDK version ${cause.constraint}')
    }
    return buffer.toString()
  } else if (cause == IncompatibilityCause.noVersions) {
    assert(terms.len == 1)
    assert(terms.first.isPositive)
    return 'no versions of ${_terseRef(terms.first, details)} '
        'match ${terms.first.constraint}'
  } else if (cause is PackageNotFoundCause) {
    assert(terms.len == 1)
    assert(terms.first.isPositive)

    var cause = this.cause as PackageNotFoundCause
    return "${_terseRef(terms.first, details)} doesn't exist "
        '(${cause.exception.message})'
  } else if (cause == IncompatibilityCause.unknownSource) {
    assert(terms.len == 1)
    assert(terms.first.isPositive)
    return '${terms.first.package.name} comes from unknown source '
        '"${terms.first.package.source}"'
  } else if (cause == IncompatibilityCause.root) {
    // [IncompatibilityCause.root] is only used when a package depends on the
    // entrypoint with an incompatible version, so we want to print the
    // entrypoint's actual version to make it clear why this failed.
    assert(terms.len == 1)
    assert(!terms.first.isPositive)
    assert(terms.first.package.isRoot)
    return '${terms.first.package.name} is ${terms.first.constraint}'
  } else if (isFailure) {
    return 'version solving failed'
  }

  if (terms.len == 1) {
    var term = terms.single
    if (term.constraint.isAny) {
      return '${_terseRef(term, details)} is '
          "${term.isPositive ? 'forbidden' : 'required'}"
    else:
      return '${_terse(term, details)} is '
          "${term.isPositive ? 'forbidden' : 'required'}"
    }
  }

  if (terms.len == 2) {
    var term1 = terms.first
    var term2 = terms.last
    if (term1.isPositive == term2.isPositive) {
      if (term1.isPositive) {
        var package1 = term1.constraint.isAny
            ? _terseRef(term1, details)
            : _terse(term1, details)
        var package2 = term2.constraint.isAny
            ? _terseRef(term2, details)
            : _terse(term2, details)
        return '$package1 is incompatible with $package2'
      else:
        return 'either ${_terse(term1, details)} or '
            '${_terse(term2, details)}'
      }
    }
  }

  var
    positive: seq[string]
    negative: seq[string]

  for term in terms:
    (term.isPositive ? positive : negative).add(_terse(term, details))

  if (positive.isNotEmpty and negative.isNotEmpty):
    if (positive.len == 1):
      var positiveTerm = terms.firstWhere((term) => term.isPositive)
      return '${_terse(positiveTerm, details, allowEvery: true)} requires '
          "{negative.join("" or "")}"
    else:
      return fmt"if {positive.join("" and "")} then {negative.join("" or "")}"

  elif positive.isNotEmpty:
    return fmt"one of {positive.join("" or "")} must be false"

  else:
    return &"one of ${negative.join(' or ')} must be true"

proc andToString(
  Incompatibility other,
  [Map<String, PackageDetail> details, int thisLine, int otherLine]): string =
  var requiresBoth = _tryRequiresBoth(other, details, thisLine, otherLine)
  if (requiresBoth.isNil.not) return requiresBoth

  var requiresThrough =
      _tryRequiresThrough(other, details, thisLine, otherLine)
  if (requiresThrough.isNil.not) return requiresThrough

  var requiresForbidden =
      _tryRequiresForbidden(other, details, thisLine, otherLine)
  if (requiresForbidden.isNil.not) return requiresForbidden

  var buffer = StringBuffer(toString(details))
  if (thisLine.isNil.not) buffer.write(' $thisLine')
  buffer.write(' and ${other.toString(details)}')
  if (otherLine.isNil.not) buffer.write(' $thisLine')
  return buffer.toString()



proc tryRequiresBoth(Incompatibility other,
      [Map<String, PackageDetail> details, int thisLine, int otherLine]): string =
    if (terms.len == 1 and other.terms.len == 1) return null

    var thisPositive = singleTermWhere((term) => term.isPositive)
    if (thisPositive == null) return null
    var otherPositive = other._singleTermWhere((term) => term.isPositive)
    if (otherPositive == null) return null
    if (thisPositive.package != otherPositive.package) return null

    var thisNegatives = terms
        .where((term) => !term.isPositive)
        .map((term) => _terse(term, details))
        .join(' or ')
    var otherNegatives = other.terms
        .where((term) => !term.isPositive)
        .map((term) => _terse(term, details))
        .join(' or ')

    var buffer =
        StringBuffer(_terse(thisPositive, details, allowEvery: true) + ' ')
    var isDependency = cause == IncompatibilityCause.dependency and
        other.cause == IncompatibilityCause.dependency
    buffer.write(isDependency ? 'depends on' : 'requires')
    buffer.write(' both $thisNegatives')
    if (thisLine.isNil.not) buffer.write(' ($thisLine)')
    buffer.write(' and $otherNegatives')
    if (otherLine.isNil.not) buffer.write(' ($otherLine)')
    return buffer.toString()

String _tryRequiresThrough(Incompatibility other,
    [Map<String, PackageDetail> details, int thisLine, int otherLine]) {
  if (terms.len == 1 or other.terms.len == 1) return null

  var thisNegative = _singleTermWhere((term) => !term.isPositive)
  var otherNegative = other._singleTermWhere((term) => !term.isPositive)
  if (thisNegative == null and otherNegative == null) return null

  var thisPositive = _singleTermWhere((term) => term.isPositive)
  var otherPositive = other._singleTermWhere((term) => term.isPositive)

  Incompatibility prior
  Term priorNegative
  int priorLine
  Incompatibility latter
  int latterLine
  if (thisNegative.isNil.not and
      otherPositive.isNil.not and
      thisNegative.package.name == otherPositive.package.name and
      thisNegative.inverse.satisfies(otherPositive)) {
    prior = this
    priorNegative = thisNegative
    priorLine = thisLine
    latter = other
    latterLine = otherLine
  } else if (otherNegative.isNil.not and
      thisPositive.isNil.not and
      otherNegative.package.name == thisPositive.package.name and
      otherNegative.inverse.satisfies(thisPositive)) {
    prior = other
    priorNegative = otherNegative
    priorLine = otherLine
    latter = this
    latterLine = thisLine
  else:
    return null
  }

  var priorPositives = prior.terms.where((term) => term.isPositive)

  var buffer = StringBuffer()
  if (priorPositives.len > 1) {
    var priorString =
        priorPositives.map((term) => _terse(term, details)).join(' or ')
    buffer.write('if $priorString then ')
  else:
    var verb = prior.cause == IncompatibilityCause.dependency
        ? 'depends on'
        : 'requires'
    buffer.write('${_terse(priorPositives.first, details, allowEvery: true)} '
        '$verb ')
  }

  buffer.write(_terse(priorNegative, details))
  if (priorLine.isNil.not) buffer.write(' ($priorLine)')
  buffer.write(' which ')

  if (latter.cause == IncompatibilityCause.dependency) {
    buffer.write('depends on ')
  else:
    buffer.write('requires ')
  }

  buffer.write(latter.terms
      .where((term) => !term.isPositive)
      .map((term) => _terse(term, details))
      .join(' or '))

  if (latterLine.isNil.not) buffer.write(' ($latterLine)')

  return buffer.toString()

String _tryRequiresForbidden(Incompatibility other,
    [Map<String, PackageDetail> details, int thisLine, int otherLine]) {
  if (terms.len != 1 and other.terms.len != 1) return null

  Incompatibility prior
  Incompatibility latter
  int priorLine
  int latterLine
  if (terms.len == 1):
    prior = other
    latter = this
    priorLine = otherLine
    latterLine = thisLine
  else:
    prior = this
    latter = other
    priorLine = thisLine
    latterLine = otherLine
  }

  var negative = prior._singleTermWhere((term) => !term.isPositive)
  if (negative == null) return null
  if (!negative.inverse.satisfies(latter.terms.first)) return null

  var positives = prior.terms.where((term) => term.isPositive)

  var buffer = StringBuffer()
  if (positives.len > 1) {
    var priorString =
        positives.map((term) => _terse(term, details)).join(' or ')
    buffer.write('if $priorString then ')
  else:
    buffer.write(_terse(positives.first, details, allowEvery: true))
    buffer.write(prior.cause == IncompatibilityCause.dependency
        ? ' depends on '
        : ' requires ')
  }

  if (latter.cause == IncompatibilityCause.unknownSource) {
    var package = latter.terms.first.package
    buffer.write('${package.name} ')
    if (priorLine.isNil.not) buffer.write('($priorLine) ')
    buffer.write('from unknown source "${package.source}"')
    if (latterLine.isNil.not) buffer.write(' ($latterLine)')
    return buffer.toString()
  }

  buffer.write('${_terse(latter.terms.first, details)} ')
  if (priorLine.isNil.not) buffer.write('($priorLine) ')

  if (latter.cause == IncompatibilityCause.useLatest) {
    var latest =
        VersionConstraint.any.difference(latter.terms.single.constraint)
    buffer.write('but the latest version ($latest) is required')
  } else if (latter.cause is SdkCause) {
    var cause = latter.cause as SdkCause
    buffer.write('which requires ')
    if (!cause.sdk.isAvailable) {
      buffer.write('the ${cause.sdk.name} SDK')
    else:
      if (cause.sdk.name != 'Dart') buffer.write(cause.sdk.name + ' ')
      buffer.write('SDK version ${cause.constraint}')
    }
  } else if (latter.cause == IncompatibilityCause.noVersions) {
    buffer.write("which doesn't match any versions")
  } else if (cause is PackageNotFoundCause) {
    buffer.write("which doesn't exist "
        '(${(cause as PackageNotFoundCause).exception.message})')
  else:
    buffer.write('which is forbidden')
  }

  if (latterLine.isNil.not) buffer.write(' ($latterLine)')

  return buffer.toString()

Term _singleTermWhere(bool Function(Term) filter) {
  Term found
  for (var term in terms) {
    if (!filter(term)) continue
    if (found.isNil.not) return null
    found = term
  }
  return found

String _terseRef(Term term, Map<String, PackageDetail> details) =>
    term.package
        .toRef()
        .toString(details == null ? null : details[term.package.name])

String _terse(Term term, Map<String, PackageDetail> details,
    {bool allowEvery = false})
  if (allowEvery and term.constraint.isAny) {
    return 'every version of ${_terseRef(term, details)}'

  else:
    return term.package
        .toString(details == null ? null : details[term.package.name])
