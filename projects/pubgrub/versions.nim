import 
  std/[options, strutils, strformat, hashes, sequtils, strformat, algorithm]

import
  hmisc/algo/htemplates,
  hmisc/core/all 

{.this: this.}


type
  VersionConstraintKind* = enum
    vckVersion
    vckVersionRange
    vckVersionUnion
    vckEmpty

  VersionConstraint* = ref object
    case kind*: VersionConstraintKind
      of vckVersion:
        major*: int ## The major version number: "1" in "1.2.3".

        minor*: int ## The minor version number: "2" in "1.2.3".

        patch*: int ## The patch version number: "3" in "1.2.3".

        preRelease*: seq[string] ## The pre-release identifier: "foo" in "1.2.3-foo".
        ##
        ## This is split into a list of components, each of which may be either a
        ## string or a non-negative integer. It may also be empty, indicating that
        ## this version has no pre-release identifier.

        build*: seq[string] ## The build identifier: "foo" in "1.2.3+foo".
        ##
        ## This is split into a list of components, each of which may be either a
        ## string or a non-negative integer. It may also be empty, indicating that
        ## this version has no build identifier.

        text*: string ## The original string representation of the version number.
        ##
        ## This preserves textual artifacts like leading zeros that may be left out
        ## of the parsed version.
        
      of vckVersionRange:
        ## Constrains versions to a fall within a given range.
        ##
        ## If there is a minimum, then this only allows versions that are at
        ## that minimum or greater. If there is a maximum, then only versions
        ## less than that are allowed. In other words, this allows `>= min, <
        ## max`.
        ##
        ## Version ranges are ordered first by their lower bounds, then by
        ## their upper bounds. For example, `>=1.0.0 <2.0.0` is before `>=1.5.0
        ## <2.0.0` is before `>=1.5.0 <3.0.0`.

        min*: Option[VersionConstraint] ## The minimum end of the range.
        ##
        ## If [includeMin] is `true`, this will be the minimum allowed version.
        ## Otherwise, it will be the highest version below the range that is
        ## not allowed.
        ##
        ## This may be `null` in which case the range has no minimum end and allows
        ## any version less than the maximum.

        max*: Option[VersionConstraint]  ## The maximum end of the range.
        ##
        ## If [includeMax] is `true`, this will be the maximum allowed version.
        ## Otherwise, it will be the lowest version above the range that is not
        ## allowed.
        ##
        ## This may be `null` in which case the range has no maximum end and
        ## allows any version greater than the minimum.

        includeMin*: bool ## If `true` then [min] is allowed by the range.
        includeMax*: bool ## If `true`, then [max] is allowed by the range.

      of vckVersionUnion:
        ranges*: seq[VersionConstraint]

      of vckEmpty:
        discard





proc splitParts(text: string): seq[string] = text.split(".")

func newVersionUnion(ranges: seq[VersionConstraint]): VersionConstraint = 
  VersionConstraint(kind: vckVersionUnion, ranges: ranges)

func newVersion*(
    major, minor, patch: int, 
    pre: string = "",
    build: string = "",
    text: string = ""
  ): VersionConstraint =
  VersionConstraint(
    kind: vckVersion,
    preRelease: tern(pre.len() != 0, splitParts(pre), @[]),
    build: tern(build.len() != 0,  splitParts(build), @[]),
    major: major,
    minor: minor,
    patch: patch
  )

func newEmptyVersion(): VersionConstraint = VersionConstraint(kind: vckEmpty)

func isPreRelease*(this: VersionConstraint): bool = preRelease.len() > 0

func firstPreRelease*(this: VersionConstraint): VersionConstraint = 
  newVersion(major, minor, patch, pre = "0")

func isFirstPreRelease*(this: VersionConstraint): bool =
  preRelease.len() == 1 and preRelease[0] == "0"


func incrementMajor*(this: VersionConstraint): VersionConstraint = 
  newVersion(major + 1, 0, 0)

func incrementMinor*(this: VersionConstraint): VersionConstraint = 
  newVersion(major, minor + 1, 0)

func incrementPatch*(this: VersionConstraint): VersionConstraint = 
  newVersion(major, minor, patch + 1)



func nextMajor*(this: VersionConstraint): VersionConstraint = 
  ## Gets the next major version number that follows this one.
  ##
  ## If this version is a pre-release of a major version release (i.e. the
  ## minor and patch versions are zero), then it just strips the pre-release
  ## suffix. Otherwise, it increments the major version and resets the minor
  ## and patch.
  if (isPreRelease() and (minor == 0) and (patch == 0)):
    return newVersion(major, minor, patch)

  return incrementMajor()


func nextMinor*(this: VersionConstraint): VersionConstraint = 
  ## Gets the next minor version number that follows this one.
  ##
  ## If this version is a pre-release of a minor version release (i.e. the
  ## patch version is zero), then it just strips the pre-release suffix.
  ## Otherwise, it increments the minor version and resets the patch.
  if (isPreRelease() and patch == 0):
    return newVersion(major, minor, patch)
  

  return incrementMinor()


func nextPatch*(this: VersionConstraint): VersionConstraint = 
  ## Gets the next patch version number that follows this one.
  ##
  ## If this version is a pre-release, then it just strips the pre-release
  ## suffix. Otherwise, it increments the patch version.
  if (isPreRelease()):
    return newVersion(major, minor, patch)

  return incrementPatch()


proc nextBreaking*(this: VersionConstraint): VersionConstraint = 
  ## Gets the next breaking version number that follows this one.
  ##
  ## Increments [major] if it's greater than zero, otherwise [minor], resets
  ## subsequent digits to zero, and strips any [preRelease] or [build]
  ## suffix.
  if (major == 0):
    return incrementMinor()

  return incrementMajor()



proc areAdjacent(range1, range2: VersionConstraint): bool =
  ## Returns whether [range1] is immediately next to, but not overlapping,
  ## [range2].
  if (range1.max != range2.min): return false

  return (range1.includeMax and not range2.includeMin) or
      (not range1.includeMax and range2.includeMin)

proc compareTo(this, other: VersionConstraint): int


proc compareMax(this, other: VersionConstraint): int =
  ## Compares the maximum values of `this` and [other].
  if (max.isNone()):
    if (other.max.isNone()):
      return 0
    else:
      return 1

  elif (other.max.isNone()):
    return -1

  var res = max.get().compareTo(other.max.get())
  if (res != 0): return res
  if (includeMax != other.includeMax): 
    if includeMax:
      return 1 
    else:
      return -1

  return 0

proc compareTo(this, other: VersionConstraint): int =
  if (min.isNone()) :
    if (other.min.isNone()): 
      return compareMax(other)

    else:
      return -1

  elif (other.min.isNone()):
    return 1

  var res = min.get().compareTo(other.min.get())
  if (res != 0): return res
  if (includeMin != other.includeMin):
    if includeMin:
      return -1
    else: 
      return 1

  return compareMax(other)


proc allowsLower(range1, range2: VersionConstraint): bool = 
  ## Returns whether [range1] allows lower versions than [range2].
  if (range1.min.isNone()): return range2.min.isSome()
  if (range2.min.isNone()): return false

  var comparison = range1.min.get().compareTo(range2.min.get())
  if (comparison == -1): return true
  if (comparison == 1): return false
  return range1.includeMin and not range2.includeMin

proc  allowsHigher(range1, range2: VersionConstraint): bool = 
  ## Returns whether [range1] allows higher versions than [range2].
  if (range1.max.isNone()): return range2.max.isSome()
  if (range2.max.isNone()): return false

  var comparison = range1.max.get().compareTo(range2.max.get())
  if (comparison == 1): return true
  if (comparison == -1): return false
  return range1.includeMax and range2.includeMax

proc strictlyLower(range1, range2: VersionConstraint): bool = 
  ## Returns whether [range1] allows only versions lower than those allowed by
  ## [range2].
  if (range1.max.isNone() or range2.min.isNone()): return false

  var comparison = range1.max.get().compareTo(range2.min.get())
  if (comparison == -1): return true
  if (comparison == 1): return false
  return not range1.includeMax or not range2.includeMin


proc strictlyHigher(range1, range2: VersionConstraint): bool = 
  ## Returns whether [range1] allows only versions higher than those allowed by
  ## [range2].
  strictlyLower(range2, range1)

func equalsWithoutPreRelease*(version1, version2: VersionConstraint): bool =
  version1.major == version2.major and
  version1.minor == version2.minor and
  version1.patch == version2.patch


proc `$`*(this: VersionConstraint): string

proc newVersionRange*(
    max: Option[VersionConstraint] = none(VersionConstraint),
    min: Option[VersionConstraint] = none(VersionConstraint),
    includeMin: bool = false,
    includeMax: bool = false,
    alwaysIncludeMaxPreRelease: bool = false
  ): VersionConstraint =
  ## Creates a new version range from [min] to [max], either inclusive or
  ## exclusive.
  ##
  ## If it is an error if [min] is greater than [max].
  ##
  ## Either [max] or [min] may be omitted to not clamp the range at that end.
  ## If both are omitted, the range allows all versions.
  ##
  ## If [includeMin] is `true`, then the minimum end of the range is inclusive.
  ## Likewise, passing [includeMax] as `true` makes the upper end inclusive.
  ##
  ## If [alwaysIncludeMaxPreRelease] is `true`, this will always include
  ## pre-release versions of an exclusive [max]. Otherwise, it will use the
  ## default behavior for pre-release versions of [max].

  var max = max
  if (min.isSome() and max.isSome() and min.get() > max.get()):
    assert false,
      fmt"Minimum version (""{min}"") must be less than maximum (""{max}"")."

  if (not alwaysIncludeMaxPreRelease and
      not includeMax and
      max.isSome() and
      not max.get().isPreRelease() and
      max.get().build.len() == 0 and
      (min.isNone() or
          not min.get().isPreRelease() or
          not equalsWithoutPreRelease(min.get(), max.get()))):
    max = some max.get().firstPreRelease

  return VersionConstraint(
    kind: vckVersionRange,
    min: min, max: max, includeMin: includeMin, includeMax: includeMax)

proc newVersionRangeAny(): VersionConstraint = newVersionRange()

func isAny(this: VersionConstraint): bool = min.isNone() and max.isNone()

proc compatibleWith*(version: VersionConstraint): VersionConstraint =
  ## Creates a version constraint which allows all versions that are
  ## backward compatible with [version].
  ##
  ## Versions are considered backward compatible with [version] if they
  ## are greater than or equal to [version], but less than the next breaking
  ## version ([Version.nextBreaking]) of [version].
  newVersionRange(some version, some version.nextBreaking.firstPreRelease, true, false)

func `==`(this, other: VersionConstraint): bool = 
  if not(other of VersionConstraint): return false

  return this.min == other.min and
    this.max == other.max and 
    includeMin == other.includeMin and 
    includeMax == other.includeMax

func hash*(this: VersionConstraint): Hash =
  !$(min.get().hash() !& (max.get().hash() *% 3) !& (includeMin.hash() *% 5) !& (includeMax.hash() *% 7))

func isEmpty*(this: VersionConstraint): bool = 
  case this.kind:
    else:
      assert false, $this.kind

proc allows*(this, other: VersionConstraint): bool =
  case this.kind:
    of vckVersionRange:
      if (min.isSome()):
        if (other < min.get()): return false
        if (includeMin and other == min.get()): return false

      if (max.isSome()):
        if (other > max.get()): return false
        if (includeMax and other == max.get()): return false

      return true

    else:
      assert false


proc allowsAll*(this, other: VersionConstraint): bool =
  case this.kind:
    of vckVersionRange:
      if (other.isEmpty): return true
      case other.kind:
        of vckEmpty: 
          return true

        of vckVersion:
          return allows(other)

        of vckVersionUnion: 
          return other.ranges.allIt(it.allowsAll)

        of vckVersionRange: 
          return not allowsLower(other, this) and not allowsHigher(other, this)

    else:
      assert false, $this.kind



proc rangesFor*(constraint: VersionConstraint): seq[VersionConstraint] =
  ## Returns [constraint] as a list of ranges.
  ##
  ## This is used to normalize ranges of various types.
  if (constraint.isEmpty): return @[]
  if (constraint.kind == vckVersionUnion): return constraint.ranges
  if (constraint.kind == vckVersionRange): return @[constraint]
  assert false

proc allowsAny*(this, other: VersionConstraint): bool =
  case this.kind:
    of vckVersionRange:
      if (other.isEmpty): return false
      case other.kind:
        of vckEmpty: return true
        of vckVersion: return allows(other)
        of vckVersionUnion: return other.ranges.anyIt(allowsAny(it))
        of vckVersionRange:
          return not strictlyLower(other, this) and not strictlyHigher(other, this)

    of vckVersion:
      return other.allows(this)

    of vckEmpty:
      return other.isEmpty()

    of vckVersionUnion:
      var ourRanges = ranges
      var theirRanges = rangesFor(other)

      # Because both lists of ranges are ordered by minimum version, we can
      # safely move through them linearly here.
      var ourIdx, theirIdx: int
      var ourRangesMoved = ourRanges[ourIdx]; inc ourIdx
      var theirRangesMoved = theirRanges[theirIdx]; inc theirIdx
      while (ourIdx < ourRanges.len and theirIdx < theirRanges.len):
        if (ourRanges[ourIdx].allowsAny(theirRanges[theirIdx])):
          return true
        
        # Move the constraint with the lower max value forward. This ensures that
        # we keep both lists in sync as much as possible.
        if (allowsHigher(theirRanges[theirIdx], ourRanges[ourIdx])):
          ourRangesMoved = ourRanges[ourIdx]; inc ourIdx

        else:
          theirRangesMoved = theirRanges[theirIdx]; inc theirIdx

      return false



proc intersect*(this, other: VersionConstraint): VersionConstraint =
  if (other.isEmpty): return other
  if (other of vckVersionUnion): return other.intersect(this)

  # A range and a Version just yields the version if it's in the range.
  if (other of vckVersion):
    return (if allows(other): other else: newEmptyVersion())

  if (other of vckVersionRange):
    # Intersect the two ranges.
    var intersectMin: Option[VersionConstraint]
    var intersectIncludeMin: bool
    if (allowsLower(this, other)):
      if (strictlyLower(this, other)): return newEmptyVersion()
      intersectMin = other.min
      intersectIncludeMin = other.includeMin

    else:
      if (strictlyLower(other, this)): return newEmptyVersion()
      intersectMin = min
      intersectIncludeMin = includeMin

    var intersectMax: Option[VersionConstraint]
    var intersectIncludeMax: bool
    if (allowsHigher(this, other)):
      intersectMax = other.max
      intersectIncludeMax = other.includeMax

    else:
      intersectMax = max
      intersectIncludeMax = includeMax

    if (intersectmin.isNone() and intersectMax.isNone()):
      # Open range.
      return newVersionRange()

    # If the range is just a single version.
    if (intersectMin == intersectMax):
      # Because we already verified that the lower range isn't strictly
      # lower, there must be some overlap.
      assert(intersectIncludeMin and intersectIncludeMax)
      return intersectMin.get()

    # If we got here, there is an actual range.
    return newVersionRange(
      min = intersectMin,
      max = intersectMax,
      includeMin = intersectIncludeMin,
      includeMax = intersectIncludeMax,
      alwaysIncludeMaxPreRelease = true
    )

proc union*(this, other: VersionConstraint): VersionConstraint

proc unionOf*(constraints: seq[VersionConstraint]): VersionConstraint =
  ## Creates a new version constraint that is the union of [constraints].
  ##
  ## It allows any versions that any of those constraints allows. If
  ## [constraints] is empty, this returns a constraint that allows no versions.
  var flattened: seq[VersionConstraint]
  for constraint in constraints:
    if (constraint of vckVersionUnion):
      flattened.add constraint.ranges

    if (constraint of vckVersionRange):
      flattened.add constraint

    else:
      raise newArgumentError(&"Unknown VersionConstraint type {constraint.kind}.");

  if (flattened.len() == 0): return newEmptyVersion()

  if (flattened.anyIt(it.isAny)):
    return newVersionRangeAny()

  flattened.sort()

  var merged: seq[VersionConstraint]
  for constraint in flattened:
    ## Merge this constraint with the previous one, but only if they touch.
    if (merged.len() == 0 or
       (not merged.last.allowsAny(constraint) and 
        not areAdjacent(merged.last, constraint))):

      merged.add(constraint) 

    else:
      merged[merged.len - 1] = merged[^1].union(constraint)

  if (merged.len == 1): return merged[0]
  return newVersionUnion(merged)

proc union*(this, other: VersionConstraint): VersionConstraint = 
  if (other of vckVersion):
    if (allows(other)): return this

    if (other == min.get()):
      return newVersionRange(
          min =min,
          max =max,
          includeMin =true,
          includeMax =includeMax,
          alwaysIncludeMaxPreRelease =true)

    if (other == max.get()):
      return newVersionRange(
          min = min,
          max = max,
          includeMin = includeMin,
          includeMax = true,
          alwaysIncludeMaxPreRelease = true)

    return unionOf(@[this, other])

  if (other of vckVersionRange):
    # If the two ranges don't overlap, we won't be able to create a single
    # VersionRange for both of them.
    var edgesTouch = (max.isSome() and
            max == other.min and
            (includeMax or other.includeMin)) or
        (min.isSome() and min == other.max and (includeMin or other.includeMax))
    if (not edgesTouch and not allowsAny(other)):
      return unionOf(@[this, other])

    var unionMin: Option[VersionConstraint]
    var unionIncludeMin: bool
    if (allowsLower(this, other)):
      unionMin = min
      unionIncludeMin = includeMin
    else:
      unionMin = other.min
      unionIncludeMin = other.includeMin

    var unionMax: Option[VersionConstraint]
    var unionIncludeMax: bool
    if (allowsHigher(this, other)):
      unionMax = max
      unionIncludeMax = includeMax
    else:
      unionMax = other.max
      unionIncludeMax = other.includeMax

    return newVersionRange(
        min = unionMin,
        max = unionMax,
        includeMin = unionIncludeMin,
        includeMax = unionIncludeMax,
        alwaysIncludeMaxPreRelease = true)

  return unionOf(@[this, other])

proc difference*(this, other: VersionConstraint): VersionConstraint =
  if (other.isEmpty): return this

  if (other of vckVersion):
    if (not allows(other)): return this

    if (other == min.get()):
      if (not includeMin): return this
      return newVersionRange(
          min = min,
          max = max,
          includeMin = false,
          includeMax = includeMax,
          alwaysIncludeMaxPreRelease = true)

    if (other == max.get()):
      if (not includeMax): return this
      return newVersionRange(
          min = min,
          max = max,
          includeMin = includeMin,
          includeMax = false,
          alwaysIncludeMaxPreRelease = true)

    return newVersionUnion(@[
      newVersionRange(
          min = min,
          max = some other,
          includeMin = includeMin,
          includeMax = false,
          alwaysIncludeMaxPreRelease = true),
      newVersionRange(
          min = some other,
          max = max,
          includeMin = false,
          includeMax = includeMax,
          alwaysIncludeMaxPreRelease = true)
    ])

  elif (other of vckVersionRange):
    if (not allowsAny(other)): return this

    var before: Option[VersionConstraint]
    if (not allowsLower(this, other)):
      before = none(VersionConstraint)

    elif (min == other.min):
      assert(includeMin and not other.includeMin)
      assert(min.isSome())
      before = min

    else:
      before = some newVersionRange(
          min = min,
          max = other.min,
          includeMin = includeMin,
          includeMax = not other.includeMin,
          alwaysIncludeMaxPreRelease = true)

    var after: Option[VersionConstraint]
    if (not allowsHigher(this, other)):
      after = none(VersionConstraint)

    elif (max == other.max):
      assert(includeMax and not other.includeMax)
      assert(max.isSome())
      after = max

    else:
      after = some newVersionRange(
          min = other.max,
          max = max,
          includeMin = not other.includeMax,
          includeMax = includeMax,
          alwaysIncludeMaxPreRelease = true)

    if (before.isNone() and after.isNone()): return newEmptyVersion()
    if (before.isNone()): return after.get()
    if (after.isNone()): return before.get()
    return newVersionUnion(@[before.get(), after.get()])

  elif (other of vckVersionUnion):
    var ranges: seq[VersionConstraint]
    var current = this

    for rnge in other.ranges:
      # Skip any ranges that are strictly lower than [current].
      if (strictlyLower(rnge, current)): continue

      # If we reach a range strictly higher than [current], no more ranges
      # will be relevant so we can bail early.
      if (strictlyHigher(rnge, current)): break

      var difference = current.difference(rnge)
      if (difference.isEmpty):
        return newEmptyVersion()

      elif (difference of vckVersionUnion):
        # If [range] split [current] in half, we only need to continue
        # checking future ranges against the latter half.
        assert(difference.ranges.len == 2)
        ranges.add(difference.ranges[0])
        current = difference.ranges[^1]

      else:
        current = difference


    if (ranges.len == 0): return current
    return newVersionUnion(ranges & current)

  raise newArgumentError("Unknown VersionConstraint type $other.")



proc  `$`*(this: VersionConstraint): string = 
  if (min.isSome()):
    result.add(if includeMin: ">=" else: ">")

  if (max.isSome()):
    let max = max.get()
    if (min.isSome()): result.add " "
    if (includeMax):
      result.add("<=")
      result.add($max)

    else:
      result.add("<")
      if (max.isFirstPreRelease):
        # Since `"<$max"` would parse the same as `"<$max-0"`, we just emit
        # `<$max` to avoid confusing "-0" suffixes.
        result.add(&"{max.major}.{max.minor}.{max.patch}")
      else:
        result.add($max)

        # If `">=$min <$max"` would parse as `">=$min <$max-0"`, add `-*` to
        # indicate that actually does allow pre-release versions.
        var minIsPreReleaseOfMax = min.isSome() and min.get().isPreRelease and
            equalsWithoutPreRelease(min.get(), max)
        if (not max.isPreRelease and max.build.len == 0 and not minIsPreReleaseOfMax):
          result.add("-∞")

  if (min.isNone() and max.isNone()):  result.add "any"
