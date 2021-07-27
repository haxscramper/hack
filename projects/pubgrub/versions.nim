import 
  ./types

import std/[options, strutils, strformat]

import hmisc/algo/htemplates

{.this: this.}

proc splitParts(text: string): seq[string] = text.split(".")

func newVersion*(
    major, minor, patch: int, 
    pre: string = "",
    build: string = "",
    text: string = ""
  ): Version =
  Version(
    preRelease: tern(pre.len() != 0, splitParts(pre), @[]),
    build: tern(build.len() != 0,  splitParts(build), @[]),
    major: major,
    minor: minor,
    patch: patch
  )



func isPreRelease*(this: Version): bool = preRelease.len() > 0

func firstPreRelease*(this: Version): Version = 
  newVersion(major, minor, patch, pre = "0")

func isFirstPreRelease*(this: Version): bool =
  preRelease.len() == 1 and preRelease[0] == "0"


func incrementMajor*(this: Version): Version = newVersion(major + 1, 0, 0)
func incrementMinor*(this: Version): Version = newVersion(major, minor + 1, 0)
func incrementPatch*(this: Version): Version = newVersion(major, minor, patch + 1)


func nextMajor*(this: Version): Version = 
  ## Gets the next major version number that follows this one.
  ##
  ## If this version is a pre-release of a major version release (i.e. the
  ## minor and patch versions are zero), then it just strips the pre-release
  ## suffix. Otherwise, it increments the major version and resets the minor
  ## and patch.
  if (isPreRelease() and (minor == 0) and (patch == 0)):
    return newVersion(major, minor, patch);

  return incrementMajor()


func nextMinor*(this: Version): Version = 
  ## Gets the next minor version number that follows this one.
  ##
  ## If this version is a pre-release of a minor version release (i.e. the
  ## patch version is zero), then it just strips the pre-release suffix.
  ## Otherwise, it increments the minor version and resets the patch.
  if (isPreRelease() and patch == 0):
    return newVersion(major, minor, patch)
  

  return incrementMinor();


func nextPatch*(this: Version): Version = 
  ## Gets the next patch version number that follows this one.
  ##
  ## If this version is a pre-release, then it just strips the pre-release
  ## suffix. Otherwise, it increments the patch version.
  if (isPreRelease()):
    return newVersion(major, minor, patch)

  return incrementPatch()


proc nextBreaking*(this: Version): Version = 
  ## Gets the next breaking version number that follows this one.
  ##
  ## Increments [major] if it's greater than zero, otherwise [minor], resets
  ## subsequent digits to zero, and strips any [preRelease] or [build]
  ## suffix.
  if (major == 0):
    return incrementMinor()

  return incrementMajor()

func `$`*(v: Version): string = "<<<>>>"

func equalsWithoutPreRelease*(version1, version2: Version): bool =
  version1.major == version2.major and
  version1.minor == version2.minor and
  version1.patch == version2.patch


proc newVersionRange*(
    min, max: Option[Version],
    includeMin: bool = false,
    includeMax: bool = false,
    alwaysIncludeMaxPreRelease: bool = false
  ): VersionRange =
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

  return VersionRange(
    min: min, max: max, includeMin: includeMin, includeMax: includeMax)

proc compatibleWith*(version: Version): VersionConstraint =
  ## Creates a version constraint which allows all versions that are
  ## backward compatible with [version].
  ##
  ## Versions are considered backward compatible with [version] if they
  ## are greater than or equal to [version], but less than the next breaking
  ## version ([Version.nextBreaking]) of [version].
  newVersionRange(some version, some version.nextBreaking.firstPreRelease, true, false)

func `==`(this, other: Version): bool = 
  if not(other of VersionRange): return false

  return this.min == other.min and
    this.max == other.max and 
    includeMin() == other.includeMin and 
    includeMax() == other.includeMax

int get hashCode =>
    min.hashCode ^
    (max.hashCode * 3) ^
    (includeMin.hashCode * 5) ^
    (includeMax.hashCode * 7);

bool get isEmpty => false;

bool get isAny => min == null && max == null;

## Tests if [other] falls within this version range.
bool allows(Version other) {
  if (min != null) {
    if (other < min!) return false;
    if (!includeMin && other == min) return false;
  }

  if (max != null) {
    if (other > max!) return false;
    if (!includeMax && other == max) return false;
  }

  return true;
}

func allowsAll(this, other: VersionConstraint): bool =
  if (other.isEmpty): return true
  if (other is Version): return allows(other)

  if (other is VersionUnion):
    return other.ranges.every(allowsAll);

  if (other is VersionRange):
    return !allowsLower(other, this) && !allowsHigher(other, this);

  throw ArgumentError('Unknown VersionConstraint type $other.');

func allowsAny(VersionConstraint other): bool
  if (other.isEmpty) return false
  if (other is Version) return allows(other)

  if (other is VersionUnion):
    return other.ranges.any(allowsAny)

  if (other is VersionRange):
    return !strictlyLower(other, this) && !strictlyHigher(other, this)

  throw ArgumentError('Unknown VersionConstraint type $other.')

VersionConstraint intersect(VersionConstraint other) {
  if (other.isEmpty) return other;
  if (other is VersionUnion) return other.intersect(this);

  # A range and a Version just yields the version if it's in the range.
  if (other is Version) {
    return allows(other) ? other : VersionConstraint.empty;
  }

  if (other is VersionRange) {
    # Intersect the two ranges.
    Version? intersectMin;
    bool intersectIncludeMin;
    if (allowsLower(this, other)) {
      if (strictlyLower(this, other)) return VersionConstraint.empty;
      intersectMin = other.min;
      intersectIncludeMin = other.includeMin;
    } else {
      if (strictlyLower(other, this)) return VersionConstraint.empty;
      intersectMin = min;
      intersectIncludeMin = includeMin;
    }

    Version? intersectMax;
    bool intersectIncludeMax;
    if (allowsHigher(this, other)) {
      intersectMax = other.max;
      intersectIncludeMax = other.includeMax;
    } else {
      intersectMax = max;
      intersectIncludeMax = includeMax;
    }

    if (intersectMin == null && intersectMax == null) {
      # Open range.
      return VersionRange();
    }

    # If the range is just a single version.
    if (intersectMin == intersectMax) {
      # Because we already verified that the lower range isn't strictly
      # lower, there must be some overlap.
      assert(intersectIncludeMin && intersectIncludeMax);
      return intersectMin!;
    }

    # If we got here, there is an actual range.
    return VersionRange(
        min: intersectMin,
        max: intersectMax,
        includeMin: intersectIncludeMin,
        includeMax: intersectIncludeMax,
        alwaysIncludeMaxPreRelease: true);
  }

  throw ArgumentError('Unknown VersionConstraint type $other.');
}

VersionConstraint union(VersionConstraint other) {
  if (other is Version) {
    if (allows(other)) return this;

    if (other == min) {
      return VersionRange(
          min: min,
          max: max,
          includeMin: true,
          includeMax: includeMax,
          alwaysIncludeMaxPreRelease: true);
    }

    if (other == max) {
      return VersionRange(
          min: min,
          max: max,
          includeMin: includeMin,
          includeMax: true,
          alwaysIncludeMaxPreRelease: true);
    }

    return VersionConstraint.unionOf([this, other]);
  }

  if (other is VersionRange) {
    # If the two ranges don't overlap, we won't be able to create a single
    # VersionRange for both of them.
    var edgesTouch = (max != null &&
            max == other.min &&
            (includeMax || other.includeMin)) ||
        (min != null && min == other.max && (includeMin || other.includeMax));
    if (!edgesTouch && !allowsAny(other)) {
      return VersionConstraint.unionOf([this, other]);
    }

    Version? unionMin;
    bool unionIncludeMin;
    if (allowsLower(this, other)) {
      unionMin = min;
      unionIncludeMin = includeMin;
    } else {
      unionMin = other.min;
      unionIncludeMin = other.includeMin;
    }

    Version? unionMax;
    bool unionIncludeMax;
    if (allowsHigher(this, other)) {
      unionMax = max;
      unionIncludeMax = includeMax;
    } else {
      unionMax = other.max;
      unionIncludeMax = other.includeMax;
    }

    return VersionRange(
        min: unionMin,
        max: unionMax,
        includeMin: unionIncludeMin,
        includeMax: unionIncludeMax,
        alwaysIncludeMaxPreRelease: true);
  }

  return VersionConstraint.unionOf([this, other]);
}

VersionConstraint difference(VersionConstraint other) {
  if (other.isEmpty) return this;

  if (other is Version) {
    if (!allows(other)) return this;

    if (other == min) {
      if (!includeMin) return this;
      return VersionRange(
          min: min,
          max: max,
          includeMin: false,
          includeMax: includeMax,
          alwaysIncludeMaxPreRelease: true);
    }

    if (other == max) {
      if (!includeMax) return this;
      return VersionRange(
          min: min,
          max: max,
          includeMin: includeMin,
          includeMax: false,
          alwaysIncludeMaxPreRelease: true);
    }

    return VersionUnion.fromRanges([
      VersionRange(
          min: min,
          max: other,
          includeMin: includeMin,
          includeMax: false,
          alwaysIncludeMaxPreRelease: true),
      VersionRange(
          min: other,
          max: max,
          includeMin: false,
          includeMax: includeMax,
          alwaysIncludeMaxPreRelease: true)
    ]);
  } else if (other is VersionRange) {
    if (!allowsAny(other)) return this;

    VersionRange? before;
    if (!allowsLower(this, other)) {
      before = null;
    } else if (min == other.min) {
      assert(includeMin && !other.includeMin);
      assert(min != null);
      before = min;
    } else {
      before = VersionRange(
          min: min,
          max: other.min,
          includeMin: includeMin,
          includeMax: !other.includeMin,
          alwaysIncludeMaxPreRelease: true);
    }

    VersionRange? after;
    if (!allowsHigher(this, other)) {
      after = null;
    } else if (max == other.max) {
      assert(includeMax && !other.includeMax);
      assert(max != null);
      after = max;
    } else {
      after = VersionRange(
          min: other.max,
          max: max,
          includeMin: !other.includeMax,
          includeMax: includeMax,
          alwaysIncludeMaxPreRelease: true);
    }

    if (before == null && after == null) return VersionConstraint.empty;
    if (before == null) return after!;
    if (after == null) return before;
    return VersionUnion.fromRanges([before, after]);
  } else if (other is VersionUnion) {
    var ranges = <VersionRange>[];
    var current = this;

    for (var range in other.ranges) {
      # Skip any ranges that are strictly lower than [current].
      if (strictlyLower(range, current)) continue;

      # If we reach a range strictly higher than [current], no more ranges
      # will be relevant so we can bail early.
      if (strictlyHigher(range, current)) break;

      var difference = current.difference(range);
      if (difference.isEmpty) {
        return VersionConstraint.empty;
      } else if (difference is VersionUnion) {
        # If [range] split [current] in half, we only need to continue
        # checking future ranges against the latter half.
        assert(difference.ranges.length == 2);
        ranges.add(difference.ranges.first);
        current = difference.ranges.last;
      } else {
        current = difference as VersionRange;
      }
    }

    if (ranges.isEmpty) return current;
    return VersionUnion.fromRanges(ranges..add(current));
  }

  throw ArgumentError('Unknown VersionConstraint type $other.');
}

int compareTo(VersionRange other) {
  if (min == null) {
    if (other.min == null) return _compareMax(other);
    return -1;
  } else if (other.min == null) {
    return 1;
  }

  var result = min!.compareTo(other.min!);
  if (result != 0) return result;
  if (includeMin != other.includeMin) return includeMin ? -1 : 1;

  return _compareMax(other);
}

## Compares the maximum values of `this` and [other].
int _compareMax(VersionRange other) {
  if (max == null) {
    if (other.max == null) return 0;
    return 1;
  } else if (other.max == null) {
    return -1;
  }

  var result = max!.compareTo(other.max!);
  if (result != 0) return result;
  if (includeMax != other.includeMax) return includeMax ? 1 : -1;
  return 0;
}

String toString() {
var buffer = StringBuffer();

final min = this.min;
if (min != null) {
  buffer..write(includeMin ? '>=' : '>')..write(min);
}

final max = this.max;

if (max != null) {
  if (min != null) buffer.write(' ');
  if (includeMax) {
    buffer..write('<=')..write(max);
  } else {
    buffer.write('<');
    if (max.isFirstPreRelease) {
      # Since `"<$max"` would parse the same as `"<$max-0"`, we just emit
      # `<$max` to avoid confusing "-0" suffixes.
      buffer.write('${max.major}.${max.minor}.${max.patch}');
    } else {
      buffer.write(max);

      # If `">=$min <$max"` would parse as `">=$min <$max-0"`, add `-*` to
      # indicate that actually does allow pre-release versions.
      var minIsPreReleaseOfMax = min != null &&
          min.isPreRelease &&
          equalsWithoutPreRelease(min, max);
      if (!max.isPreRelease && max.build.isEmpty && !minIsPreReleaseOfMax) {
        buffer.write('-∞');
      }
    }
  }
}

if (min == null && max == null) buffer.write('any');
return buffer.toString();
