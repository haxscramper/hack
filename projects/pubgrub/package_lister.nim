import
  ./term,
  ./incompatibility,
  ./incompatibility_cause,
  ./types

# import '../log.dart' as log;
# import '../package.dart';
# import '../package_name.dart';
# import '../pubspec.dart';
# import '../sdk.dart';
# import '../source.dart';
# import '../system_cache.dart';
# import '../utils.dart';
# import 'incompatibility.dart';
# import 'incompatibility_cause.dart';
# import 'term.dart';


  # ## All versions of the package, sorted by [Version.compareTo].
  # Future<List<PackageId>> get _versions => _versionsMemo.runOnce(() async {
  #       _cachedVersions = await withDependencyType(
  #           _dependencyType, () => _source.getVersions(_ref));
  #       _cachedVersions.sort((id1, id2) => id1.version.compareTo(id2.version));
  #       return _cachedVersions;
  #     });
  # final _versionsMemo = AsyncMemoizer<List<PackageId>>();

  # ## The most recent version of this package (or the oldest, if we're
  # ## downgrading).
  # Future<PackageId> get latest =>
  #     _latestMemo.runOnce(() => bestVersion(VersionConstraint.any));
  # final _latestMemo = AsyncMemoizer<PackageId>();

  ## Creates a package lister for the dependency identified by [ref].
  PackageLister(SystemCache cache, this._ref, this._locked,
      this._dependencyType, this._overriddenPackages,
      {bool downgrade = false})
      : _source = cache.source(_ref.source),
        _isDowngrade = downgrade;

  ## Creates a package lister for the root [package].
  PackageLister.root(Package package)
      : _ref = PackageRef.root(package),
        _source = _RootSource(package),
        # Treat the package as locked so we avoid the logic for finding the
        # boundaries of various constraints, which is useless for the root
        # package.
        _locked = PackageId.root(package),
        _dependencyType = DependencyType.none,
        _overriddenPackages = const UnmodifiableSetView.empty(),
        _isDowngrade = false;

  ## Returns the number of versions of this package that match [constraint].
proc countVersions(
    self: PackageLister, constraint: VersionConstraint ): int =
  if (locked.isNil().not() and
      constraint.allows(locked.version)) return 1

  try:
    return (self.versions)
      .filter((id) => constraint.allows(id.version))
      .length

  except PackageNotFoundException:
    # If it fails for any reason, just treat that as no versions. This will
    # sort this reference higher so that we can traverse into it and report
    # the error in a user-friendly way.
    return 0;

Future<PackageId> bestVersion(VersionConstraint constraint) async {
  ## Returns the best version of this package that matches [constraint]
  ## according to the solver's prioritization scheme, or `null` if no versions
  ## match.
  ##
  ## Throws a [PackageNotFoundException] if this lister's package doesn't
  ## exist.

  if (self.locked.isNil().not() and
      constraint.allows(self.locked.version)):
    return self.locked;

  # If [constraint] has a minimum (or a maximum in downgrade mode), we can
  # bail early once we're past it.
  var isPastLimit = (Version _) => false

  if (constraint of VersionRange):
    if (self.isDowngrade):
      var max = constraint.max
      if (max.isNil()): isPastLimit = (version) => version > max

    else:
      var min = constraint.min
      if (min.isNil()): isPastLimit = (version) => version < min

    # Return the most preferable version that matches [constraint]: the latest
    # non-prerelease version if one exists, or the latest prerelease version
    # otherwise.
    bestPrerelease: PackageId
    for id in (if self.isDowngrade: self.versions else: versions.reversed):
      if (isPastLimit.isNil().not() and isPastLimit(id.version)): break

      if (not constraint.allows(id.version)): continue
      if (not id.version.isPreRelease): return id
      bestPrerelease ??= id;

    return bestPrerelease

Future<List<Incompatibility>> incompatibilitiesFor(PackageId id) async {
  ## Returns incompatibilities that encapsulate [id]'s dependencies, or that
  ## indicate that it can't be safely selected.
  ##
  ## If multiple subsequent versions of this package have the same
  ## dependencies, this will return incompatibilities that reflect that. It
  ## won't return incompatibilities that have already been returned by a
  ## previous call to [incompatibilitiesFor].

  if (_knownInvalidVersions.allows(id.version)) return const [];

  Pubspec pubspec;
  try {
    pubspec =
        await withDependencyType(_dependencyType, () => _source.describe(id));
  } on PubspecException catch (error) {
    # The lockfile for the pubspec couldn't be parsed,
    log.fine('Failed to parse pubspec for $id:\n$error');
    _knownInvalidVersions = _knownInvalidVersions.union(id.version);
    return [
      Incompatibility(
          [Term(id.toRange(), true)], IncompatibilityCause.noVersions)
    ];
  } on PackageNotFoundException {
    # We can only get here if the lockfile refers to a specific package
    # version that doesn't exist (probably because it was yanked).
    _knownInvalidVersions = _knownInvalidVersions.union(id.version);
    return [
      Incompatibility(
          [Term(id.toRange(), true)], IncompatibilityCause.noVersions)
    ];
  }

  if (_cachedVersions == null &&
      _locked != null &&
      id.version == _locked.version) {
    if (_listedLockedVersion) return const [];

    var depender = id.toRange();
    _listedLockedVersion = true;
    for (var sdk in sdks.values) {
      if (!_matchesSdkConstraint(pubspec, sdk)) {
        return [
          Incompatibility([Term(depender, true)],
              SdkCause(pubspec.sdkConstraints[sdk.identifier], sdk))
        ];
      }
    }

    if (id.isRoot) {
      var incompatibilities = <Incompatibility>[];

      for (var range in pubspec.dependencies.values) {
        if (pubspec.dependencyOverrides.containsKey(range.name)) continue;
        incompatibilities.add(_dependency(depender, range));
      }

      for (var range in pubspec.devDependencies.values) {
        if (pubspec.dependencyOverrides.containsKey(range.name)) continue;
        incompatibilities.add(_dependency(depender, range));
      }

      for (var range in pubspec.dependencyOverrides.values) {
        incompatibilities.add(_dependency(depender, range));
      }

      return incompatibilities;
    } else {
      return pubspec.dependencies.values
          .where((range) => !_overriddenPackages.contains(range.name))
          .map((range) => _dependency(depender, range))
          .toList();
    }
  }

  var versions = await _versions;
  var index = lowerBound(versions, id,
      compare: (id1, id2) => id1.version.compareTo(id2.version));
  assert(index < versions.length);
  assert(versions[index].version == id.version);

  for (var sdk in sdks.values) {
    var sdkIncompatibility = await _checkSdkConstraint(index, sdk);
    if (sdkIncompatibility != null) return [sdkIncompatibility];
  }

  # Don't recompute dependencies that have already been emitted.
  var dependencies = Map<String, PackageRange>.from(pubspec.dependencies);
  for (var package in dependencies.keys.toList()) {
    if (_overriddenPackages.contains(package)) {
      dependencies.remove(package);
      continue;
    }

    var constraint = _alreadyListedDependencies[package];
    if (constraint != null && constraint.allows(id.version)) {
      dependencies.remove(package);
    }
  }

  var lower = await _dependencyBounds(dependencies, index, upper: false);
  var upper = await _dependencyBounds(dependencies, index);

  return ordered(dependencies.keys).map((package) {
    var constraint = VersionRange(
        min: lower[package],
        includeMin: true,
        max: upper[package],
        alwaysIncludeMaxPreRelease: true);

    _alreadyListedDependencies[package] = constraint.union(
        _alreadyListedDependencies[package] ?? VersionConstraint.empty);

    return _dependency(
        _ref.withConstraint(constraint), dependencies[package]);
  }).toList();
}

## Returns an [Incompatibility] that represents a dependency from [depender]
## onto [target].
Incompatibility _dependency(PackageRange depender, PackageRange target) =>
    Incompatibility([Term(depender, true), Term(target, false)],
        IncompatibilityCause.dependency);

## If the version at [index] in [_versions] isn't compatible with the current
## version of [sdk], returns an [Incompatibility] indicating that.
##
## Otherwise, returns `null`.
Future<Incompatibility> _checkSdkConstraint(int index, Sdk sdk) async {
  var versions = await _versions;

  bool allowsSdk(Pubspec pubspec) => _matchesSdkConstraint(pubspec, sdk);

  if (allowsSdk(await _describeSafe(versions[index]))) return null;

  var bounds = await _findBounds(index, (pubspec) => !allowsSdk(pubspec));
  var incompatibleVersions = VersionRange(
      min: bounds.first == 0 ? null : versions[bounds.first].version,
      includeMin: true,
      max: bounds.last == versions.length - 1
          ? null
          : versions[bounds.last + 1].version,
      alwaysIncludeMaxPreRelease: true);
  _knownInvalidVersions = incompatibleVersions.union(_knownInvalidVersions);

  var sdkConstraint = await foldAsync(
      slice(versions, bounds.first, bounds.last + 1), VersionConstraint.empty,
      (previous, version) async {
    var pubspec = await _describeSafe(version);
    return previous.union(
        pubspec.sdkConstraints[sdk.identifier] ?? VersionConstraint.any);
  });

  return Incompatibility(
      [Term(_ref.withConstraint(incompatibleVersions), true)],
      SdkCause(sdkConstraint, sdk));
}

## Returns the first and last indices in [_versions] of the contiguous set of
## versions whose pubspecs match [match].
##
## Assumes [match] returns true for the pubspec whose version is at [index].
Future<Pair<int, int>> _findBounds(
    int start, bool Function(Pubspec) match) async {
  var versions = await _versions;

  var first = start - 1;
  while (first > 0) {
    if (!match(await _describeSafe(versions[first]))) break;
    first--;
  }

  var last = start + 1;
  while (last < versions.length) {
    if (!match(await _describeSafe(versions[last]))) break;
    last++;
  }

  return Pair(first + 1, last - 1);
}

## Returns a map where each key is a package name and each value is the upper
## or lower (according to [upper]) bound of the range of versions with an
## identical dependency to that in [dependencies], around the version at
## [index].
##
## If a package is absent from the return value, that indicates indicate that
## all versions above or below [index] (according to [upper]) have the same
## dependency.
Future<Map<String, Version>> _dependencyBounds(
    Map<String, PackageRange> dependencies, int index,
    {bool upper = true}) async {
  var versions = await _versions;
  var bounds = <String, Version>{};
  var previous = versions[index];
  outer:
  for (var id in upper
      ? versions.skip(index + 1)
      : versions.reversed.skip(versions.length - index)) {
    var pubspec = await _describeSafe(id);

    # The upper bound is exclusive and so is the first package with a
    # different dependency. The lower bound is inclusive, and so is the last
    # package with the same dependency.
    var boundary = (upper ? id : previous).version;

    # Once we hit an incompatible version, it doesn't matter whether it has
    # the same dependencies.
    for (var sdk in sdks.values) {
      if (_matchesSdkConstraint(pubspec, sdk)) continue;
      for (var name in dependencies.keys) {
        bounds.putIfAbsent(name, () => boundary);
      }
      break outer;
    }

    for (var range in dependencies.values) {
      if (bounds.containsKey(range.name)) continue;
      if (pubspec.dependencies[range.name] != range) {
        bounds[range.name] = boundary;
      }
    }

    if (bounds.length == dependencies.length) break;
    previous = id;
  }

  return bounds;
}

## Returns the pubspec for [id], or an empty pubspec matching [id] if the
## real pubspec for [id] fails to load for any reason.
##
## This makes the bounds-finding logic resilient to broken pubspecs while
## keeping the actual error handling in a central location.
Future<Pubspec> _describeSafe(PackageId id) async {
  try {
    return await withDependencyType(
        _dependencyType, () => _source.describe(id));
  } catch (_) {
    return Pubspec(id.name, version: id.version);
  }
}

## Returns whether [pubspec]'s constraint on [sdk] matches the current
## version.
bool _matchesSdkConstraint(Pubspec pubspec, Sdk sdk) {
  if (_overriddenPackages.contains(pubspec.name)) return true;

  var constraint = pubspec.sdkConstraints[sdk.identifier];
  if (constraint == null) return true;

  return sdk.isAvailable && constraint.allows(sdk.version);
}

type
  RootSource = ref object of BoundSource
    ## A fake source that contains only the root package.
    ##
    ## This only implements the subset of the [BoundSource] API that
    ## [PackageLister] uses to find information about packages.
    package*: Package ## The entrypoint package.


## An error to throw for unused source methods.
UnsupportedError get _unsupported =>
    UnsupportedError('_RootSource is not a full source.');

_RootSource(this._package);

@override
Future<List<PackageId>> getVersions(PackageRef ref, {Duration maxAge}) {
  assert(ref.isRoot);
  return Future.value([PackageId.root(_package)]);
}

@override
Future<Pubspec> describe(PackageId id) {
  assert(id.isRoot);
  return Future.value(_package.pubspec);
}

@override
Source get source => throw _unsupported;
@override
SystemCache get systemCache => throw _unsupported;
@override
Future<List<PackageId>> doGetVersions(PackageRef ref, Duration maxAge) =>
    throw _unsupported;
@override
Future<Pubspec> doDescribe(PackageId id) => throw _unsupported;
@override
String getDirectory(PackageId id, {String relativeFrom}) =>
    throw _unsupported;
