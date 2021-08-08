import std/[tables]

import
  ./assignment,
  ./failure,
  ./partial_solution,
  ./incompatibility_cause,
  ./package_lister,
  ./reformat_ranges,
  ./result,
  ./set_relation,
  ./term,
  ./versionm,
  ./incompatibility

# TODO(nweiz): Currently, a bunch of tests that use the solver are skipped
# because they exercise parts of the solver that haven't been reimplemented.
# They should all be re-enabled before this gets released.

## The version solver that finds a set of package versions that satisfy the
## root package's dependencies.
##
## See https:#github.com/dart-lang/pub/tree/master/doc/solver.md for details
## on how this solver works.

# VersionSolver(this._type, this._systemCache, this._root, this._lockFile,
#     Iterable<String> unlock)
#     : _overriddenPackages = MapKeySet(_root.pubspec.dependencyOverrides),
#       _unlock = {...unlock};

proc solve(solver: VersionSolver): SolveResult =
  ## Finds a set of dependencies that match the root package's constraints, or
  ## throws an error if no such set is available.

  solver.addIncompatibility(
    newIncompatibility([newTerm(
      rootRange(solver.root), false)], newRootIncompatibilityCause()))

  var next = solver.root.name
  while next != "":
    solve.propagate(next)
    next = solver.choosePackageVersion()


proc propagate(solve: VersionSolver, package: string) =
  ## Performs [unit propagation][] on incompatibilities transitively related to
  ## [package] to derive new assignments for [_solution].
  ##
  ## [unit propagation]: https:#github.com/dart-lang/pub/tree/master/doc/solver.md#unit-propagation
  var changed = toHashSet(package)

  while changed.len > 0:
    var package = changed.first
    changed.excl package

    # Iterate in reverse because conflict resolution tends to produce more
    # general incompatibilities as time goes on. If we look at those first,
    # we can derive stronger assignments sooner and more eagerly find
    # conflicts.
    for incompatibility in incompatibilities[package].reversed():
      var result = propagateIncompatibility(incompatibility);
      if (result == #conflict) {
        # If [incompatibility] is satisfied by [_solution], we use
        # [_resolveConflict] to determine the root cause of the conflict as a
        # new incompatibility. It also backjumps to a point in [_solution]
        # where that incompatibility will allow us to derive new assignments
        # that avoid the conflict.
        var rootCause = solver.resolveConflict(incompatibility);

        # Backjumping erases all the assignments we did at the previous
        # decision level, so we clear [changed] and refill it with the
        # newly-propagated assignment.
        changed.clear();
        changed.add(solver.propagateIncompatibility(rootCause) as String);
        break;

      else result is String:
        changed.add(result)

proc propagateIncompatibility(solver: VersionSolver, incompatibility: Incompatibility) =
  ## If [incompatibility] is [almost satisfied][] by [_solution], adds the
  ## negation of the unsatisfied term to [_solution].
  ##
  ## [almost satisfied]: https:#github.com/dart-lang/pub/tree/master/doc/solver.md#incompatibility
  ##
  ## If [incompatibility] is satisfied by [_solution], returns `#conflict`. If
  ## [incompatibility] is almost satisfied by [_solution], returns the
  ## unsatisfied term's package name. Otherwise, returns `#none`.

  # The first entry in `incompatibility.terms` that's not yet satisfied by
  # [_solution], if one exists. If we find more than one, [_solution] is
  # inconclusive for [incompatibility] and we can't deduce anything.
  var unsatisfied: Term

  for i in 0 ..< incompatibility.terms.len:
    var term = incompatibility.terms[i]
    var relation = solver.solution.relation(term)

    if (relation == SetRelation.disjoint):
      # If [term] is already contradicted by [_solution], then
      # [incompatibility] is contradicted as well and there's nothing new we
      # can deduce from it.
      return #none;
    elif (relation == SetRelation.overlapping):
      # If more than one term is inconclusive, we can't deduce anything about
      # [incompatibility].
      if not isNil(unsatisfied != null): return #none

      # If exactly one term in [incompatibility] is inconclusive, then it's
      # almost satisfied and [term] is the unsatisfied term. We can add the
      # inverse of the term to [_solution].
      unsatisfied = term

  # If *all* terms in [incompatibility] are satisfied by [_solution], then
  # [incompatibility] is satisfied and we have a conflict.
  if isNIl(unsatisfied): return #conflict;

  log(&"derived:${unsatisfied.isPositive ? ' not' : ''} '${unsatisfied.package}'");
  solution.derive(
    unsatisfied.package, not unsatisfied.isPositive, incompatibility);

  return unsatisfied.package.name

proc resolveConflict(
  solver: VersionSolver, incompatibility: Incompatibility): Incompatibility =
  ## Given an [incompatibility] that's satisfied by [_solution], [conflict
  ## resolution][] constructs a new incompatibility that encapsulates the root
  ## cause of the conflict and backtracks [_solution] until the new
  ## incompatibility will allow [_propagate] to deduce new assignments.
  ##
  ## [conflict resolution]: https:#github.com/dart-lang/pub/tree/master/doc/solver.md#conflict-resolution
  ##
  ## Adds the new incompatibility to [_incompatibilities] and returns it.

  log("${log.red(log.bold(conflict))}: $incompatibility")

  var newIncompatibility = false;
  while (not incompatibility.isFailure):
    # The term in `incompatibility.terms` that was most recently satisfied by
    # [_solution].
    var mostRecentTerm: Term

    # The earliest assignment in [_solution] such that [incompatibility] is
    # satisfied by [_solution] up to and including this assignment.
    var mostRecentSatisfier: Assignment

    # The difference between [mostRecentSatisfier] and [mostRecentTerm];
    # that is, the versions that are allowed by [mostRecentSatisfier] and not
    # by [mostRecentTerm]. This is `null` if [mostRecentSatisfier] totally
    # satisfies [mostRecentTerm].
    var difference: Term

    # The decision level of the earliest assignment in [_solution] *before*
    # [mostRecentSatisfier] such that [incompatibility] is satisfied by
    # [_solution] up to and including this assignment plus
    # [mostRecentSatisfier].
    #
    # Decision level 1 is the level where the root package was selected. It's
    # safe to go back to decision level 0, but stopping at 1 tends to produce
    # better error messages, because references to the root package end up
    # closer to the final conclusion that no solution exists.
    var previousSatisfierLevel = 1;

    for (var term in incompatibility.terms) {
      var satisfier = _solution.satisfier(term);
      if (mostRecentSatisfier == null):
        mostRecentTerm = term
        mostRecentSatisfier = satisfier

      elif (mostRecentSatisfier.index < satisfier.index):
        previousSatisfierLevel = math.max(
            previousSatisfierLevel, mostRecentSatisfier.decisionLevel)
        mostRecentTerm = term
        mostRecentSatisfier = satisfier
        difference = nil

      else:
        previousSatisfierLevel =
            math.max(previousSatisfierLevel, satisfier.decisionLevel);

      if (mostRecentTerm == term):
        # If [mostRecentSatisfier] doesn't satisfy [mostRecentTerm] on its
        # own, then the next-most-recent satisfier may be the one that
        # satisfies the remainder.
        difference = mostRecentSatisfier.difference(mostRecentTerm);
        if (difference != null) {
          previousSatisfierLevel = math.max(previousSatisfierLevel,
              _solution.satisfier(difference.inverse).decisionLevel);

    # If [mostRecentSatisfier] is the only satisfier left at its decision
    # level, or if it has no cause (indicating that it's a decision rather
    # than a derivation), then [incompatibility] is the root cause. We then
    # backjump to [previousSatisfierLevel], where [incompatibility] is
    # guaranteed to allow [_propagate] to produce more assignments.
    if (previousSatisfierLevel < mostRecentSatisfier.decisionLevel or
        mostRecentSatisfier.cause == null):
      solution.backtrack(previousSatisfierLevel)

      if (newIncompatibility): solver.addIncompatibility(incompatibility)
      return incompatibility

    # Create a new incompatibility by combining [incompatibility] with the
    # incompatibility that caused [mostRecentSatisfier] to be assigned. Doing
    # this iteratively constructs an incompatibility that's guaranteed to be
    # true (that is, we know for sure no solution will satisfy the
    # incompatibility) while also approximating the intuitive notion of the
    # "root cause" of the conflict.
    var newTerms = <Term>[
      for (var term in incompatibility.terms)
        if (term != mostRecentTerm) term,
      for (var term in mostRecentSatisfier.cause.terms)
        if (term.package != mostRecentSatisfier.package) term,
    ];

    # The [mostRecentSatisfier] may not satisfy [mostRecentTerm] on its own
    # if there are a collection of constraints on [mostRecentTerm] that
    # only satisfy it together. For example, if [mostRecentTerm] is
    # `foo ^1.0.0` and [_solution] contains `[foo >=1.0.0,
    # foo <2.0.0]`, then [mostRecentSatisfier] will be `foo <2.0.0` even
    # though it doesn't totally satisfy `foo ^1.0.0`.
    #
    # In this case, we add `not (mostRecentSatisfier \ mostRecentTerm)` to
    # the incompatibility as well, See [the algorithm documentation][] for
    # details.
    #
    # [the algorithm documentation]: https:#github.com/dart-lang/pub/tree/master/doc/solver.md#conflict-resolution
    if (difference != null) newTerms.add(difference.inverse);

    incompatibility = Incompatibility(
        newTerms, ConflictCause(incompatibility, mostRecentSatisfier.cause));
    newIncompatibility = true;

    var partially = difference == null ? '' : ' partially';
    var bang = log.red('!');
    _log('$bang $mostRecentTerm is$partially satisfied by '
        '$mostRecentSatisfier');
    _log('$bang which is caused by "${mostRecentSatisfier.cause}"');
    _log('$bang thus: $incompatibility');
  }

  throw SolveFailure(reformatRanges(_packageListers, incompatibility));
}

proc choosePackageVersion(): string =
  ## Tries to select a version of a required package.
  ##
  ## Returns the name of the package whose incompatibilities should be
  ## propagated by [_propagate], or `null` indicating that version solving is
  ## complete and a solution has been found.
  var unsatisfied = solution.unsatisfied.toList()
  if (unsatisfied.len == 0): return ""

  # If we require a package from an unknown source, add an incompatibility
  # that will force a conflict for that package.
  for candidate in unsatisfied:
    if (candidate.source is! UnknownSource) continue;
    solver.addIncompatibility(
      Incompatibility([Term(candidate.withConstraint(VersionConstraint.any), true)],
        IncompatibilityCause.unknownSource))

    return candidate.name

  # Prefer packages with as few remaining versions as possible, so that if a
  # conflict is necessary it's forced quickly.
  var package = minByAsync(
    unsatisfied,
    (package) => packageLister(package).countVersions(package.constraint)
  )

  var version: PackageId
  try:
    version = packageLister(package).bestVersion(package.constraint)

  except PackageNotFoundException as error:
    solver.addIncompatibility(
      newIncompatibility(
        [newTerm(package.withConstraint(VersionConstraint.any), true)],
        newPackageNotFoundCause(error)))

    return package.name

  if (version == null):
    # If the constraint excludes only a single version, it must have come
    # from the inverse of a lockfile's dependency. In that case, we request
    # any version instead so that the lister gives us more general
    # incompatibilities. This makes error reporting much nicer.
    if (excludesSingleVersion(package.constraint)) {
      version = packageLister(package).bestVersion(VersionConstraint.any);

    else:
      # If there are no versions that satisfy [package.constraint], add an
      # incompatibility that indicates that.
      _addIncompatibility(Incompatibility(
          [Term(package, true)], IncompatibilityCause.noVersions));
      return package.name;

  var conflict = false
  for incompatibility in packageLister(package).incompatibilitiesFor(version):
    solver.addIncompatibility(incompatibility);

    # If an incompatibility is already satisfied, then selecting [version]
    # would cause a conflict. We'll continue adding its dependencies, then go
    # back to unit propagation which will guide us to choose a better
    # version.
    conflict = conflict ||
        incompatibility.terms.every((term) =>
            term.package.name == package.name || _solution.satisfies(term));

  if (not conflict):
    solution.decide(version);
    log("selecting $version");

  return package.name;

proc addIncompatibility(self: VersionSolver, Incompatibility incompatibility) {
  ## Adds [incompatibility] to [_incompatibilities].
  log('fact: $incompatibility')
  for term in incompatibility.terms:
    incompatibilities
        .putIfAbsent(term.package.name, () => [])
        .add(incompatibility)

## Returns whether [constraint] allows all versions except one.
proc excludesSingleVersion(constraint: VersionConstraint): bool =
    VersionConstraint.any.difference(constraint) is Version

proc result(): SolveResult =
  ## Creates a [SolveResult] from the decisions in [_solution].
  var decisions = solution.decisions.toList()
  var pubspecs: Table[String, Pubspec]
  for (id in decisions):
    if (id.isRoot):
      pubspecs[id.name] = root.pubspec

    else:
      pubspecs[id.name] = systemCache.source(id.source).describe(id)

  return newSolveResult(
    systemCache.sources,
    root,
    lockFile,
    decisions,
    pubspecs,
    getAvailableVersions(decisions),
    solution.attemptedSolutions
  );

pr cgetAvailableVersions(packages: seq[PackageId]): Table[string, seq[Version]] =
  ## Generates a map containing all of the known available versions for each
  ## package in [packages].
  ##
  ## The version list may not always be complete. If the package is the root
  ## package, or if it's a package that we didn't unlock while solving because
  ## we weren't trying to upgrade it, we will just know the current version.

  var availableVersions: Table[String, seq[Version]]
  for package in packages:
    var cached = packageListers[package.toRef()].cachedVersions;
    # If the version list was never requested, use versions from cached
    # version listings if the package is "hosted".
    # TODO(sigurdm): This has a smell. The Git source should have a
    # reasonable behavior here (we should be able to call getVersions in a
    # way that doesn't fetch.
    var ids: seq[PackageId]

    try:
      ids = cached ??
          (package.source is HostedSource
              ? (await _systemCache
                  .source(package.source)
                  .getVersions(package.toRef(), maxAge: Duration(days: 3)))
              : [package]);

    except:
      ids = PackageId(package)

    availableVersions[package.name] = ids.map((id) => id.version)

  return availableVersions

proc packageLister(package: PackageName): PackageLister
  ## Returns the package lister for [package], creating it if necessary.
  var pref = package.toRef()
  return packageListers.putIfAbsent(pref, () {
    if (pref.isRoot) return PackageLister.root(_root)

    var locked = getLocked(pref.name);
    if (locked != null && !locked.samePackage(pref)) locked = null;

    var overridden = overriddenPackages
    if (overridden.contains(package.name)):
      # If the package is overridden, ignore its dependencies back onto the
      # root package.
      overridden = Set.from(overridden)..add(_root.name);

    return PackageLister(_systemCache, pref, locked,
        _root.dependencyType(package.name), overridden,
        downgrade: _type == SolveType.DOWNGRADE);

proc getLocked(String package): PackageId =
  ## Gets the version of [ref] currently locked in the lock file.
  ##
  ## Returns `null` if it isn't in the lockfile (or has been unlocked).

  if (`type` == SolveType.GET):
    if (_unlock.contains(package)):
      return null;

    return lockFile.packages[package];

  # When downgrading, we don't want to force the latest versions of
  # non-hosted packages, since they don't support multiple versions and thus
  # can't be downgraded.
  if (`type` == SolveType.DOWNGRADE):
    var locked = lockFile.packages[package];
    if (locked.isNil().not() and
        not locked.source.hasMultipleVersions): return locked

  if (unlock.isEmpty or unlock.contains(package)): return nil
  return lockFile.packages[package]

void log(message: varargs[string, `$`]) {
  ## Logs [message] in the context of the current selected packages.
  ##
  ## If [message] is omitted, just logs a description of leaf-most selection.

  # Indent for the previous selections.
  echo "  ".repeat(solution.decisionLevel), message.join(" ")
