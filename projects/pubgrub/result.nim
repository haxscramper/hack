import 
  ./package

## The result of a successful version resolution.
type
  SolveResult = ref object
    packages: seq[PackageId]   ## The list of concrete package versions
    ## that were selected for each package reachable from the root.

    root: Package ## The root package of this resolution.

    pubspecs: Table[string, Pubspec] ## A map from package names to the
    ## pubspecs for the versions of those packages that were installed.


    availableVersions: Table[string, seq[Version]]   ## The available
    ## versions of all selected packages from their source.
    ##
    ## An entry here may not include the full list of versions available if the
    ## given package was locked and did not need to be unlocked during the solve.


    attemptedSolutions: int## The number of solutions that were attempted
    ## before either finding a successful solution or exhausting all
    ## options.
    ##
    ## In other words, one more than the number of times it had to backtrack
    ## because it found an invalid solution.

    sources: SourceRegistry
    previousLockFile: LockFile


proc lockFile(this: SolveResult): LockFile =
  ## The [LockFile] representing the packages selected by this version
  ## resolution.

  # Don't factor in overridden dependencies' SDK constraints, because we'll
  # accept those packages even if their constraints don't match.
  var nonOverrides = pubspecs.values
      .where(
          (pubspec) => !_root.dependencyOverrides.containsKey(pubspec.name))
      .toList();

  var sdkConstraints = <String, VersionConstraint>{};
  for (var pubspec in nonOverrides) {
    pubspec.sdkConstraints.forEach((identifier, constraint) {
      sdkConstraints[identifier] = constraint
          .intersect(sdkConstraints[identifier] ?? VersionConstraint.any);
    });
  }

  return newLockFile(
    packages,
    sdkConstraints = sdkConstraints,
    mainDependencies = MapKeySet(_root.dependencies),
    devDependencies = MapKeySet(_root.devDependencies),
    overriddenDependencies = MapKeySet(_root.dependencyOverrides))

## Returns the names of all packages that were changed.
##
## This includes packages that were added or removed.
Set<String> get changedPackages {
  if (packages == null) return null;

  var changed = packages
      .where((id) => _previousLockFile.packages[id.name] != id)
      .map((id) => id.name)
      .toSet();

  return changed.union(_previousLockFile.packages.keys
      .where((package) => !availableVersions.containsKey(package))
      .toSet());
}

proc newSolveResult(
    _sources,
    _root,
    _previousLockFile,
    packages,
    pubspecs,
    availableVersions,
    attemptedSolutions
  ): SolveResult =

  SolveResult(
    _sources: _sources,
    _root: _root,
    _previousLockFile: _previousLockFile,
    packages: packages,
    pubspecs: pubspecs,
    availableVersions: availableVersions,
    attemptedSolution: attemptedSolutions
  )



proc showReport(stype: SolveType, cache: SystemCache) =
  ## Displays a report of what changes were made to the lockfile.
  ##
  ## [type] is the type of version resolution that was run.

  newSolveReport(
    stype, _sources, _root, _previousLockFile, this, cache).show()

proc summarizeChanges(stype: SolveType, cache: SystemCache, dryRun: bool = false):
  ##
  ##Displays a one-line message summarizing what changes were made (or
  ## would be made) to the lockfile.
  ##
  ## If [type] is `SolveType.UPGRADE` it also shows the number of packages
  ## that are not at the latest available version.
  ##
  ## [type] is the type of version resolution that was run.

  let report = newSolveReport(
    stype, _sources, _root, _previousLockFile, this, cache);

  report.summarize(dryRun: dryRun);
  if (stype == SolveType.UPGRADE):
    report.reportDiscontinued()
    report.reportOutdated()

proc `$`(this: SolveResult): string =
 &Took $attemptedSolutions tries to resolve to\n
    '- ${packages.join("\n- ")};
