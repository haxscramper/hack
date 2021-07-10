import 'package:path/path.dart' as path
import 'package:pub_semver/pub_semver.dart'

import '../command_runner.dart'
import '../lock_file.dart'
import '../log.dart' as log
import '../package.dart'
import '../package_name.dart'
import '../source_registry.dart'
import '../system_cache.dart'
import '../utils.dart'
import 'result.dart'
import 'type.dart'

## Unlike [SolveResult], which is the static data describing a resolution,
## this class contains the mutable state used while generating the report
## itself.
##
## It's a report builder.
type
  SolveReport = object
    `type`: SolveType
    sources: SourceRegistry
    root: Package
    previousLockFile: LockFile
    result: SolveResult
    cache: SystemCache

    dependencies: Table[String, PackageId] ## The dependencies in [_result], keyed by package name.
    output: string

proc newSolveReport(this._type, this._sources, this._root, this._previousLockFile,
    this._result, this._cache) {
  # Fill the map so we can use it later.
  for (var id in result.packages) {
    dependencies[id.name] = id
  }
}

proc show() =
  ## Displays a report of the results of the version resolution relative to
  ## the previous lock file.
  reportChanges()
  reportOverrides()
}

proc summarize(dryRun: bool = false) =
  ## Displays a one-line message summarizing what changes were made (or would
  ## be made) to the lockfile.
  ##
  ## If [dryRun] is true, describes it in terms of what would be done.

  # Count how many dependencies actually changed.
  var dependencies = dependencies.keys.toSet()
  dependencies.addAll(previousLockFile.packages.keys)
  dependencies.remove(root.name)

  var numChanged = dependencies.filter((name) => (
    var oldId = previousLockFile.packages[name]
    var newId = dependencies[name]

    # Added or removed dependencies count.
    if (oldId.isNil()): return true
    if (newId.isNil()): return true

    # The dependency existed before, so see if it was modified.
    return oldId != newId
  )).len

  var suffix = ''
  if (_root.dir.isNil().not()):
    final dir = path.normalize(_root.dir)
    if (dir != '.'):
      suffix = ' in $dir'

  if (dryRun):
    if (numChanged == 0):
      log.message('No dependencies would change$suffix.')

    elif (numChanged == 1):
      log.message('Would change $numChanged dependency$suffix.')

    else:
      log.message('Would change $numChanged dependencies$suffix.')

  else:
    if (numChanged == 0):
      if (_type == SolveType.GET):
        log.message('Got dependencies$suffix!')

      else:
        log.message('No dependencies changed$suffix.')

    elif (numChanged == 1):
      log.message('Changed $numChanged dependency$suffix!')

    else:
      log.message('Changed $numChanged dependencies$suffix!')

proc reportChanges() =
  ## Displays a report of all of the previous and current dependencies and
  ## how they have changed.
  output.clear()

  # Show the new set of dependencies ordered by name.
  var names = result.packages.map((id) => id.name).toList()
  names.remove(_root.name)
  names.sort()
  for (final name in names):
    await reportPackage(name)

  # Show any removed ones.
  var removed = previousLockFile.packages.keys.toSet()
  removed.removeAll(names)
  removed.remove(_root.name) # Never consider root.
  if (removed.isNotEmpty) {
    output.writeln('These packages are no longer being depended on:')
    for (var name in ordered(removed)) {
      await reportPackage(name, alwaysShow: true)

  log.message(_output)

proc reportOverrides() =
  ## Displays a warning about the overrides currently in effect.
  output.clear()

  if (_root.dependencyOverrides.isNotEmpty):
    output.writeln('Warning: You are using these overridden dependencies:')

    for (var name in ordered(_root.dependencyOverrides.keys)):
      reportPackage(name, alwaysShow: true, highlightOverride: false)

    log.warning(_output)

proc reportDiscontinued() =
  ## Displays a single-line message, number of discontinued packages
  ## if discontinued packages are detected.
  var numDiscontinued = 0
  for id in result.packages:
    if (id.source.isNil()): continue
    final status =
        await cache.source(id.source).status(id, Duration(days: 3))
    if (status.isDiscontinued) numDiscontinued++

  if (numDiscontinued > 0):
    if (numDiscontinued == 1):
      log.message('1 package is discontinued.')

    else:
      log.message('$numDiscontinued packages are discontinued.')

## Displays a two-line message, number of outdated packages and an
## instruction to run `pub outdated` if outdated packages are detected.
void reportOutdated() {
  final outdatedPackagesCount = result.packages.where((id) {
    final versions = result.availableVersions[id.name]
    # A version is counted:
    # - if there is a newer version which is not a pre-release and current
    # version is also not a pre-release or,
    # - if the current version is pre-release then any upgraded version is
    # considered.
    return versions.any((v) =>
        v > id.version and (id.version.isPreRelease or !v.isPreRelease))
  }).length

  if (outdatedPackagesCount > 0) {
    String packageCountString
    if (outdatedPackagesCount == 1) {
      packageCountString = '1 package has'
    } else {
      packageCountString = '$outdatedPackagesCount packages have'
    }
    log.message('$packageCountString newer versions incompatible with '
        'dependency constraints.\nTry `$topLevelProgram pub outdated` for more information.')
  }
}

## Reports the results of the upgrade on the package named [name].
##
## If [alwaysShow] is true, the package is reported even if it didn't change,
## regardless of [_type]. If [highlightOverride] is true (or absent), writes
## "(override)" next to overridden packages.
Future<void> reportPackage(String name,
    {bool alwaysShow = false, bool highlightOverride = true}) async {
  var newId = dependencies[name]
  var oldId = previousLockFile.packages[name]
  var id = newId ?? oldId

  var isOverridden = root.dependencyOverrides.containsKey(id.name)

  # If the package was previously a dependency but the dependency has
  # changed in some way.
  var changed = false

  # If the dependency was added or removed.
  var addedOrRemoved = false

  # Show a one-character "icon" describing the change. They are:
  #
  #     ! The package is being overridden.
  #     - The package was removed.
  #     + The package was added.
  #     > The package was upgraded from a lower version.
  #     < The package was downgraded from a higher version.
  #     * Any other change between the old and new package.
  String icon
  if (isOverridden):
    icon = log.magenta('! ')

  elif (newId.isNil()):
    icon = log.red('- ')
    addedOrRemoved = true

  elif (oldId.isNil()):
    icon = log.green('+ ')
    addedOrRemoved = true

  elif (!oldId.samePackage(newId)):
    icon = log.cyan('* ')
    changed = true

  elif (oldId.version < newId.version):
    icon = log.green('> ')
    changed = true

  elif (oldId.version > newId.version):
    icon = log.cyan('< ')
    changed = true

  else:
    # Unchanged.
    icon = '  '

  String message
  # See if there are any newer versions of the package that we were
  # unable to upgrade to.
  if (newId.isNil().not() and type != SolveType.DOWNGRADE):
    var versions = result.availableVersions[newId.name]

    var newerStable = false
    var newerUnstable = false

    for (var version in versions):
      if (version > newId.version):
        if (version.isPreRelease):
          newerUnstable = true

        else:
          newerStable = true

    final status =
        await cache.source(id.source).status(id, Duration(days: 3))

    if (status.isDiscontinued):
      if (status.discontinuedReplacedBy.isNil()):
        message = '(discontinued)'

      else:
        message =
            '(discontinued replaced by ${status.discontinuedReplacedBy})'

    elif newerStable:
      # If there are newer stable versions, only show those.
      message = '(${maxAll(versions, Version.prioritize)} available)'

    elif newId.version.isPreRelease and newerUnstable
      # Only show newer prereleases for versions where a prerelease is
      # already chosen.
      message = '(${maxAll(versions)} available)'

  if (`type` == SolveType.GET and
      not(alwaysShow or
          changed or
          addedOrRemoved or
          message.isNil().not())) {
    return

  output.write(icon)
  output.write(log.bold(id.name))
  output.write(' ')
  writeId(id)

  # If the package was upgraded, show what it was upgraded from.
  if (changed):
    output.write(' (was ')
    writeId(oldId)
    output.write(')')

  # Highlight overridden packages.
  if (isOverridden and highlightOverride):
    output.write(" ${log.magenta('(overridden)')}")

  if (message.isNil().not()):
    output.write(' ${log.cyan(message)}')

  output.writeln()
}

## Writes a terse description of [id] (not including its name) to the output.
void writeId(PackageId id) {
  output.write(id.version)

  if (id.source != sources.defaultSource):
    var description = id.source.formatDescription(id.description)
    output.write(' from ${id.source} $description')
