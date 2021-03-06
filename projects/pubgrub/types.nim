import std/[tables, sets, options, strformat]

type
  Feature* = object
    ## A feature declared by a package.
    ##
    ## Features are collections of optional dependencies. Dependers can
    ## choose which features to require from packages they depend on.
    name*: string ## The name of this feature.
    onByDefault*: bool ## Whether this feature is enabled by default.
    dependencies*: seq[PackageRange] ## The additional dependencies added by
                                    ## this feature.
    requires*: seq[string] ## Other features that this feature requires.

    # ## A map from SDK identifiers to this feature's constraints on those SDKs.
    # final Map<String, VersionConstraint> sdkConstraints;

  Source* = ref object of RootObj
    name*: string

  Package* = ref object
    ## A named, versioned, unit of code and resource reuse.
    dir*: string ## The path to the directory containing the package.
    pubspec*: Pubspec ## The parsed pubspec associated with this package.
    source*: Source

  Term* = ref object
    ## A statement about a package which is true or false for a given
    ## selection of package versions.
    ##
    ## See https:#github.com/dart-lang/pub/tree/master/doc/solver.md#term.

    isPositive*: bool ## Whether the term is positive or not.
      ##
      ## A positive constraint is true when a package version that matches
      ## [package] is selected a negative constraint is true when no
      ## package versions that match [package] are selected.

    package*: PackageRange ## The range of package versions referred to by
                          ## this term.

  PackageName* = ref object of RootObj
    ## The base class of [PackageRef], [PackageId], and [PackageRange].
    name*: string ## The name of the package being identified.
    description*: string
    source*: Source

  PackageRange* = ref object of PackageName
    constraint*: VersionConstraint ## The allowed package versions.
    features*: Table[string, FeatureDependency] ## The dependencies declared
    ## on features of the target package.

  PackageRef* = ref object of PackageName
    ## A reference to a [Package], but not any particular version(s) of it.

  VersionConstraint* = ref object of RootObj

  Version* = ref object of VersionConstraint

  VersionRange = ref object of VersionConstraint
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

    min*: Option[Version] ## The minimum end of the range.
    ##
    ## If [includeMin] is `true`, this will be the minimum allowed version.
    ## Otherwise, it will be the highest version below the range that is
    ## not allowed.
    ##
    ## This may be `null` in which case the range has no minimum end and allows
    ## any version less than the maximum.

    max*: Option[Version]  ## The maximum end of the range.
    ##
    ## If [includeMax] is `true`, this will be the maximum allowed version.
    ## Otherwise, it will be the lowest version above the range that is not
    ## allowed.
    ##
    ## This may be `null` in which case the range has no maximum end and
    ## allows any version greater than the minimum.

    includeMin*: bool ## If `true` then [min] is allowed by the range.
    includeMax*: bool ## If `true`, then [max] is allowed by the range.





  Pubspec* = ref object
    ## The parsed contents of a pubspec file.
    name*: string ## The package's name.
    version*: Version ## The package's version.
    dependencies*: Table[string, PackageRange] ## The additional packages
    ## this package depends on.

    devDependencies*: Table[string, PackageRange] ## The packages this
    ## package depends on when it is the root package.

    dependencyOverrides*: Table[string, PackageRange]   ## The dependency
    ## constraints that this package overrides when it is the root package.
    ##
    ## Dependencies here will replace any dependency on a package with the
    ## same name anywhere in the dependency graph.

    features*: Table[string, Feature]

  FeatureDependency* = enum
    required ## The feature must exist and be enabled for this dependency
    ## to be satisfied.

    ifAvailable ## The feature must be enabled if it exists, but is not
    ## required to exist for this dependency to be satisfied.

    unused ## The feature is neither required to exist nor to be enabled
    ## for this feature to be satisfied.

  DependencyType* {.pure.} = enum
    ## The type of dependency from one package to another.
    direct ## A dependency declared in `dependencies`.
    dev ## A dependency declared in `dev_dependencies`.
    none ## No dependency exists.

  PackageId* = ref object of PackageName
    ## A reference to a specific version of a package.
    ##
    ## A package ID contains enough information to correctly get the
    ## package.
    ##
    ## It's possible for multiple distinct package IDs to point to
    ## different packages that have identical contents. For example, the
    ## same package may be available from multiple sources. As far as Pub
    ## is concerned, those packages are different.
    ##
    ## Note that a package ID's [description] field has a different
    ## structure than the [PackageRef.description] or
    ## [PackageRange.description] fields for some sources. For example, the
    ## `git` source adds revision information to the description to ensure
    ## that the same ID always points to the same source.

    version*: Version ## The package's version.

  PackageLister* = object
    ## A cache of all the versions of a single package that provides information
    ## about those versions to the solver.

    pref*: PackageRef ## The package that is being listed.

    locked*: PackageId ## The version of this package in the lockfile.
    ##
    ## This is `null` if this package isn't locked or if the current
    ## version solve isn't a `pub get`.

    # source*: BoundSource ## The source from which [_ref] comes.

    dependencyType*: DependencyType  ## The type of the dependency from the
                                    ## root package onto [_ref].

    overriddenPackages*: HashSet[string] ## The set of package names that
                                         ## were overridden by the root
                                         ## package.

    isDowngrade*: bool ## Whether this is a downgrade, in which case the
    ## package priority should be reversed


    alreadyListedDependencies: Table[string, VersionConstraint] ## A map
    ## from dependency names to constraints indicating which versions of
    ## [_ref] have already had their dependencies on the given versions
    ## returned by [incompatibilitiesFor].
    ##
    ## This allows us to avoid returning the same incompatibilities multiple
    ## times.


    knownInvalidVersions*: VersionConstraint     ## A constraint indicating
    ## which versions of [_ref] are already known to be invalid for some
    ## reason.
    ##
    ## This allows us to avoid returning the same incompatibilities from
    ## [incompatibilitiesFor] multiple times.


    listedLockedVersion*: bool ## Whether we've returned incompatibilities
                              ## for [_locked].

    cachedVersionsour: seq[PackageId]
    versions*: seq[PackageId]
    latestVersion*: PackageId

  PackageDetail* = ref object
    ## An enum of different levels of detail that can be used when displaying a
    ## terse package name.

    # ## The default [PackageDetail] configuration.
    # static const defaults = PackageDetail();

    showVersion*: bool ## Whether to show the package version or version range.
    ##
    ## If this is `null`, the version is shown for all packages other than root
    ## [PackageId]s or [PackageRange]s with `git` or `path` sources and `any`
    ## constraints.

    showSource*: bool ## Whether to show the package source.
    ##
    ## If this is `null`, the source is shown for all non-hosted, non-root
    ## packages. It's always `true` if [showDescription] is `true`.

    showDescription*: bool ## Whether to show the package description.
    ##
    ## This defaults to `false`.

    showFeatures*: bool ## Whether to show the package features.
    ##
    ## This defaults to `true`.


proc isEnabled*(dep: FeatureDependency): bool =
  dep != unused

proc newVersionRange(
    min, max: Option[Version],
    includeMin: bool = false,
    includeMax: bool = false,
    alwaysIncludeMaxPreRelease: bool = false
  ): VersionRange =

  if (min.isSome() and max.isSome() and min.get() > max.get()):
    assert false,
      fmt"Minimum version (""{min}"") must be less than maximum (""{max}"")."

  if (not alwaysIncludeMaxPreRelease and
      not includeMax and
      max != null and
      not max.isPreRelease and
      max.build.isEmpty and
      (min == null or
          not min.isPreRelease or
          not equalsWithoutPreRelease(min, max))):
    max = max.firstPreRelease

  return VersionRange(
    min: min, max: max, includeMin: includeMin, includeMax: includeMax);
