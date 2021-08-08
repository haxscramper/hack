import std/[tables, os, hashes, strformat, strutils, options]

import hmisc/core/all

import
  ./pubspec,
  ./types,
  ./utils,
  ./versions

{.this: this.}

# ## The equality to use when comparing the feature sets of two package names.
# const _featureEquality = MapEquality<string, FeatureDependency>();

proc name(self: Package): string =
  ## The name of the package.
  return self.pubspec.name

proc version(self: Package): VersionConstraint =
  ## The package's version.
  self.pubspec.version

proc isInMemory*(self: Package): bool =
  ## An in-memory package can be created for doing a resolution without having
  ## a package on disk. Paths should not be resolved for these.
  self.dir == ""

proc orderByNameAndVersion(a, b: Package): int =
  ## Compares [a] and [b] orders them by name then version number.
  ##
  ## This is normally used as a [Comparator] to pass to sort. This does not
  ## take a package's description or root directory into account, so multiple
  ## distinct packages may order the same.

  var name = cmp(a.name, b.name)
  if (name != 0): return name

  return cmp(a.version, b.version)



proc dependencies(self: Package): Table[string, PackageRange] =
  ## The immediate dependencies this package specifies in its pubspec.
  self.pubspec.dependencies

proc devDependencies(self: Package): Table[string, PackageRange] =
  ## The immediate dev dependencies this package specifies in its pubspec.
  self.pubspec.devDependencies

proc dependencyOverrides(self: Package): Table[string, PackageRange] =
  ## The dependency overrides this package specifies in its pubspec.
  return self.pubspec.dependencyOverrides

proc isRoot*(self: Package): bool =
  ## Whether this package is the root package.
  isNil(self.source)

proc isRoot*(self: PackageName): bool =
  ## Whether this package is the root package.
  isNil(self.source)

proc toRef(self: PackageName): PackageRef =
  ## Returns a [PackageRef] with this one's [name], [source], and
  ## [description].
  PackageRef(
    name: self.name,
    source: self.source,
    description: self.description
  )

proc descriptionsEqual(s: Source, a, b: string): bool = assert false

proc withConstraint*(self: PackageName, constraint: VersionConstraint): PackageRange =
  ## Returns a [PackageRange] for this package with the given version constraint.
  PackageRange(name: self.name, source: self.source,
               constraint: constraint, description: self.description)

proc samePackage*(self, other: PackageName): bool =
  ## Returns whether this refers to the same package as [other].
  ##
  ## This doesn't compare any constraint information; it's equivalent to
  ## `this.toRef() == other.toRef()`.
  if (other.name != self.name): return false
  if isNil(self.source): return isNil(other.source)

  return other.source == self.source and
    self.source.descriptionsEqual(self.description, other.description)

proc hash*(s: Source): Hash = assert false

proc hash*(self: PackageName): Hash =
  if (isNil(self.source)): return hash(self.name)
  return !$(self.name.hash !& self.source.hash !& hash(self.description))



proc rootRef*(package: Package): PackageRef =
  ## Creates a reference to the given root package.
  PackageRef(name: package.name, source: nil, description: package.name)



proc rootId*(package: Package): PackageId =
  ## Creates an ID for the given root package.
  PackageId(name: package.name, source: nil, description: package.name)


proc hash(id: PackageId): Hash =
  !$(hash(id.version) !& hash(PackageName(id)))

proc `==`(self, other: PackageId): bool =
  other of PackageId and
  self.samePackage(other) and
  other.version == self.version



proc toRange(self: PackageId): PackageRange =
  ## Returns a [PackageRange] that allows only [version] of this package.
  self.withConstraint(self.version)



proc newPackageRange*(
    name: string, source: Source,
    constraint: VersionConstraint,
    description: string,
    features: Table[string, FeatureDependency]
  ): PackageRange =
  ## A reference to a constrained range of versions of one package.
  ## Creates a reference to package with the given [name], [source],
  ## [constraint], and [description].
  ##
  ## Since an ID's description is an implementation detail of its source, this
  ## should generally not be called outside of [Source] subclasses.

  PackageRange(
    constraint: constraint,
    features: features,
    name: name,
    source: source,
    description: description
  )


proc newRootPackageRange*(package: Package): PackageRange =
  ## Creates a range that selects the root package.
  PackageRange(
    constraint: package.version,
    name: package.name,
    description: package.name
  )
# PackageRange.root(Package package)
#     : constraint = package.version,
#       features = const {},
#       super._(package.name, null, package.name);

proc featureDescription*(self: PackageRange): string =
  ## Returns a description of [features], or the empty string if [features] is
  ## empty.
  if (self.features.len == 0): return ""

  var enabledFeatures: seq[string]
  var disabledFeatures: seq[string]

  for name, `type` in self.features:
    if (`type` == FeatureDependency.unused):
      disabledFeatures.add(name)

    else:
      enabledFeatures.add(name)

  var description = "";
  if (enabledFeatures.len != 0):
    description &= "with {toSentence(enabledFeatures)}"
    if (disabledFeatures.len != 0):
      description &= ", "

  if (disabledFeatures.len != 0):
    description &= &"without {toSentence(disabledFeatures)}"

  return description

proc toString(self: PackageRange, detail: PackageDetail = nil): string =
  assert false

proc withFeatures(
    self: PackageRange,
    features: Table[string, FeatureDependency]
  ): PackageRange =

  ## Returns a new [PackageRange] with [features] merged with [this.features].
  if (features.len == 0):
    return self


  result = newPackageRange(
    self.name,
    self.source,
    self.constraint,
    self.description,
    self.features
  )

  for k, v in features:
    result.features[k] = v
  # result.features &= self.features



proc withTerseConstraint*(this: PackageRange): PackageRange =
  ## Returns a copy of [this] with the same semantics, but with a `^`-style
  ## constraint if possible.
  if not(this.constraint of vckVersionRange): return this
  if (startsWith($this.constraint[], '^')): return this

  var vRange = this.constraint
  if (not vRange.includeMin): return this
  if (vRange.includeMax): return this
  if (vRange.min.isnone()): return this

  if (
    vRange.max.get() == vRange.min.get().nextBreaking.firstPreRelease() or
    (vRange.min.get().isPreRelease() and vRange.max.get() == vRange.min.get().nextBreaking())
  ):
    return withConstraint(compatibleWith(vRange.min.get()))

  else:
    return this


proc allows*(this: PackageRange, id: PackageId): bool =
  ## Whether [id] satisfies this dependency.
  ##
  ## Specifically, whether [id] refers to the same package as [this] *and*
  ## [constraint] allows `id.version`.
  samePackage(id) and constraint.allows(id.version)

proc hash*(this: PackageRange): Hash = 
  # FIXME super.hash() !&
  !$( constraint.hash() 
  # !& featureEquality.hash(features) # FIXME
  )

proc `==`*(this, other: PackageRange): bool = 
  samePackage(other) and
  other.constraint == constraint 
  # and featureEquality.equals(other.features, features) # FIXME

## An enum of types of dependencies on a [Feature].
# class FeatureDependency {

#   static const required = FeatureDependency._('required');

#   static const ifAvailable = FeatureDependency._('if available');

#   static const unused = FeatureDependency._('unused');

#   final string _name;

#   ## Whether this type of dependency enables the feature it depends on.
#   bool get isEnabled => this != unused;

#   const FeatureDependency._(this._name);

#   @override
#   string tostring() => _name;
# }

  # const PackageDetail(
  #     {this.showVersion,
  #     bool showSource,
  #     bool showDescription,
  #     bool showFeatures})
  #     : showSource = showDescription == true ? true : showSource,
  #       showDescription = showDescription ?? false,
  #       showFeatures = showFeatures ?? true;

proc max*(self, other: PackageDetail): PackageDetail =
  ## Returns a [PackageDetail] with the maximum amount of detail between [this]
  ## and [other].
  PackageDetail(
    showVersion:     self.showVersion     or other.showVersion,
    showSource:      self.showSource      or other.showSource,
    showDescription: self.showDescription or other.showDescription,
    showFeatures:    self.showFeatures    or other.showFeatures
  )
