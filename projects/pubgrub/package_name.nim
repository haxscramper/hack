import std/[tables, os, hashes, strformat]

import
  ./pubspec,
  ./types,
  ./utils

# ## The equality to use when comparing the feature sets of two package names.
# const _featureEquality = MapEquality<string, FeatureDependency>();

proc name(self: Package): string =
  ## The name of the package.
  return self.pubspec.name

proc version(self: Package): Version =
  ## The package's version.
  self.pubspec.version

proc `<`*(v1, v2: Version): bool = assert false

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


# get immediateDependencies(self: Package) {
#   ## All immediate dependencies this package specifies.
#   ##
#   ## This includes regular, dev dependencies, and overrides.
#   # Make sure to add overrides last so they replace normal dependencies.
#   return {}
#     ..addAll(dependencies)
#     ..addAll(devDependencies)
#     ..addAll(dependencyOverrides);
# }

  # ## Returns a list of asset ids for all Dart executables in this package's bin
  # ## directory.
  # List<string> get executablePaths(self: Package) {
  #   return ordered(listFiles(beneath: 'bin', recursive: false))
  #       .where((executable) => p.extension(executable) == '.dart')
  #       .map((executable) => p.relative(executable, from: dir))
  #       .toList();
  # }

  # List<string> get executableNames(self: Package) =>
  #     executablePaths.map(p.basenameWithoutExtension).toList();

  # ## Returns the path to the README file at the root of the entrypoint, or null
  # ## if no README file is found.
  # ##
  # ## If multiple READMEs are found, this uses the same conventions as
  # ## pub.dartlang.org for choosing the primary one: the README with the fewest
  # ## extensions that is lexically ordered first is chosen.
  # string get readmePath(self: Package) {
  #   var readmes = listFiles(recursive: false)
  #       .map(p.basename)
  #       .where((entry) => entry.contains(_readmeRegexp));
  #   if (readmes.isEmpty) return null;

  #   return p.join(dir, readmes.reduce((readme1, readme2) {
  #     var extensions1 = '.'.allMatches(readme1).length;
  #     var extensions2 = '.'.allMatches(readme2).length;
  #     var comparison = extensions1.compareTo(extensions2);
  #     if (comparison == 0) comparison = readme1.compareTo(readme2);
  #     return (comparison <= 0) ? readme1 : readme2;
  #   }));
  # }

  # ## Returns the path to the CHANGELOG file at the root of the entrypoint, or
  # ## null if no CHANGELOG file is found.
  # string get changelogPath(self: Package) {
  #   return listFiles(recursive: false).firstWhere(
  #       (entry) => p.basename(entry).contains(_changelogRegexp),
  #       orElse: () => null);
  # }

  # ## Returns whether or not this package is in a Git repo.
  # bool get inGitRepo {
  #   if (_inGitRepoCache != null) return _inGitRepoCache;

  #   if (dir == null || !git.isInstalled) {
  #     _inGitRepoCache = false;
  #   } else {
  #     # If the entire package directory is ignored, don't consider it part of a
  #     # git repo. `git check-ignore` will return a status code of 0 for
  #     # ignored, 1 for not ignored, and 128 for not a Git repo.
  #     var result = runProcessSync(git.command, ['check-ignore', '--quiet', '.'],
  #         workingDir: dir);
  #     _inGitRepoCache = result.exitCode == 1;
  #   }

  #   return _inGitRepoCache;
  # }

  # bool _inGitRepoCache;

  # ## Loads the package whose root directory is [packageDir].
  # ##
  # ## [name] is the expected name of that package (e.g. the name given in the
  # ## dependency), or `null` if the package being loaded is the entrypoint
  # ## package.
  # Package.load(string name, this.dir, SourceRegistry sources)
  #     : pubspec = Pubspec.load(dir, sources, expectedName: name);

  # ## Constructs a package with the given pubspec.
  # ##
  # ## The package will have no directory associated with it.
  # Package.inMemory(this.pubspec) : dir = null;

  # ## Creates a package with [pubspec] located at [dir].
  # Package(this.pubspec, this.dir);

  # ## Given a relative path within this package, returns its absolute path.
  # ##
  # ## This is similar to `p.join(dir, part1, ...)`, except that subclasses may
  # ## override it to report that certain paths exist elsewhere than within
  # ## [dir].
  # string path(string part1,
  #     [string part2,
  #     string part3,
  #     string part4,
  #     string part5,
  #     string part6,
  #     string part7]) {
  #   if (_isInMemory) {
  #     throw StateError("Package $name is in-memory and doesn't have paths "
  #         'on disk.');
  #   }
  #   return p.join(dir, part1, part2, part3, part4, part5, part6, part7);
  # }

#   ## Given an absolute path within this package (such as that returned by
#   ## [path] or [listFiles]), returns it relative to the package root.
#   string relative(string path) {
#     if (dir == null) {
#       throw StateError("Package $name is in-memory and doesn't have paths "
#           'on disk.');
#     }
#     return p.relative(path, from: dir);
#   }

#   ## Returns the type of dependency from this package onto [name].
#   DependencyType dependencyType(string name) {
#     if (pubspec.fields['dependencies']?.containsKey(name) ?? false) {
#       return DependencyType.direct;
#     } else if (pubspec.fields['dev_dependencies']?.containsKey(name) ?? false) {
#       return DependencyType.dev;
#     } else {
#       return DependencyType.none;
#     }
#   }

#   static final _basicIgnoreRules = [
#     '.*', # Don't include dot-files.
#     '!.htaccess', # Include .htaccess anyways.
#     # TODO(sigurdm): consider removing this. `packages` folders are not used
#     # anymore.
#     'packages/',
#     'pubspec.lock',
#     '!pubspec.lock/', # We allow a directory called pubspec lock.
#   ];

#   ## Returns a list of files that are considered to be part of this package.
#   ##
#   ## If [beneath] is passed, this will only return files beneath that path,
#   ## which is expected to be relative to the package's root directory. If
#   ## [recursive] is true, this will return all files beneath that path;
#   ## otherwise, it will only return files one level beneath it.
#   ##
#   ## This will take .pubignore and .gitignore files into account. For each
#   ## directory a .pubignore takes precedence over a .gitignore.
#   ##
#   ## Note that the returned paths won't always be beneath [dir]. To safely
#   ## convert them to paths relative to the package root, use [relative].
#   List<string> listFiles({string beneath, bool recursive = true}) {
#     # An in-memory package has no files.
#     if (dir == null) return [];
#     beneath = beneath == null ? '.' : p.toUri(p.normalize(beneath)).path;
#     string resolve(string path) {
#       if (Platform.isWindows) {
#         return p.joinAll([dir, ...p.posix.split(path)]);
#       }
#       return p.join(dir, path);
#     }

#     return Ignore.listFiles(
#       beneath: beneath,
#       listDir: (dir) {
#         var contents = Directory(resolve(dir)).listSync();
#         if (!recursive) {
#           contents = contents.where((entity) => entity is! Directory).toList();
#         }
#         return contents.map((entity) {
#           if (linkExists(entity.path)) {
#             final target = Link(entity.path).targetSync();
#             if (dirExists(entity.path)) {
#               throw DataException(
#                   '''Pub does not support publishing packages with directory symlinks: `${entity.path}`.''');
#             }
#             if (!fileExists(entity.path)) {
#               throw DataException(
#                   '''Pub does not support publishing packages with non-resolving symlink: `${entity.path}` => `$target`.''');
#             }
#           }
#           final relative = p.relative(entity.path, from: this.dir);
#           if (Platform.isWindows) {
#             return p.posix.joinAll(p.split(relative));
#           }
#           return relative;
#         });
#       },
#       ignoreForDir: (dir) {
#         final pubIgnore = resolve('$dir/.pubignore');
#         final gitIgnore = resolve('$dir/.gitignore');
#         final ignoreFile = fileExists(pubIgnore)
#             ? pubIgnore
#             : (fileExists(gitIgnore) ? gitIgnore : null);

#         final rules = [
#           if (dir == '.') ..._basicIgnoreRules,
#           if (ignoreFile != null) readTextFile(ignoreFile),
#         ];
#         return rules.isEmpty
#             ? null
#             : Ignore(
#                 rules,
#                 onInvalidPattern: (pattern, exception) {
#                   log.warning(
#                       '$ignoreFile had invalid pattern $pattern. ${exception.message}');
#                 },
#                 # Ignore case on MacOs and Windows, because `git clone` and
#                 # `git init` will set `core.ignoreCase = true` in the local
#                 # local `.git/config` file for the repository.
#                 #
#                 # So on Windows and MacOS most users will have case-insensitive
#                 # behavior with `.gitignore`, hence, it seems reasonable to do
#                 # the same when we interpret `.gitignore` and `.pubignore`.
#                 #
#                 # There are cases where a user may have case-sensitive behavior
#                 # with `.gitignore` on Windows and MacOS:
#                 #
#                 #  (A) The user has manually overwritten the repository
#                 #      configuration setting `core.ignoreCase = false`.
#                 #
#                 #  (B) The git-clone or git-init command that create the
#                 #      repository did not deem `core.ignoreCase = true` to be
#                 #      appropriate. Documentation for [git-config]][1] implies
#                 #      this might depend on whether or not the filesystem is
#                 #      case sensitive:
#                 #      > If true, this option enables various workarounds to
#                 #      > enable Git to work better on filesystems that are not
#                 #      > case sensitive, like FAT.
#                 #      > ...
#                 #      > The default is false, except git-clone[1] or
#                 #      > git-init[1] will probe and set core.ignoreCase true
#                 #      > if appropriate when the repository is created.
#                 #
#                 # In either case, it seems likely that users on Windows and
#                 # MacOS will prefer case-insensitive matching. We specifically
#                 # know that some tooling will generate `.PDB` files instead of
#                 # `.pdb`, see: [#3003][2]
#                 #
#                 # [1]: https://git-scm.com/docs/git-config/2.14.6#Documentation/git-config.txt-coreignoreCase
#                 # [2]: https://github.com/dart-lang/pub/issues/3003
#                 ignoreCase: Platform.isMacOS || Platform.isWindows,
#               );
#       },
#       isDir: (dir) => dirExists(resolve(dir)),
#     ).map(resolve).toList();
#   }
# }


# type

  #   ## The [Source] used to look up this package.
  #   ##
  #   ## If this is a root package, this will be `null`.
  # final Source source;

  # ## The metadata used by the package's [source] to identify and locate it.
  # ##
  # ## It contains whatever [Source]-specific data it needs to be able to get
  # ## the package. For example, the description of a git sourced package might
  # ## by the URL "git:#github.com/dart/uilib.git".
  # final dynamic description;

proc isRoot(self: Package): bool =
  ## Whether this package is the root package.
  isNil(self.source)



  # PackageName._(this.name, this.source, this.description);

proc toRef(self: PackageName): PackageRef =
  ## Returns a [PackageRef] with this one's [name], [source], and
  ## [description].
  PackageRef(
    name: self.name,
    source: self.source,
    description: self.description
  )

proc descriptionsEqual(s: Source, a, b: string): bool = assert false

proc withConstraint(self: PackageName, constraint: VersionConstraint): PackageRange =
  ## Returns a [PackageRange] for this package with the given version constraint.
  PackageRange(name: self.name, source: self.source,
               constraint: constraint, description: self.description)

proc samePackage(self, other: PackageName): bool =
  ## Returns whether this refers to the same package as [other].
  ##
  ## This doesn't compare any constraint information; it's equivalent to
  ## `this.toRef() == other.toRef()`.
  if (other.name != self.name): return false
  if isNil(self.source): return isNil(other.source)

  return other.source == self.source and
    self.source.descriptionsEqual(self.description, other.description)

proc hash(s: Source): Hash = assert false

proc hash(self: PackageName): Hash =
  if (isNil(self.source)): return hash(self.name)
  return !$(self.name.hash !& self.source.hash !& hash(self.description))



  # ## Creates a reference to a package with the given [name], [source], and
  # ## [description].
  # ##
  # ## Since an ID's description is an implementation detail of its source, this
  # ## should generally not be called outside of [Source] subclasses. A reference
  # ## can be obtained from a user-supplied description using [Source.parseRef].
  # PackageRef(string name, Source source, description)
  #     : super._(name, source, description);

proc rootRef*(package: Package): PackageRef =
  ## Creates a reference to the given root package.
  PackageRef(name: package.name, source: nil, description: package.name)
  # PackageRef.root(Package package) : super._(package.name, null, package.name);

#   @override
#   string tostring([PackageDetail detail]) {
#     detail ??= PackageDetail.defaults;
#     if (isRoot) return name;

#     var buffer = stringBuffer(name);
#     if (detail.showSource ?? source is! HostedSource) {
#       buffer.write(' from $source');
#       if (detail.showDescription) {
#         buffer.write(' ${source.formatDescription(description)}');
#       }
#     }

#     return buffer.tostring();
#   }

#   @override
#   bool operator ==(other) => other is PackageRef && samePackage(other);
# }

# proc
#   ## Creates an ID for a package with the given [name], [source], [version],
#   ## and [description].
#   ##
#   ## Since an ID's description is an implementation detail of its source, this
#   ## should generally not be called outside of [Source] subclasses.
#   PackageId(string name, Source source, this.version, description)
#       : super._(name, source, description);

proc rootId*(package: Package): PackageId =
  PackageId(name: package.name, source: nil, description: package.name)
  # ## Creates an ID for the given root package.
  # PackageId.root(Package package)
  #     : version = package.version,
  #       super._(package.name, null, package.name);

proc hash(v: Version): Hash = assert false

proc hash(id: PackageId): Hash =
  !$(hash(id.version) !& hash(PackageName(id)))
  # @override
  # int get hashCode => super.hashCode ^ version.hashCode;

proc `==`(self, other: PackageId): bool =
  other of PackageId and
  self.samePackage(other) and
  other.version == self.version


# @override
# bool operator ==(other) =>
#     other is PackageId && samePackage(other) && other.version == version;

proc toRange(self: PackageId): PackageRange =
  ## Returns a [PackageRange] that allows only [version] of this package.
  self.withConstraint(self.version)
# PackageRange toRange() => withConstraint(version);

# @override
# string tostring([PackageDetail detail]) {
#   detail ??= PackageDetail.defaults;

#   var buffer = stringBuffer(name);
#   if (detail.showVersion ?? !isRoot) buffer.write(' $version');

#   if (!isRoot && (detail.showSource ?? source is! HostedSource)) {
#     buffer.write(' from $source');
#     if (detail.showDescription) {
#       buffer.write(' ${source.formatDescription(description)}');
#     }
#   }

#   return buffer.tostring();


## A reference to a constrained range of versions of one package.
## Creates a reference to package with the given [name], [source],
## [constraint], and [description].
##
## Since an ID's description is an implementation detail of its source, this
## should generally not be called outside of [Source] subclasses.
proc newPackageRange(
    name: string, source: Source,
    constraint: VersionConstraint,
    description: string,
    features: Table[string, FeatureDependency]
  ): PackageRange =

  PackageRange(
    constraint: constraint,
    features: features,
    name: name,
    source: source,
    description: description
  )
    # {Map<string, FeatureDependency> features})
    # : features = features == null
    #       ? const {}
    #       : UnmodifiableMapView(Map.from(features)),
    #   super._(name, source, description);

proc newRootPackageRange(package: Package): PackageRange =
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

proc featureDescription(self: PackageRange): string =
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



proc withTerseConstraint(self: PackageRange): PackageRange =
  ## Returns a copy of [this] with the same semantics, but with a `^`-style
  ## constraint if possible.
  if not(self.constraint of VersionRange): return this
  if (self.constraint.tostring().startsWith('^')): return this

  var vRange = self.constraint.VersionRange()
  if (not vRange.includeMin): return this
  if (vRange.includeMax): return this
  if (vRange.min.isSome()): return this

  if (
    vRange.max == vRange.min.nextBreaking.firstPreRelease or
    (vRange.min.isPreRelease && vRange.max == vRange.min.nextBreaking)):
    return withConstraint(VersionConstraint.compatibleWith(vRange.min))

  else:
    return this

## Whether [id] satisfies this dependency.
##
## Specifically, whether [id] refers to the same package as [this] *and*
## [constraint] allows `id.version`.
bool allows(PackageId id) => samePackage(id) && constraint.allows(id.version);

@override
int get hashCode =>
    super.hashCode ^ constraint.hashCode ^ _featureEquality.hash(features);

@override
bool operator ==(other) =>
    other is PackageRange &&
    samePackage(other) &&
    other.constraint == constraint &&
    _featureEquality.equals(other.features, features);

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
