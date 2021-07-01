# Existing problems

## Centralized package registry

I don't think it is necessary to invent something new. Just look at what others do and copy implementation, since I'm not sure that we have enough resourses to have some unique approach. And it is not really necessary. I'm not saying we should aim for fully copying implementation of other package manager, since nim has it's own set of unique features, but it is important to consider.

Most package management solutions include cenrtalized package index that keeps track of all package version, thus solving issue of findinf requirements for a particular version of the package. For example `cargo` is centralized, full information, including each package version is stored in [git repo](https://github.com/rust-lang/crates.io-index) in the form of simple json files. Right now nim employs similar solution via [nim-lang/packages](https://github.com/nim-lang/packages), but it requires someone to manually merge PR with a new package. Not really scalable solution - sometimes you might need to wait several days before package is added to the registry, but I believe this can be automated.

Having centralized package registry which records all `package+version -> dependencies` mapping is important for full dependency resolutionm. It also allows for projects like [nim package directory](https://nimble.directory/) to exist.

**IDEA**: make `nimble publish` put current package metadata in the `nim-lang/packages` index

## Current nimble dependency resolution

Current implementation of dependency resolution does not construct explicit dependency graph, and instead just [loops](https://github.com/nim-lang/nimble/blob/95e6870f60655e81ff488779c7f589fe649061ec/src/nimble.nim#L64) though requirements, almost immediately [installing](https://github.com/nim-lang/nimble/blob/95e6870f60655e81ff488779c7f589fe649061ec/src/nimble.nim#L84-L86) them which I believe to be the source of such bugs as ["nimble loops infinitely trying to install codependent requirements"](https://github.com/nim-lang/nimble/issues/887) and ["Dependency resolution depends on the order in requires"](https://github.com/nim-lang/nimble/issues/505) (could be prevented with explicit dependency graph construction).

Related links:

- [Can't install with nimble: nimterop versions](https://github.com/disruptek/golden/issues/21)  
- [Dependency resolution depends on the order in `requires`](https://github.com/nim-lang/nimble/issues/505)  
- [Installing package fails to find the dependent package even though it exists.](https://github.com/nim-lang/nimble/issues/766)
- [nimble loops infinitely trying to install codependent requirements](https://github.com/nim-lang/nimble/issues/887)   
- [Nimble download/install sequence improvement](https://forum.nim-lang.org/t/7671) - 

**IDEA**: Reimplement dependency resolution algorithm. Use full knowledge about requirements about each version of each package from previous section.

## Do not require full manifest evaluation

Right now it is necessary to fully evaluate nimscript configuration in order to determine list of dependencies, as it might contain code with complicated logic. While this might seem like an acceptable solution to 'optional' dependencies, it cannot be statically reasoned about and introduces a lot of complexity in tooling (right now nimble has to generate a `.nims` file, evaluate it separately and only then get list of dependencies. To be honest it looks and feels like a hack to me).

Right now `.nimble` configuration file contains arbitrary nimscript code that requires compiler to evaluate. This might be considered a good solution for custom `task` targets, but ultimately this leads to issues where even `author = <author name here>` might require full compiler to evaluate. Instead I propose that some important metadata like package `name` and `requires` were written in much stricter declarative manner. Specifically this means:

- `packageName = "name"` must have string literal and be located at the toplevel.
- Non-optinoal dependencies are written as `requires "dependency1", "depenendency2"` and are located at the toplevel file as well.
- Optional dependencies are written in `when defined(windows)` section. `when` section must contain static list of defines, identical to the toplevel ones. This part is particularly important as it allows to determine feature-based dependencies. Pubgrub resolution algorithms allows each package to provide a list of features that can be either enabled or disabled. This can be reused for os-specific optional dependency resolution by treating os as feature. It is automatically set or unset by `nimble` installation.

<!-- ```nim
let test = defined(windows) or defined(macos)

if defined(windows) and test:
    requires "package1", "package2"

else:
    requires "package2", "package3"
```

Instead I propose extension to the `requires`, `task` and other keywords that would actually match *intended semantics* of optional dependenices. Even with current implementation it is possible to statically parse approximately **97%** of the packages, but right now it is not possible to rely on this as `.nimble` is *not guaranteed* to support static parsing. -->

## Strict declarative subset for package manifests




Existing nimble packages **almost universally comply** with these requirements, and overwhelming number of ones that don't have simple repeating pattern violating the rule. Optional dependencies are already handled this way for most of the packages - [treeform/hottie](https://github.com/treeform/hottie/blob/master/hottie.nimble#L11), [minefuto/qwertycd](https://github.com/minefuto/qwertycd/blob/master/qwertycd.nimble#L18) and several others.

Additional restrictions might be placed of other metadata fields like `foreignDep`, `author`, `description` etc. Foreign dependencies might also be checked upon installation, but that can be implemented later.


### Current edge cases

Because `.nimble` configuration file has access to full features of the nim language users have taken advantage of this to work around certain limitations of the nimble.


In order to retrieve [`version`](https://github.com/h3rald/fae/blob/master/fae.nimble#L17) data following `import` must be resolved 
```nim
when fileExists(thisModuleFile.parentDir / "src/faepkg/config.nim"):
  # In the git repository the Nimble sources are in a ``src`` directory.
  import src/faepkg/config
else:
  # When the package is installed, the ``src`` directory disappears.
  import faepkg/config
``` 
and then vesion is set as `version       = appVersion`. This approach is used very few packages, specifically:

- [litestore](https://github.com/h3rald/litestore/blob/master/litestore.nimble), [fae](https://github.com/h3rald/fae/blob/master/fae.nimble#L17), [nifty](https://github.com/h3rald/nifty/blob/master/nifty.nimble), [nimhttpd](https://github.com/h3rald/nimhttpd/blob/master/nimhttpd.nimble), [min](https://github.com/h3rald/min/blob/master/min.nimble), [mn](https://github.com/h3rald/mn/blob/master/mn.nimble), [hastysite](https://github.com/h3rald/hastysite), [HastyScribe](https://github.com/h3rald/hastyscribe/blob/master/hastyscribe.nimble)
- [srv](https://github.com/me7/srv/blob/master/srv.nimble)
- [lilt](https://github.com/Quelklef/lilt/blob/master/lilt.nimble) - currently archived on github.
- [falas](https://github.com/brentp/falas/blob/master/falas.nimble)
- [argon2_bind](https://github.com/D-Nice/argon2_bind/blob/master/argon2_bind.nimble) - exactly the same approach. Instead `version       = vPath.staticRead.splitLines[0]` is added on top of module paths checks.


There are a couple more packages that have non-standard property configurations, like `installFiles = @[TzDbVersion & ".json"` in [timezones](https://github.com/GULPF/timezones/blob/master/timezones.nimble#L11) and `installExt = @[when defined(windows): "dll" elif` in [pvim](https://github.com/paranim/pvim/blob/master/pvim.nimble). In [nwsync](https://github.com/Beamdog/nwsync/blob/0c5ed032eb167d697bda0c9041ca435e948e8798/nwsync.nimble#L14) package `bin` field uses following code snippet
```nim
bin = listFiles(thisDir()).
  mapIt(it.extractFilename()).
  filterIt(it.startsWith("nwsync_") and it.endsWith(".nim")).
  mapIt(it.splitFile.name)
```

Currently there is no way to use **package manifest** as a single source of truth about versions, package names, author and more. `version` hacks looks like a workaround proposed in [this forum thread](https://forum.nim-lang.org/t/7231) or [reddit post](https://www.reddit.com/r/nim/comments/chcr4m/best_practice_for_embedding_nimble_package/) where in latter it is described as "messy to accomplish". It works, but I would argue that API like [std/compilesettings](https://nim-lang.org/docs/compilesettings.html) would be much better suited. It already has support for [`nimblePaths`](https://nim-lang.org/docs/compilesettings.html#MultipleValueSetting) setting. In other languages this information is exposed either via [Environment Variables](https://doc.rust-lang.org/cargo/reference/environment-variables.html), [runtime manfiest parse](https://pub.dev/documentation/pubspec_parse/latest/pubspec_parse/Pubspec/Pubspec.parse.html)


# Missing features

## Task-level and development dependencies

- https://github.com/nim-lang/nimble/issues/482
- https://forum.nim-lang.org/t/8147

## Package features

### Special defines

Package cannot advertise possible `define` that affect it's behavior. [Improving compiler's knowledge of "hidden" identifiers](https://github.com/nim-lang/RFCs/issues/337)

### Binary packages

I don't always want to build binary

# Command-line interface ergonomics

# Quality-of-life features

- https://forum.nim-lang.org/t/4648
- https://github.com/nim-lang/nimble/issues/806 make noninteractive init possible
- Current directory does not contain `.nimble` file - look up in the directory tree?
- Disable warnings (by default) from external packages when installing dependencies - I usually can't fix the source of the warnings anyway, so they are almost useless (in context of installing dependency).
- Not possible to perform `build`/`test` without dependency resolution and installation interfering (i.e. "resolve packages only using local installations")
- Several CLI flags are undocumented in `--help` (especiall `--json`)
  - Insanely noisy output
    - Shows warnigns about structure of dependencies by default. Did I ask for this? No. Can I fix broken structure? Most likely no, and certainly not now.
    - Due to dependencies being resolved and *installed* in one recursive       step it is not possible to first resolve all dependencies (possibly with cached elements) and then download everything. Package can appear multiple times in the output if it is required by more than one package, but each additional encounter will have useless 'requirement already satisfied' message.
- Support all package dependencies at once



# Existing discussions

There have been a lot of talks about possible nimble improvements on IRC, nim forum, github issues and so on.

## From https://irclogs.nim-lang.org/25-03-2019.html#17:09:17

- my top issues with nimble are:

  - a way to update installed packages without having #head #master  0.1.0 installed at the same time and giving me weird error
  - requiring nimble packages at task level (very important for tests)
  - please stop asking me to override when installing multiple projects which depends on the same package, just solve it once. It takes too much time o install nimbus even though there are only like 15 dependencies and our CI is getting very long because of nimble dependencies resolution.

the short-list we discussed was:

 - better monorepo support - host multiple projects in single repo and    version and reference accordingly (a bit like nimble develop, but  within repo) - so we can publish smaller and more reusable libs for the community without paying the multi-repo tax
 - side-by-side checkouts - different versions / branches / etc (== repro builds)
 - avoid spurios rebuilds - build times are getting significant as we grow - arguably this is nim, but the line between nim and nimble is more blurred than other langs (which tend to have a more pure compiler and a more pure build&pm tool) - same as nim, we have a growing test suite, so it should smartly run only the necessary tests - an extension of dependency tracking really
 - standard tasks & workflows for building 3rd part libs (== repro builds, but outside of nim)
    - related: due to lack of standard way of building external libraries (compile C++ code for example) and `exec()`, `doCmd()`-based workarounds things like [Nim - [...] nimble shell command injection](https://consensys.net/diligence/vulnerabilities/nim-insecure-ssl-tls-defaults-remote-code-execution/) (specifically [Remove usage of command string-based exec interfaces](https://github.com/nim-lang/nimble/issues/895)) are more likely to happen.
    - Provide a way to interface with external package managers, we already have partial support for this in the form of [external dependencies](https://github.com/nim-lang/nimble#external-dependencies) and [distros module](https://nim-lang.org/docs/distros.html) from the stdlib.
    - Due to limited nimscript features in `.nimble` (like inability to use helper procs form other dependencies) it might be necessary to resort to `exec("nim r src/actual_build_script.nim"`)
 - more declarative information, so we can build tools that analyze deps, tasks etc, for security for example
 - better dependency resolver that can deal with social information (like semver) to help work out what the deterministic part should look like (this would require community standards in the pkg repo as well)

## [Removing Nim's knowledge of Nimble](https://github.com/nim-lang/nimble/issues/654)


# Proposed solution

As I have already mentioned nim has it's unique set of features that might require a customized solutions in some places, but generally speaking it does not have any extraordinary requirements. The same can be said for various discussion that I've listed above - in most cases there are requests for 

## Pubgrub algorithm for dependency resolution

[Introduction article to the algorithm](https://nex3.medium.com/pubgrub-2fb6470504f) and much more detailed [specification](https://github.com/dart-lang/pub/blob/master/doc/solver.md). [Overview talk](https://www.youtube.com/watch?v=Fifni75xYeQ) from Dart Conf. Introductory [talk](https://www.youtube.com/watch?v=bBqErIegbiA) for Swift package manager 

Pubgrub algorithm itself has been reimplemented in several different languages and is used by multiple package managers.

- [Original dart implementation used by `pub` package manager](https://github.com/dart-lang/pub/tree/master/lib/src/source)
- [Implementation for swift package manager](https://github.com/apple/swift-package-manager/tree/main/Sources/PackageGraph/Pubgrub)
- [C++ implementation of the algorithm](https://github.com/vector-of-bool/pubgrub)
- [Rust library implementing the algorithm](https://github.com/pubgrub-rs/pubgrub) and [book](https://pubgrub-rs-guide.netlify.app/internals/intro.html) about internal implementation.
- [Ruby implementation](https://github.com/jhawthorn/pub_grub)
- Elm [implementation](https://github.com/mpizenberg/elm-pubgrub) and introduction [thread](https://discourse.elm-lang.org/t/elm-pubgrub-a-state-of-the-art-dependency-solver-in-elm/6077)
- [Python](https://github.com/ddelange/pipgrip) pip dependency resolver

I've examined dart implementation in close detail and it seems there is no need for any specialized knowledge to adopt the implementation for nim needs. The source code is [*extremely*](https://github.com/dart-lang/pub/blob/master/lib/src/solver/version_solver.dart#L119) [well](https://github.com/dart-lang/pub/blob/master/lib/src/solver/version_solver.dart#L225) documented, and paired with comprehensive [documentation](https://github.com/dart-lang/pub/blob/master/doc/solver.md). The algorithm is designed to provide extremely concise and clear error messages about failure reasons - feature that is *completely* absent from nimble right now. <!-- While it is clear that good error reportin is not of particular value here, we are getting it basically for free. -->

### Comparison with existing solutions

Compared to alternative approaches discussed in #890, and specifically libsolv pubgrub has several important differences that make it especially well-suite for the task at hand:

- Algorithm has several implementations, including two real-world uses [swift (PR link)](https://github.com/apple/swift-package-manager/pull/1918) and [dart (solver doc link)](https://github.com/dart-lang/pub/blob/master/doc/solver.md)
- Has extensive documentation on usage and implementation, whereas libsolv has to be treated like a black box for the most part. There was [no direct comparison](https://twitter.com/search?q=pubgrub%20libsolv&src=typed_query) between libsolv and pubgrub, so it is hard to provide a definitive answer which one is better.

### Features

Dart implementation of the pubgrub algorithm provides following features:

- Development dependencies
- Feature-based dependencies (right now is done using `when defined(windows)` hack)
- Lockfiles
- Semantic versioning



### 

https://github.com/nim-lang/nimble/issues/612
https://github.com/disruptek/bump

> Just considering the requires stmt, you could have:
https://github.com/nim-lang/nimble/issues/612#issuecomment-661299817

You could, not nobody does. As shown by the parsing of all existing nimble packages that are available at the moment I could say that this is not that big of a problem, and overwhelmin majority of users write `requires "packge <= version>".



------------------  TODO

https://discord.com/channels/371759389889003530/371759389889003532/859151591642497024

https://github.com/rust-lang/cargo/issues/5657

> and I've said it elsewhere, I would look into Java's maven, it works for decades now.
https://matrix.to/#/!EtGqjSRNQoJCbpCJSF:matrix.org/$16251410127549AMaUK:t2bot.io?via=matrix.org&via=t2bot.io&via=gitter.im

https://github.com/ziglang/zig/issues/943
https://research.swtch.com/vgo
https://github.com/rust-lang/cargo/issues/5657
https://blog.illicitonion.com/rust-minimum-versions-semver-is-a-lie/
> Avoid making breaking changes, libraries specify their minimum supported version of their dependencies, and the Go tooling will choose the minimum supported version of each dependency which works for the transitive set of dependencies.

https://github.com/disruptek/nimph
> shashlick's big contribution basically suggests that the pm bootstraps the build tool, which knows enough about the project/compiler to get the thing built.
> so this makes it possible for the pm to write the build program, or to re-use a packaged builder, or to just use the compiler for building (which is how nimph works).
> `[pm] -> [build tool] -> [compiler]`

> Basically merge `[pm] -> [build tool] -> [compiler]` into `[pm + compiler + build tool]`?

Remove nim compiler internal knowledge about directories like `nimblePaths` and instead streamline the workflow by having single source of thuth about package location: confguration file.

I want to specifically mention several things - I don't think there should be any additional compiler changes. Package manager should be a separate tool that does not create two-way information flow. Instead we should adopt simple model `[pm] -> [compiler]` or `[pm] -> [build tool] -> [compiler]`. Package manager either runs compiler, or configures environment (for example via `nim.cfg` with appropriate `--path`) where compiler can run. Intermediate build tools might include something like `testament` or other tooling. 

Nim configuration files support conditional checks for features in form of `@if cudnn:`, and these easily map in `defines()` in nim code. If nimble thinks that `defined(windows)` is true so does `nim` compiler itself. Feature flags can be passed using `--define:<package><feature>` flags as well.


This would allow to easily integrate `testament` with nimble? TODO could not test testament at all. 

----

If nimble implementation is fully separated from the compiler it can easily use any external dependencies. Not integrating package management in compiler opens room for futher experiments and customized solutions.

------

This can also be used to adress issues like these, because with simple configuration file I can create "nim distribution" using simple configuration files with things like `--path:distributedStdlib`. The need for hardcoding special directories and path would be reduced as well.

https://github.com/nim-lang/RFCs/issues/310
https://github.com/nim-lang/RFCs/issues/371



### Existing alternative package managers for nim

- https://github.com/disruptek/nimph
- https://github.com/c-blake/nimp
- https://github.com/h3rald/nifty
- https://github.com/Araq/nawabs