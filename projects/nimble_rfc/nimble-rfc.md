The RFC is extremely conservative on breakages and new features - it only strives to formalize existing behavior and improve general experience of using nimble. I'm not going to propose any drastic measures as dropping versions and using git hash commits instead, or integrating package management with module system. As a result, majority of existing packages won't be affected in any way and common workflow of using nimble would stay largely the same.

# Package registry with dependency metadata

<!-- I don't think it is necessary to invent something new. Just look at what others do and copy implementation, since I'm not sure that we have enough resourses to have some unique approach. And it is not really necessary. I'm not saying we should aim for fully copying implementation of other package manager, since nim has it's own set of unique features, but it is important to consider. -->

Most package management solutions include centralized package index that keeps track of all package version, thus solving problem of finding requirements for a particular version of the package. For example, `cargo` is centralized and full information, including each package version is stored in [git repo](https://github.com/rust-lang/crates.io-index) in the form of simple json files. When new version of the package is published simple [edit](https://github.com/rust-lang/crates.io-index/commit/b5ec043c7dea843767340ec3e588fe3a009008ef) to package file is made. Right now nim employs similar solution via [nim-lang/packages](https://github.com/nim-lang/packages), but it requires someone to manually merge PR with a new package. Not really scalable solution - sometimes you might need to wait several days before package is added to the registry, but I believe this can be automated. New version is recorded as simple one-line diff that adds information about new package version, it's requirements etc.

```diff
  {"name":"package","vers":"0.4.2","deps":[{"name":"depname","req":"^1.4"}],"url": "https://gitlab.com/XXX/YYY.git"}
+ {"name":"package","vers":"0.4.3","deps":[{"name":"depname","req":"^1.4"}],"url": "https://gitlab.com/XXX/YYY.git"}
```

Having package registry which records all `package+version -> dependencies` mapping is important for full dependency resolutionm. It also allows for projects like [nim package directory](https://nimble.directory/) to exist, which help increase discoverability of different nimble projects, and analyse whole ecosystem at once (which was crucial when writing this RFC - without access to comprehensive list of packages I would not be able to provide any concrete numbers).

**IDEA**: make `nimble publish` put current package metadata in the `nim-lang/packages` index

Changes to package publishing workflow

```diff
  git commit -m "[VERSION] v1.2.3"          # Commit your changes
  git tag v1.2.3                            # tag changes
+ nimble publish                            # push new package version to registry
  git push --tags                           # upload tags to github
  git push origin master                    # upload code to github
```

**NOTE**: Convenience command like `nimble newversion` could be introduced to tag, commit and push package all at once.

# Use explicit dependency graph in Nimble

Current implementation of dependency resolution does not construct explicit dependency graph, and instead just [loops](https://github.com/nim-lang/nimble/blob/95e6870f60655e81ff488779c7f589fe649061ec/src/nimble.nim#L64) though requirements, almost immediately [installing](https://github.com/nim-lang/nimble/blob/95e6870f60655e81ff488779c7f589fe649061ec/src/nimble.nim#L84-L86) them which I believe to be the source of such bugs as ["nimble loops infinitely trying to install codependent requirements"](https://github.com/nim-lang/nimble/issues/887) and ["Dependency resolution depends on the order in requires"](https://github.com/nim-lang/nimble/issues/505) (could be prevented with explicit dependency graph construction).

Related links:

- [Can't install with nimble: nimterop versions](https://github.com/disruptek/golden/issues/21)  
- [Dependency resolution depends on the order in `requires`](https://github.com/nim-lang/nimble/issues/505)  
- [Installing package fails to find the dependent package even though it exists.](https://github.com/nim-lang/nimble/issues/766)
- [nimble loops infinitely trying to install codependent requirements](https://github.com/nim-lang/nimble/issues/887)   
- [Nimble download/install sequence improvement](https://forum.nim-lang.org/t/7671) - 


**IDEA**: Improve dependency resolution algorithm. Use full knowledge about requirements about each version of each package from previous section.

**NOTE**: Right now `nimble test` and similar tasks are always interleaved with dependency resolution and potentially package downloads. With full access to existing package metadata these steps can be simplified as only unregistered packages would have to be downloaded.

# Do not require full manifest evaluation

Right now it is necessary to fully evaluate nimscript configuration in order to determine list of dependencies, as it might contain code with complicated logic. It cannot be statically reasoned about and introduces a lot of complexity in tooling -- `nimble` has to generate a `.nims` file, evaluate it separately. It processes nim code and modifies global variables like `vesion`, then prints result in the end. Output of script execution is parsed by `nimble` and only then get list of dependencies.

Arbitrary nimscript might be considered a good solution for custom `task` targets, but ultimately this leads to issues where even `author = <author name here>` might require full compiler to evaluate.

Instead, small subset of the package manifest must be written in a declarative manner (still using nimscript syntax, but with more strict rules). Specifically this concerns `requires` and couple more metadata fields.

- `version = "version"` and `packageName = "name"` must have string literal and be located at the toplevel.
- Non-optinoal dependencies are written as `requires "dependency1", "depenendency2"` and are located at the toplevel file as well.
- Optional dependencies are written in `when defined(windows)` section. `when` section must contain static list of `requires`, identical to the toplevel ones. This part is particularly important as it allows to determine feature-based dependencies to avoid installing unnecessary packages, especially in case if they have `after install` that might fail whole installation. 


Existing `nimble` packages **almost universally comply** with these requirements, and most of ones that don't have simple repeating pattern violating the rule [import in nimble](#import-in-nimble). Optional dependencies are already handled this way for most of the packages - [treeform/hottie](https://github.com/treeform/hottie/blob/master/hottie.nimble#L11), [minefuto/qwertycd](https://github.com/minefuto/qwertycd/blob/master/qwertycd.nimble#L18) and several others.




**IDEA**: Small subset of important metadata like `requires` were written in much stricter declarative manner.

**NOTE**: `version` and `packageName` information is redundant - first one is already stored in git tags (and `nimble` actually uses *them* to fetch required package versions), and second one is optional and `"must match name specified inside <packagename>.nimble"`. The only advantage of having `version` in the `.nimble` configuration is that you don't need to shell out to `git` in order to find out package version.

**NOTE**: Additional restrictions might be placed of other metadata fields like `foreignDep`, `author`, `description` etc. Foreign dependencies might also be checked upon installation, but that can be implemented later. Almost all packages that we have today comply with the requirements:

- scope:
  - `toplevel` means top level of the manifest - not inside of any task, when etc.
  - `when` shows number of times particular metadata field was encountered in `when` section
  - `task` shows number of encounters inside of `task`
- type:
  - Canonical
    - For packges with `seq` values "canonical" way of writing is `value = @["string", "string"]`
    - Canonical for single-value variables: `value = "string"`
    - Canonical for `requires` and `foreignDep` is `requires "string", "string"`

  - ident -- metadata was set from identifier (most likely with `import common`, followed by `value = importedConst`)
  - spec -- any other way of writing value
```
                |_____toplevel______||_______when________||_______task________|
                  canon  ident   spec  canon  ident   spec  canon  ident   spec
namedbin......        4      0      0      1      0      0      0      0      0
foreigndep....        9      0     10     92      0      0      0      0      0
backend.......       25      0      0      0      0      0      0      0      0
installfiles..       35      0      0      0      0      0      0      0      0
skipext.......       37      0      0      0      0      0      0      0      0
installdirs...       57      0      0      2      0      0      0      0      0
skipfiles.....       68      0      0      0      0      0      0      0      0
bindir........       71      0      0      3      0      0      0      0      1
installext....      112      0      0      1      0      0      0      0      0
packagename...      114      0      0      0      0      0      0      0      0
bin...........      362      0      0      0      0      0      0      0      2
skipdirs......      409      0      0      0      0      0      0      0      0
srcdir........     1145      0      0      0      0      0      0      0      1
version.......     1736      0      0      0     15      0      0      0      3
description...     1745      0      0      0      9      0      0      0      1
author........     1748      0      0      0      9      0      0      0      0
license.......     1753      0      0      0      2      0      0      0      0
requires......     2677      0     17     44      0      0      0      0     43
```

- Also, it seems like `namedbin` is almost never used.
- Almost all uses of `foreignDep` happen inside of `when` section



<!-- ```nim
let test = defined(windows) or defined(macos)

if defined(windows) and test:
    requires "package1", "package2"

else:
    requires "package2", "package3"
```

Instead I propose extension to the `requires`, `task` and other keywords that would actually match *intended semantics* of optional dependenices. Even with current implementation it is possible to statically parse approximately **97%** of the packages, but right now it is not possible to rely on this as `.nimble` is *not guaranteed* to support static parsing. -->


# Streamline nimble-tooling interaction.

<!-- I want to specifically mention several things - I don't think there should be any additional compiler changes.  -->
Package manager should be a separate tool that does not create two-way information flow. Instead we should adopt simple model `[pm] -> [compiler]` or `[pm] -> [build tool] -> [compiler]`. Package manager either runs compiler, or configures environment where compiler can run. Intermediate build tools might include something like `testament` or other tooling. We already have a pretty nice configuration format in form of `nim.cfg` that would allow `nimble` to inform compiler of all the necessary configuration values. 

Having volatile configuration file would make it easier to inspect how `nimble` called the compiler, and even though n linked issue suggest that "If the experience becomes seamless then the user really won't need to care about what `Nimble` does.", in practice it is quite hard to run `nim` compiler the same way `nimble` does it - the only option is to wait for compilation to fail and then copy error message that contains the command itself, and I don't believe we would be able to make this seamless enough so nobody would *ever* need to run compiler manually or check how `nimble` communicates with compiler.

Another *very* important advantage - `nim.cfg` has is support for external tooling that `nimble` can't interface with right now. For example `testament` - if someone wishes to use it for testing their projects simple `exec("testament all")` *usually* does the right thing in CI, but under the hood it knows nothing about actual project requirements and simply relies on `--nimblePaths`. I can't make `nimble test` use my own compiler build, nor is it possible to fully integrate external tooling in the project. For `haxdoc` I basically had to copy-paste dependency resolution part of `nimble`, remove package installation parts and then work based on that. 

At the same having to run `nimble setup` each time after changing `.nimble` file could become annoying pretty fast, so old commands like `nimble build` and `nimble test` should not be deprecated. The change is mostly make following workflows synced with each other, and make it as easy as *sometimes* running single command.

Counter-[argument](https://github.com/nim-lang/nimble/issues/654#issuecomment-529206716) to this approach is

> > Most Rust programmers don't invoke rustc directly, but instead do it through Cargo.

> There's no explanation for why this approach is brilliant and greatly improves the user experience. Today I can do `nim c hello.nim` and it simply works. I don't need any `cmake`, `configure` or `.nimble` if I want to use `nimble` packages.

> I tried using Rust and it was ridiculous that I couldn't do anything without learning Cargo. Same with many other languages where there's this whole ecosystem of random tools that hide away the compiler. Meanwhile in Go, the package manager and build system are first class citizens.

We can leave `nimblePaths` as they are now, and you would be able to use nim compiler as it is now - out of sync with actual package requirements, but if you don't care thats fine. The fix is one `nimble setup` away, so once you need it, you can configure everything very quickly. Any other tool can emulate this behavior as well (like `testament`).

<!-- Integrating package manager and compiler seems like a pretty drastic change to me. It closes ways for further experiments - once added stuff like this can't be removed, which means it is a one-way road. In addition - keeping things separate allows for range of worflows, including ones that are based on submodule, or don't use `nimble` at all. -->

This approach enables range of workflows

- `[nimble build] -> [nim c]` -- `nimble` updates environment in which nim compiler would operate and then executes it. Simply shorthand for `nimble sync` followed by `nim c src/main.nim`
- `[nim c]` -- nim compiler is launched as a standalone tool - it can read already existing environment configuration and work the same way as if it was launched directly by nimble.
- `<custom tool>` -- custom tools have full access to `nim.cfg` and could easily work the same way as if they were launched by `nimble` **without having to provide explicit support** for that feature.
- `[other PM] -> [nim c]` -- if someone wants to manage their environment using different tools, or even manually (for example using git submodules), it should be possible to write `nim.cfg` by hand (or using some helper tool). Submodule-based workflow is not really different from package-based one.

`nim.cfg` correctly sets environment for all subdirectories it is located in, which means paths are correctly set up for subdirectories, tests, other projects that you might want to develop in the same repository, dependencies and so on.

Package manager can *edit* `nim.cfg` to modify volalile configuration elements like `--path`.

<!-- This would allow to easily integrate `testament` with nimble? TODO could not test testament at all. 

----

If nimble implementation is fully separated from the compiler it can easily use any external dependencies. Not integrating package management in compiler opens room for futher experiments and customized solutions.

---


Nim configuration files support conditional checks for features in form of `@if cudnn:`, and these easily map in `defines()` in nim code. If nimble thinks that `defined(windows)` is true so does `nim` compiler itself. Feature flags can be passed using `--define:<package><feature>` flags as well.

------

This can also be used to adress issues like these, because with simple configuration file I can create "nim distribution" using simple configuration files with things like `--path:distributedStdlib`. The need for hardcoding special directories and path would be reduced as well.

https://github.com/nim-lang/RFCs/issues/310
https://github.com/nim-lang/RFCs/issues/371

------
 -->


<!-- ----


Can I use my own compiler build with nimble? Or maybe some other tool? Like `testament`, ro `nim doc`, or any other core tool. What about external tooling?  -->

# Dependency resolution

Dependency resolution for nimble have been discussed multiple times in different contexts, specifically in ["modern techniques for dependency resolution"](https://github.com/nim-lang/nimble/issues/890). Possible options for solving this problem that were mentioned:

- Use existing dependency solver like [libsolv](https://github.com/openSUSE/libsolv) -- provides low-level features to implement dependency resolution on top. Would require additional effort to design integration of the nimble
- Implement custom solution using SAT solver like z3 -- requires a lot of additional work and research to implement.
- Allow [multiple versions](https://research.swtch.com/version-sat) of the same package -- would require custom complier support, and this must be a last resort approach, not a go-to fix that is enabled each time dependency resolution halts for some unknown reason.

All of these options are used in certain package managers, and with some effort they might be reimplemented for nim as well. But, while looking for existing solutions that could be easily adopted I've found one that seems to be suited especially well for the problem at hand -- [PubGrub: Next-Generation Version Solving](https://nex3.medium.com/pubgrub-2fb6470504f) by Natalie Weizenbaum. The article introduces new dependency resolution *algorithm* called pubgrub. It has already been adopted by `dart` and `swift` package managers, and have several reimplementations in other languages. They were both mentioned in the linked [article](https://research.swtch.com/version-sat) suggested in `#890`, which was written in 2016 and mentions both of these package managers, with comments on their currend dependency solver implementation. "[Dart's](https://pub.dartlang.org/) pub includes a [backtracking solver](https://github.com/dart-lang/pub/blob/b48babe56e9800a53ecce888482cb758f6815bd5/lib/src/solver/backtracking_solver.dart) that [often takes a long time](https://github.com/dart-lang/pub/issues/912).", "[Swift's package manager](https://github.com/apple/swift-package-manager) uses a [basic backtracking solver](https://github.com/apple/swift-package-manager/blob/master/Sources/PackageGraph/DependencyResolver.swift#L518)." 

Basics of the algorithm are explained in the [introduction article to the algorithm](https://nex3.medium.com/pubgrub-2fb6470504f) and much more detailed [specification](https://github.com/dart-lang/pub/blob/master/doc/solver.md). [Overview talk](https://www.youtube.com/watch?v=Fifni75xYeQ) from Dart Conf by the algorithm author and introductory [talk](https://www.youtube.com/watch?v=bBqErIegbiA) for Swift package manager. Implementations in different programming languages:

- [Original dart implementation used by `pub` package manager](https://github.com/dart-lang/pub/tree/master/lib/src/source)
- [Implementation for swift package manager](https://github.com/apple/swift-package-manager/tree/main/Sources/PackageGraph/Pubgrub)
- [C++ implementation of the algorithm](https://github.com/vector-of-bool/pubgrub)
- [Rust library implementing the algorithm](https://github.com/pubgrub-rs/pubgrub) and [book](https://pubgrub-rs-guide.netlify.app/internals/intro.html) about internal implementation.
- [Ruby implementation](https://github.com/jhawthorn/pub_grub)
- Elm [implementation](https://github.com/mpizenberg/elm-pubgrub) and introduction [thread](https://discourse.elm-lang.org/t/elm-pubgrub-a-state-of-the-art-dependency-solver-in-elm/6077)
- [Python](https://github.com/ddelange/pipgrip) pip dependency resolver

I've examined dart implementation in close detail and it seems there is no need for any specialized knowledge (compared to `libsolv` and especially `z3` approach) to adopt the implementation for nim needs. The source code is [*extremely*](https://github.com/dart-lang/pub/blob/master/lib/src/solver/version_solver.dart#L119) [well](https://github.com/dart-lang/pub/blob/master/lib/src/solver/version_solver.dart#L225) documented, and paired with comprehensive [documentation](https://github.com/dart-lang/pub/blob/master/doc/solver.md), for both algorithm and user-side behavior. The algorithm is designed to provide extremely concise and clear error messages about failure reasons. <!-- While it is clear that good error reportin is not of particular value here, we are getting it basically for free. -->

Compared to alternative approaches discussed in [#890](https://github.com/nim-lang/nimble/issues/890), and specifically libsolv pubgrub has several important differences that make it especially well-suite for the task at hand:

- Algorithm has several implementations, including two real-world uses [swift (PR link)](https://github.com/apple/swift-package-manager/pull/1918) and [dart (solver doc link)](https://github.com/dart-lang/pub/blob/master/doc/solver.md) that show integration with features like lockfiles, semantic versioning, package features and more.
- Has extensive documentation on usage and implementation, whereas libsolv has to be treated like a black box for the most part. There was [no direct comparison](https://twitter.com/search?q=pubgrub%20libsolv&src=typed_query) between libsolv and pubgrub, so it is hard to provide a definitive answer which one is better.
- Provides implementation of features like lockfiles, development dependencies, semantic versioning, feature-based dependency resolution (`when defined(windows)`) and special attention to error messages.
- allows each package to provide a list of features that can be either enabled or disabled. This can be reused for os-specific optional dependency resolution by treating os as feature. It is automatically set or unset by `nimble` installation.

<!-- ## Features

Dart implementation of the pubgrub algorithm provides following features:

- Development dependencies
- Feature-based dependencies (right now is done using `when defined(windows)` hack)
- Lockfiles
- Semantic versioning
- Improved error message reporting -->

**IDEA**: adopt pubgrub algorithm for solving nimble dependency graph.

**NOTE**: With support for full package dependency graph it might be possible to improve current implementation a little more, and introducing such major change in the implementation should be carefully considered. If full dependency graph is introduced as proposed in the previous sections it might become less of an issue. 

# Quality-of-life features

In addition to changes directly related to package management, some quality-of-life improvements can be made.


## End user

- [Update all installed nimble packages](https://forum.nim-lang.org/t/4648)
- [make noninteractive init possible](https://github.com/nim-lang/nimble/issues/806) 
- Current directory does not contain `.nimble` file - look up in the directory tree?
- Disable warnings (by default) from external packages when installing dependencies - I usually can't fix the source of the warnings anyway, so they are almost useless (in context of installing dependency).
- Not possible to perform `build`/`test` without dependency resolution and installation interfering. This becomes a non-issue if `nimble` is used to manage volatile configuration file - `nim c` simply does the right thing.
- Several CLI flags are undocumented in `--help` (especially `--json`)
- Very noisy output that repeats information about package resolution each time it is encountered in the dependency graph (for haxdoc I have one dependency printed 33 (!) times). Some of this might be important to uneunderstand *why* package installation failed, but some of this information can certainly be reduced.
- Allow disabling binary build - I don't always want to build binary for hybrid packages.
- Create `LICENSE` file in project when choosing license - otherwise 
- Additional heuristics where `git` binary is used - if it fails for some reason (even unrelated to the original query), `nimble` falls back to `hg` and prints quite unhelpful error message `'hg' not in PATH`.
    <!-- - Shows warnigns about structure of dependencies by default. Did I ask for this? No. Can I fix broken structure? Most likely no, and certainly not now.
    - Due to dependencies being resolved and *installed* in one recursive step it is not possible to first resolve all dependencies (possibly with cached elements) and then download everything. Package can appear multiple times in the output if it is required by more than one package, but each additional encounter will have useless 'requirement already satisfied' message.
  - Support all package dependencies at once -->

## Developers

Nimble already provides some api in form of `nimblepkg` package, but it does not provide full features of the nimble itself. For example I had to copy dependency resolution code and remove download/install parts for it in order to be able to correctly compile documentation for the whole project in haxdoc. Ideally most of the internal API for package handling should be available as a library. Also, this might help with testing internal implementation details such as package resolution. 

This would also allow to freely experiment with alternative package managers that are fully interoperable with nimble - largely because they share the same core implementation and only differ in small details, like dependency resolution algorithm (we can postpone any changes in the nimble dependency resolution core, and if someone wants they can try out pubgrub in proof-of-concept package manager to see if this is really worth it). 
<!-- This also adresses concerns of community split over package manager by ensuring basics stay the same for everyone (man) -->


# Adopting changes

Most existing packages won't have any breakages. For few ones that used certain patterns listed below much easier (and cleaner) solution is provided.

Note: before we get comments like [this](https://github.com/nim-lang/nimble/issues/612#issuecomment-661299817) that start mentioning all possible ways of writing `requires` I want to say that, as it turns out, people right now *do indeed* write `requires "<string literal>"` almost all the time, so it is not an issue that we are facing right now. Out of `2781` requires processed I've found that `2738` can already be consireded 'canonical', and ones that don't are written as `requires: "str lit"`

<!-- NOTE: volatile `.cfg` file generated by nimble that contains all the necessary information like author, version and so on.
NOTE: Also write documentation on intended workflows with nimble or something -->

In some packages in order to retrieve [`version`](https://github.com/h3rald/fae/blob/master/fae.nimble#L17) data following `import` must be resolved 
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

Since `0.11.0` nimble [defines](https://github.com/nim-lang/nimble/blob/686555cd347badfe6c48c4ac49afade1ef8409ad/changelog.markdown#0110---22092019) `NimblePkgVersion` flag - we can simply put it in the `nim.cfg` so other tools could pick it up. 

<!-- Currently there is no way to use **package manifest** as a single source of truth about versions, package names, author and more. `version` hacks looks like a workaround proposed in [this forum thread](https://forum.nim-lang.org/t/7231) or [reddit post](https://www.reddit.com/r/nim/comments/chcr4m/best_practice_for_embedding_nimble_package/) where in latter it is described as "messy to accomplish". It works, but I would argue that API based on `--define:<Packagename>Minor` in the `nim.cfg` and `const <Package>Minor {.intdefine.}` in the code, the same way as `NimMajor` is handled right now. No special extensions to the compiler API are necessary. In other languages this information is exposed either via [Environment Variables](https://doc.rust-lang.org/cargo/reference/environment-variables.html) or [runtime manfiest parse](https://pub.dev/documentation/pubspec_parse/latest/pubspec_parse/Pubspec/Pubspec.parse.html). Again, no additional changes to compiler implementation are necessary, all required mechanisms are already in place. -->

# New features

## Task-level dependencies

First requested in the github [issue](https://github.com/nim-lang/nimble/issues/482) by mratsim. Separate [implementation](https://forum.nim-lang.org/t/8147). 

`task` section might contain `requies` or `when .. requires` sections. When task is executed nimble creates new requirement lists, write resolved dependencies in `nim.cfg` and executes body. All calls to external tools via `exec("nim doc")` operate as expected. `before` section is treated as part of the body itself. Special tasks like `test` are not different in any way.

## Using minimal version for package resolution

Provide an option to consider *minimal* allowed dependency option for `requires` range rather than *maximal* one. This would allow developer to keep package requirements in check by enforcing correct minimal version ranges. It mostly solves "just works" problem keeps the devs honest about their requirements and the user happy. This would benefit package ecosystem as a whole.

Example of the problem this would allow to solve:

- For example, the developer has `dep >= 0.1.0` as a version constraint.
- Then developer updated their software to rely on features for `dep 0.1.3`, but didn't update requirement since `0.1.3 >= 0.1.0`
- The end user, now with a software that pins the version of `dep` to `0.1.1`, couldn't use this deps because it won't compile,and there is no way package manager could've prevented this.
- If original developer had package manager to select *minimal* matching version (in our case that would be `0.1.0`) the problem wouldn't exist as they just fixed correct requirement range.

This idea was suggested in the go [article](https://research.swtch.com/vgo-mvs), and later considered by [rust](https://github.com/rust-lang/cargo/issues/5657), [zig](https://github.com/ziglang/zig/issues/943), [conan](https://github.com/conan-io/conan/issues/7357).

<!-- https://about.sourcegraph.com/blog/the-pain-that-minimal-version-selection-solves/ -->

As of now almost all package `requires` are potentially subject to this issue (to some degree).

```
verLater        (   > V    ): 44
verEarlier      (   < V    ): 13
verEqLater      (   >= V   ): 2712
verEqEarlier    (   <= V   ): 3
verIntersect    (> V & < V ): 71
verEq           (    V     ): 45
verAny          (    *     ): 635
verSpecial      (  #head   ): 100
```

We already run CI for important packages to make sure there is no compiler regressions. Check like this could be added to futher improve ecosystem health in the long run.

## Third-party libraries and foreign dependencies

 <!-- - standard tasks & workflows for building 3rd part libs (== repro builds, but outside of nim) -->

Due to lack of standard way of building external libraries (compile C++ code for example) and `exec()`, `doCmd()`-based workarounds things like [Nim - [...] nimble shell command injection](https://consensys.net/diligence/vulnerabilities/nim-insecure-ssl-tls-defaults-remote-code-execution/) (specifically [Remove usage of command string-based exec interfaces](https://github.com/nim-lang/nimble/issues/895)) are more likely to happen.

<!-- Provide a way to interface with external package managers, we already have partial support for this in the form of [external dependencies](https://github.com/nim-lang/nimble#external-dependencies) and [distros module](https://nim-lang.org/docs/distros.html) from the stdlib. -->

It is not possible to engineer a way to properly interface with every existing build system for C++ there is, but it providing better and more secure (at least easier to audit) convenience API for calling external program in form of `runCmd(cmd: string{lit}, args: varargs[string])` and deprecate (remove?) usages of `exec`. I would that would help avoid different shell quoting issues as well.

I'm pretty sure there is a lot of people who are not entirely thrilled by the idea of executing code with OS sideeffects just to get version number or find out package requirements list. Declarative subset of the manifest would allow to skip package evaluation and just simply read configuration. Right now it is not possible to disable `exec`, `staticExec` or `execCmd` execution in `nimscript`. 

List of most commonly used `exec` commands - approximately 16% contain some form of string concatenation, and quite a few others build string externally. Overall it seems like on average almost every package calls to `exec` in one way or another.

```
`exec` has been used 2345 times in 2013 packages
out of which 379 commands contained &
nim        1392
nimble     304
git        59
rm         55
$cmd       41 # let cmd = "some string"; exec(cmd)
mkdir      26
testament  26
cd         25
release-it 24
echo       22
mv         20
$fmt       19
node       19
cmd        14
make       14
true       13
demo       13
docker     13
```

# Recap

- changed
  - Define strict declarative subset of manifest file that would include `name`, `version` and `requires` metainformation.
  - Store full information about package dependencies and all vesion in the nim package index.
  - Consider replacing current nimble dependency resolution algorithm with pubgrub.
  - Make nimble generate `nim.cfg` instead of calling it call `nim` directly. Configuration file contains all path for dependencies and additional information.
  - Based on my analysis proposed changes would not affect most of the packages in any way - they are already fully compilant with the specification. Few packages that rely on `import` to store version in separate file do this only as a a workaround.
- added
  - add `nimble setup` command that would update package configuration file.
  - add `nimble newversion [--major] [--minor]` to automatically tag and commit changes.

# Extra

Random facts

```
Processed 2013 packages via pnode in 5.329
Total commit count 203328
`exec` has been used 2345 times in 2013 packages
Total package release count 7706
```

Total number of commits per day in all packages at the time of analysis.

![image](https://user-images.githubusercontent.com/20562256/125093484-028a2680-e0db-11eb-9c41-ff705dd04fea.png)



<!-- Due to limited nimscript features in `.nimble` (like inability to use helper procs form other dependencies) it might be necessary to resort to `exec("nim r src/actual_build_script.nim"`) -->




<!-- ## Modifying dependencies -->


<!-- 
## Task-level and development dependencies

- https://github.com/nim-lang/nimble/issues/482
- https://forum.nim-lang.org/t/8147

 -->
<!-- 

https://github.com/nim-lang/Nim/issues/8219

# ---------------------------------------
# --------------------------------------- -->

<!-- ## Package features

### Special defines

Package cannot advertise possible `define` that affect it's behavior. [Improving compiler's knowledge of "hidden" identifiers](https://github.com/nim-lang/RFCs/issues/337) -->
<!-- 
### Binary packages



# Command-line interface ergonomics -->

<!-- # Existing discussions

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
 - more declarative information, so we can build tools that analyze deps, tasks etc, for security for example
 - better dependency resolver that can deal with social information (like semver) to help work out what the deterministic part should look like (this would require community standards in the pkg repo as well)

##  -->

<!-- 
# Proposed solution

As I have already mentioned nim has it's unique set of features that might require a customized solutions in some places, but generally speaking it does not have any extraordinary requirements. The same can be said for various discussion that I've listed above - in most cases there are requests for 

 -->
<!-- 
### 

https://github.com/nim-lang/nimble/issues/612
https://github.com/disruptek/bump





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

<!-- https://github.com/disruptek/nimph
> shashlick's big contribution basically suggests that the pm bootstraps the build tool, which knows enough about the project/compiler to get the thing built.
> so this makes it possible for the pm to write the build program, or to re-use a packaged builder, or to just use the compiler for building (which is how nimph works).
> `[pm] -> [build tool] -> [compiler]`

> Basically merge `[pm] -> [build tool] -> [compiler]` into `[pm + compiler + build tool]`?


 -->

<!-- 
### Existing alternative package managers for nim

- https://github.com/disruptek/nimph
- https://github.com/c-blake/nimp
- https://github.com/h3rald/nifty
- https://github.com/Araq/nawabs --> 


