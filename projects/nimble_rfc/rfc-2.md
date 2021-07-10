<!-- Improve workflow when working with local packages - right now `nimble develop` interferes with regular installation and resolution etc. Ah, I know - development mode can create different set of requirements altogether, it should not be that difficult. -->

# Completely static toplevel

A little more aggressive extension of the ideas proposed in the manifest evaluation section. The idea is simple - make all package metadata static. Based on the analysis of existing package configurations, they are already written approximately `99.288%` of all configuration values are already written in 'canonical' form.

To be more specific, 'canonical' value format for different metadata fields is

| field | description | canonical form  |
| -- | -- | -- |
| `version` | Package version string | `"major.minor.patch"` |
| `author` | Author's name | `"<arbitrary string>"` |
| `description` | Short package description | `"<arbitrary string>"` |
| `packagename` | Package name | `"<string>"` |
| `license` | Package license | `"license name"` |
| `srcdir` | Source directory to take <br> installation files from | `"relative/path"` |
| `bindir` | Directory where nimble build will output binaries | `"path"` |
| `backend` | Compilation backed | One of `"c"`, `"cc"`, `"cpp"`, `"objc"`, `"js"` |
| `bin` | List of compiled binary files | `@["path1", "path2"]` |
| `skipdirs` | Directories to skip while installing | `@["path1", "path2"]` |
| `skipext` | Extension to skip while installing | `@["ext1", "ext2"]` |
| `installfiles` | List of files which should <br> be exclusively installed | `@["path1", "path2"]` |
| `installdirs` | List of directories which should <br> be exclusively installed | `@["path1", "path2"]` |
| `skipfiles` | List of file names which should be skipped <br> during installation | `@["path1", "path2"]` |
| `installext` | Extension to use while installing | `@["ext1", "ext2"]` |
| `namedbin` | Path-name mappig for binary files | `{"path": "name"}` |

Note that `namedbin` is currently declared as `Table[]`, but due to *extremely* rare usage (4 times directly assigned and two more in form of `namedBin["XXXX"] = "YYYY"`) this can be replaced with `{"string", "string"}`, even though it would break *all* uses.

Aside from package metadata might contain:

- Helper procedure declarations used in `task`, `after` and `before` hooks.
- `when` section for optional requires, foreign dependencies and assignments to some metadata fields <!--, like `when defined(windows): installExt = @["dll"] else installExt = @["so"]` -->
- `task`, `before` and `after` hooks
- imports or includes

Right now it possible to put arbitrary nimscript code at the toplevel, and it would be executed each time I want to access some package metadata. For example, if I want to know package version, and it contains code like `mkDir()` it would make me a directory, somewhere, each time.

Instead, I suggest that `.nimble` is statically rewritten into executable nimscript by nimble and then executed. It is not really different from current implementation where `.nimble` file is converted to nimscript that is [`include`s](https://github.com/nim-lang/nimble/blob/bdc9678e2211cebf843a6f470397d50077f7705f/src/nimblepkg/nimscriptwrapper.nim#L114) an [api](https://github.com/nim-lang/nimble/blob/master/src/nimblepkg/nimscriptapi.nim) file where `task` is defined as a template that [modifies](https://github.com/nim-lang/nimble/blob/bdc9678e2211cebf843a6f470397d50077f7705f/src/nimblepkg/nimscriptapi.nim#L190) global list of `nimbleTasks` only when code runs. 

```nim
template task*(name: untyped; description: string; body: untyped): untyped =
  proc `name Task`*() = body

  nimbleTasks.add (astToStr(name), description) # < This part is placed at nimscript toplevel
  
  if actionName.len == 0 or actionName == "help":
    success = true
  elif actionName == astToStr(name).normalize:
    success = true
    `name Task`()
```

`nimble` can perform file rewrite only based on AST - declare ``name Task`` procedure that would be executed when `nimble Task` is run.

# Make binary build optional

This was first discussed in context of `requires nimble` potentially overwriting active `nimble` binary with a new one, even if that's undesirable. Hybrid packages were introduced to solve the problem "I have a binary package, but I also want my API to be reusable", but at the same time it is currently not possible to request *only* API part of a package.

**IDEA**: Allow requiring only library or binary part of a package as `requires nimble/lib` or `requires nimble/bin`. Current requirement format stays the same to avoid breakages, but if user need better control over what's installed, it should be supported.


**EXAMPLE**:

- There is a package `nimble` that provides a binary and an API. API is for code reuse. These are the "features" the package offers. By "features" I do not mean it in the context of dependency resolution but simply "what stuff I can do with it" (like I can run `nimble install`, or `nimble dump` or `import nimblepkg/version`).
- I want to reuse API that developer provided, but not really interested in absolutely all features of nimble package. More specifically, I'm interested in the "library part"
- In order to avoid unnecessary costs (building, disc storage, additional dependencies, other inconveniences like packages overriding each other) I opt to explicitly communicate my intentions <!-- - **not** using `binary` and I say `binary = off`. But! Someone else might say that "no binary" is not what I want, and I should be just --> by listing subset of features I'm interested in, specifically library - i.e. `nimble/lib`, `nimble/bin` or both (simply `nimble`)

**NOTE**: That is all modeled as package features (already mentioned in context of optional dependencies). We can only do this for hybrid packages and os-specific dependencies. Latter one is automatically set by package manager - user can't set `os = windows`. Hybrid package choice is done using `nimble/lib`. 

# Provide better control over build configuration

Introduce `build` section that provides a way to give a better description of the dependencies related to particular build target. Right now, it is not possible to specify different backends for each build target, or different requirements. 

```nim
build backend:
  requires "jester"
  backend = "c"
  # optionally you can override how this gets built
  # by writing exec("nim c blah"), otherwise you get this
  # by default

build frontend:
  requires "karax"
  backend = "js"
```
