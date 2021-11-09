import std/[tables, hashes, strformat]

# import hmisc/core/all

# startHax()

type
  Version = object
    major*: int
    minor*: int
    patch*: int

  PackageUsed = object of RootObj
    name*: string
    version*: Version

  PackageRequire = object of PackageUsed
    # usedFeatures*: seq[string]

  PackageFeature = object
    requires*: seq[PackageRequire]

  Package = object
    name*: string
    # rootFeature*: PackageFeature
    # features*: seq[PackageFeature]

  PackageName = distinct string

func pkg*(name: string): PackageName = PackageName(name)

func `$`*(v: Version): string = &"{v.major}.{v.minor}.{v.patch}"
func `$`*(req: PackageRequire): string = &"{req.name} {req.version}"
func `$`*(req: PackageUsed): string = &"{req.name} {req.version}"

func `>`*(name: PackageName, req: (int, int, int)): PackageRequire =
  result.name = name.string
  result.version = Version(major: req[0], minor: req[1], patch: req[2])

func `<`*(v1, v2: Version): bool =
  (v1.major < v2.major) or
  (v1.major == v2.major and v1.minor < v2.minor) or
  (v1.major == v2.major and v1.minor == v2.minor and v1.patch < v2.patch)

func toUsed*(req: PackageRequire): PackageUsed =
  PackageUsed(name: req.name, version: req.version)

func hash*(v: Version): Hash = !$(hash(v.major) !& hash(v.minor) !& hash(v.patch))
func hash*(req: PackageRequire): Hash =
  !$(hash(req.name) !& hash(req.version) # !& hash(req.usedFeatures)
  )

proc approximateDepList*(
    visited: var Table[PackageRequire, seq[PackageUsed]],
    dep: PackageRequire,
    getDeps: proc(require: PackageRequire): seq[PackageRequire]
  ): seq[PackageUsed] =

  result.add dep.toUsed()

  if dep in visited:
    result = visited[dep]

  else:
    for subDep in getDeps(dep):
      result.add approximateDepList(visited, subDep, getDeps)

  echo "approximate dep list for", dep
  echo result


proc finalDepList*(candidates: seq[PackageUsed]): seq[PackageUsed] =
  var temp: Table[string, PackageUsed]

  for candidate in candidates:
    let name = candidate.name
    if name in temp:
      let cmp = temp[name].version < candidate.version
      echo &"{name}: {temp[name].version} < {candidate.version} = {cmp}"
      if cmp:
        temp[name] = candidate

    else:
      temp[name] = candidate

  for _, used in pairs(temp):
    result.add used

proc resolveDeps*(
    root: PackageRequire,
    getDeps: proc(req: PackageRequire): seq[PackageRequire]
  ): seq[PackageUsed] =

  var visited: Table[PackageRequire, seq[PackageUsed]]
  let initial = approximateDepList(visited, root, getDeps)
  return finalDepList(initial)

let deps = toTable {
  pkg"a" > (0, 1, 0): @[ pkg"b" > (0, 1, 2), pkg"c" > (0, 1, 2) ],

  pkg"b" > (0, 1, 1): @[ pkg"d" > (0, 1, 1) ],
  pkg"b" > (0, 1, 2): @[ pkg"d" > (0, 1, 2) ],

  pkg"c" > (0, 1, 1): @[],
  pkg"c" > (0, 1, 2): @[ pkg"d" > (0, 1, 4) ],
  pkg"c" > (0, 1, 3): @[ pkg"f" > (0, 1, 1) ],

  pkg"d" > (0, 1, 1): @[ pkg"e" > (0, 1, 1) ],
  pkg"d" > (0, 1, 2): @[ pkg"e" > (0, 1, 1) ],
  pkg"d" > (0, 1, 3): @[ pkg"e" > (0, 1, 2) ],
  pkg"d" > (0, 1, 4): @[ pkg"e" > (0, 1, 2) ],

  pkg"e" > (0, 1, 1): @[],
  pkg"e" > (0, 1, 2): @[],
  pkg"e" > (0, 1, 3): @[],

  pkg"f" > (0, 1, 1): @[ pkg"g" > (0, 1, 1) ],

  pkg"g" > (0, 1, 1): @[ pkg"f" > (0, 1, 1) ]
}

proc getDeps(require: PackageRequire): seq[PackageRequire] =
  result = deps[require]
  echo "Get deps for ", require, " -> ", result


when isMainModule:
  echo "result: ", resolveDeps(pkg"a" > (0, 1, 0), getDeps)


# Why we have to know all existing version for all packages instead of just
# talking lowesta avilable in the system?
#
# If we just do this, which package is installed would depend on the state
# of the world and your local system. On the other hand, if we fetch data
# from centralized location, the only external thing that could potentially
# affect how your package works, would be someone *deleting existing
# version* entirely.

# Also MVS helps fighting abandoned software version - if it compiled
# successfully ten years ago and all deps are still up (which is not
# guaranteed, but much more likely than "all deps have no new versions")
# then it would build /exactly/ the way it did earlier.

#[

- How to gradually adopt MVS without breaking nim ecosystem?
- How to integrate package features in already existing workflow
- Communicate package features between dependency resolution and actual code
  - Can be done using `-d:package_featurename=true` in the `nim.cfg` file
    that is generated by nimble - this solution is opt-in, can be implemented
    later.

]#

#[

For hcparse I want to provide multiple APIs, some of which have large (in
terms of SLOC nearing millions of auto-generated C code) dependencies or
complicated build steps (binary requires compilation of intermediate static
library, which in turn depends on `cmake`, `g++`, `boost` begin available,
and takes several minutes to finish). If user only wants to use particular
subset of the API, it makes sense to allow `requires hcparse/[macros] >
0.1.3`

]#