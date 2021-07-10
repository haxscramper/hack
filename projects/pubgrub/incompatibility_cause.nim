import
  ./incompatibility

type
  IncompatibilityCause = enum
    ## The reason an [Incompatibility]'s terms are incompatible.

    root ## the incompatibility represents the requirement that the root
    ## package exists.

    dependency ## The incompatibility represents a package's dependency.

    useLatest ## The incompatibility represents the user's request that we
    ## use the latest version of a given package.

    noVersion ## The incompatibility indicates that the package has no
    ## versions that match the given constraint.

    unknownSource ## The incompatibility indicates that the package has an
                  ## unknown source.



  ConflictCause = object
    ## The incompatibility was derived from two existing incompatibilities during
    ## conflict resolution.

    conflict: Incompatibility ## The incompatibility that was originally
    ## found to be in conflict, from which the target incompatibility was
    ## derived.

    Incompatibility other; ## The incompatibility that caused the most
    ## recent satisfier for [conflict], from which the target
    ## incompatibility was derived.

proc newConflictCause(conflict, other: Incompatibility): ConflictCause =
  ConflictCause(conflict: conflict, other: other)



# ## A class for stateless [IncompatibilityCause]s.
# class _Cause implements IncompatibilityCause {
#   final String _name;

#   const _Cause(this._name);

#   @override
#   String toString() => _name;
# }

# ## The incompatibility represents a package's SDK constraint being
# ## incompatible with the current SDK.
# class SdkCause implements IncompatibilityCause {
#   ## The union of all the incompatible versions' constraints on the SDK.
#   final VersionConstraint constraint;

#   ## The SDK with which the package was incompatible.
#   final Sdk sdk;

#   SdkCause(this.constraint, this.sdk);
# }

# ## The incompatibility represents a package that couldn't be found by its
# ## source.
# class PackageNotFoundCause implements IncompatibilityCause {
#   ## The exception indicating why the package couldn't be found.
#   final PackageNotFoundException exception;

#   ## If the incompatibility was caused by an SDK being unavailable, this is
#   ## that SDK.
#   ##
#   ## Otherwise `null`.
#   Sdk get sdk => exception.missingSdk;

#   PackageNotFoundCause(this.exception);
# }
