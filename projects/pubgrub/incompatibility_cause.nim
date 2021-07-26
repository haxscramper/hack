


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
