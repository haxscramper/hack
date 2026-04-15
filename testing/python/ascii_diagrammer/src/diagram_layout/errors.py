"""Application-specific exceptions.

The implementation is intentionally fail-fast. Validation and resolution
errors surface directly as exceptions with descriptive messages.
"""


class DiagramError(Exception):
    """Base class for all diagram processing errors."""


class ValidationError(DiagramError):
    """Raised when expanded or semantic validation fails."""


class ResolutionError(DiagramError):
    """Raised when the constraint system cannot be resolved."""
