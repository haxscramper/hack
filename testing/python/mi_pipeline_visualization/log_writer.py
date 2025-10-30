import structlog
import logging

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

if not logger.handlers:
    handler = logging.StreamHandler()
    handler.setLevel(logging.DEBUG)
    logger.addHandler(handler)

def format_callsite(logger, method_name, event_dict):
    if "filename" in event_dict and "lineno" in event_dict:
        event_dict[
            "location"] = f" {event_dict['filename']}:{event_dict['lineno']}"
        del event_dict["filename"]
        del event_dict["lineno"]
        if "func_name" in event_dict:
            del event_dict["func_name"]
    return event_dict


structlog.configure(
    processors=[
        structlog.stdlib.filter_by_level, structlog.stdlib.add_logger_name,
        structlog.stdlib.add_log_level,
        structlog.stdlib.PositionalArgumentsFormatter(),
        structlog.processors.TimeStamper(fmt="iso"),
        structlog.processors.StackInfoRenderer(),
        structlog.processors.CallsiteParameterAdder(parameters=[
            structlog.processors.CallsiteParameter.FILENAME,
            structlog.processors.CallsiteParameter.LINENO,
            structlog.processors.CallsiteParameter.FUNC_NAME
        ]), format_callsite,
        structlog.dev.ConsoleRenderer(
            colors=True,
            exception_formatter=structlog.dev.RichTracebackFormatter(width=-1),
            force_colors=True,
        ),
    ],
    wrapper_class=structlog.stdlib.BoundLogger,
    logger_factory=structlog.stdlib.LoggerFactory(),
    cache_logger_on_first_use=True,
)

log: structlog.PrintLogger = structlog.get_logger(__name__)
