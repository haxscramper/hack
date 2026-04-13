from dataclasses import dataclass


@dataclass(frozen=True)
class BoxCharset:
    top_left: str
    top_right: str
    bottom_left: str
    bottom_right: str
    horizontal: str
    vertical: str
    fill: str = " "


UNICODE_CHARSET = BoxCharset(
    top_left="┌",
    top_right="┐",
    bottom_left="└",
    bottom_right="┘",
    horizontal="─",
    vertical="│",
)

ASCII_CHARSET = BoxCharset(
    top_left="+",
    top_right="+",
    bottom_left="+",
    bottom_right="+",
    horizontal="-",
    vertical="|",
)


@dataclass(frozen=True)
class LineCharset:
    horizontal: str
    vertical: str
    corner_ne: str
    corner_nw: str
    corner_se: str
    corner_sw: str


UNICODE_LINE_CHARSET = LineCharset(
    horizontal="─",
    vertical="│",
    corner_ne="└",
    corner_nw="┘",
    corner_se="┌",
    corner_sw="┐",
)

ASCII_LINE_CHARSET = LineCharset(
    horizontal="-",
    vertical="|",
    corner_ne="+",
    corner_nw="+",
    corner_se="+",
    corner_sw="+",
)


def make_single_char_box_charset(ch: str) -> BoxCharset:
    return BoxCharset(
        top_left=ch,
        top_right=ch,
        bottom_left=ch,
        bottom_right=ch,
        horizontal=ch,
        vertical=ch,
    )


def make_single_char_line_charset(ch: str) -> LineCharset:
    return LineCharset(
        horizontal=ch,
        vertical=ch,
        corner_ne=ch,
        corner_nw=ch,
        corner_se=ch,
        corner_sw=ch,
    )
