import hashlib
import json
import logging
from beartype.typing import Literal, Annotated, Sequence, Union
from pydantic import BaseModel, Field, field_serializer, field_validator
import enum

from index_service.services.core.types import IndexDocument, IndexEdge, IndexMultiDocument

log = logging.getLogger(__name__)


class DocumentLink(IndexEdge, extra="forbid"):
    order: int
    relation: Literal["nested"] = "nested"


TextAlignment = Literal["left", "center", "right", "justify"]


class Markup(str, enum.Enum):
    BOLD = "bold"
    ITALIC = "italic"
    UNDERLINE = "underline"
    STRIKE = "strike"
    CODE = "code"
    MATH = "math"
    RAW = "raw"


class TextStyles(BaseModel, extra="forbid"):
    markup: set[Markup] = set()
    textColor: str | None = None
    backgroundColor: str | None = None

    @field_serializer("markup")
    def serialize_markup(self, value: set[Markup]) -> list[str]:
        return sorted(item.value for item in value)

    @field_validator("markup", mode="before")
    @classmethod
    def deserialize_markup(cls, value):
        if isinstance(value, list):
            return {Markup(item) for item in value}
        return value

    def extend(self, value: Markup) -> "TextStyles":
        return TextStyles().model_copy(update=dict(markup=self.markup | {value}))


class StyledText(BaseModel, extra="forbid"):
    type: Literal["text"] = "text"
    text: str
    styles: TextStyles = Field(default_factory=TextStyles)


class Link(BaseModel, extra="forbid"):
    type: Literal["link"] = "link"
    href: str
    content: list[StyledText] = Field(default_factory=list)


InlineContent = Annotated[Union[StyledText, Link], Field(discriminator="type")]


class CaptionProps(BaseModel, extra="forbid"):
    caption: str | None = None


class DefaultProps(CaptionProps, extra="forbid"):
    textColor: str = "default"
    backgroundColor: str = "default"
    textAlignment: TextAlignment = "left"


class HeadingProps(DefaultProps):
    level: Literal[1, 2, 3] = 1


class CodeBlockProps(CaptionProps, extra="forbid"):
    language: str = "text"


class DivProps(CaptionProps, extra="forbid"):
    identifier: str = ""
    classes: list[str] = Field(default_factory=list)
    attributes: dict[str, str] = Field(default_factory=dict)


class DocumentBlock(IndexMultiDocument, extra="forbid"):
    nested: Sequence["DocumentBlock"] = Field(default_factory=list)


class File(DocumentBlock):
    type: Literal["file"] = "file"


class Document(DocumentBlock):
    type: Literal["document"] = "document"


class Paragraph(DocumentBlock):
    type: Literal["paragraph"] = "paragraph"
    props: DefaultProps = Field(default_factory=DefaultProps)
    content: list[InlineContent] = Field(default_factory=list)


class Math(DocumentBlock):
    type: Literal["math"] = "math"
    content: list[InlineContent] = Field(default_factory=list)


class Heading(DocumentBlock):
    type: Literal["heading"] = "heading"
    props: HeadingProps = Field(default_factory=HeadingProps)
    content: list[InlineContent] = Field(default_factory=list)


class Code(DocumentBlock):
    type: Literal["codeBlock"] = "codeBlock"
    props: CodeBlockProps = Field(default_factory=CodeBlockProps)
    content: list[StyledText] = Field(default_factory=list)


class RawBlock(DocumentBlock, extra="forbid"):
    type: Literal["rawBlock"] = "rawBlock"
    content: str
    lang: str


class BulletListItem(DocumentBlock):
    type: Literal["bulletListItem"] = "bulletListItem"
    props: DefaultProps = Field(default_factory=DefaultProps)
    content: list[InlineContent] = Field(default_factory=list)


class NumberedListItem(DocumentBlock):
    type: Literal["numberedListItem"] = "numberedListItem"
    props: DefaultProps = Field(default_factory=DefaultProps)
    content: list[InlineContent] = Field(default_factory=list)


class Quote(DocumentBlock):
    type: Literal["quote"] = "quote"
    props: DefaultProps = Field(default_factory=DefaultProps)
    content: list[InlineContent] = Field(default_factory=list)


class Div(DocumentBlock):
    type: Literal["div"] = "div"
    props: DivProps = Field(default_factory=DivProps)


# add near existing schema definitions


class TableCellProps(DefaultProps, extra="forbid"):
    rowSpan: int = 1
    colSpan: int = 1
    isHeader: bool = False


class TableCell(DocumentBlock, extra="forbid"):
    type: Literal["tableCell"] = "tableCell"
    props: TableCellProps = Field(default_factory=TableCellProps)
    content: list[InlineContent] = Field(default_factory=list)


class TableRow(DocumentBlock, extra="forbid"):
    type: Literal["tableRow"] = "tableRow"


class TableProps(CaptionProps, extra="forbid"):
    pass


class Table(DocumentBlock):
    type: Literal["table"] = "table"
    props: TableProps = Field(default_factory=TableProps)


for _cls in (
        Document,
        Paragraph,
        Heading,
        Code,
        BulletListItem,
        NumberedListItem,
        Quote,
        Div,
        Table,
):
    _cls.model_rebuild()


def build(
        cls: type[DocumentBlock],
        *,
        file_hash: str,
        nested: Sequence[DocumentBlock] = (),
        **kwargs,
) -> DocumentBlock:
    """Construct a block with its hash computed inline from its own payload
    plus the hashes of already-built nested children."""
    nested = list(nested)
    block = cls.model_construct(hash="", file_hash=file_hash, nested=nested, **kwargs)
    payload = block.model_dump(exclude={"id", "nested", "hash"})
    hasher = hashlib.sha256()
    hasher.update(json.dumps(payload, sort_keys=True).encode())
    for n in nested:
        hasher.update(n.hash.encode())
    block.hash = hasher.hexdigest()
    return block


def merge_text(nodes: list[InlineContent]) -> list[InlineContent]:
    merged: list[InlineContent] = []
    for node in nodes:
        if (isinstance(node, StyledText) and merged and
                isinstance(merged[-1], StyledText) and merged[-1].styles == node.styles):
            merged[-1] = StyledText(
                text=merged[-1].text + node.text,
                styles=node.styles,
            )
        else:
            merged.append(node)
    return merged


def _flatten(
    block: DocumentBlock,
    documents: list[IndexDocument],
    links: list[IndexEdge],
    parent_hash: str | None,
    order: int,
) -> None:
    documents.append(block.model_copy(update={"nested": []}))
    if parent_hash is not None:
        links.append(
            DocumentLink(
                from_=parent_hash,
                to_=block.hash,
                order=order,
                file_hash=block.file_hash,
            ))

    for i, nested in enumerate(block.nested):
        _flatten(nested, documents, links, block.hash, i)
