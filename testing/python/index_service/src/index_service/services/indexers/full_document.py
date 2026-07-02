import hashlib
import json
import logging
from pathlib import Path
from typing import Annotated, Literal, Union

from beartype.typing import ClassVar, Sequence
from plumbum.cmd import pandoc
from pydantic import BaseModel, Field

from index_service.services.core.job_types import BaseIndexer, RunContext
from index_service.services.core.types import (
    IndexDocument,
    IndexerOutput,
    IndexerRequest,
    IndexLink,
    MultiDocumentModel,
)

AnyModel = BaseModel

log = logging.getLogger(__name__)


class NestedLinkMeta(BaseModel, extra="forbid"):
    order: int
    relation: Literal["nested"] = "nested"


TextAlignment = Literal["left", "center", "right", "justify"]


class TextStyles(BaseModel, extra="forbid"):
    bold: bool | None = None
    italic: bool | None = None
    underline: bool | None = None
    strike: bool | None = None
    code: bool | None = None
    textColor: str | None = None
    backgroundColor: str | None = None


class StyledText(BaseModel, extra="forbid"):
    type: Literal["text"] = "text"
    text: str
    styles: TextStyles = Field(default_factory=TextStyles)


class Link(BaseModel, extra="forbid"):
    type: Literal["link"] = "link"
    href: str
    content: list[StyledText] = Field(default_factory=list)


InlineContent = Annotated[Union[StyledText, Link], Field(discriminator="type")]


class DefaultProps(BaseModel, extra="forbid"):
    textColor: str = "default"
    backgroundColor: str = "default"
    textAlignment: TextAlignment = "left"


class HeadingProps(DefaultProps):
    level: Literal[1, 2, 3] = 1


class CodeBlockProps(BaseModel, extra="forbid"):
    language: str = "text"


class DocumentBlock(IndexDocument, extra="forbid"):
    nested: Sequence["DocumentBlock"] = Field(default_factory=list)


class Document(DocumentBlock):
    type: Literal["document"] = "document"


class Paragraph(DocumentBlock):
    type: Literal["paragraph"] = "paragraph"
    props: DefaultProps = Field(default_factory=DefaultProps)
    content: list[InlineContent] = Field(default_factory=list)


class Heading(DocumentBlock):
    type: Literal["heading"] = "heading"
    props: HeadingProps = Field(default_factory=HeadingProps)
    content: list[InlineContent] = Field(default_factory=list)


class Code(DocumentBlock):
    type: Literal["codeBlock"] = "codeBlock"
    props: CodeBlockProps = Field(default_factory=CodeBlockProps)
    content: list[StyledText] = Field(default_factory=list)


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


for _cls in (
        Document,
        Paragraph,
        Heading,
        Code,
        BulletListItem,
        NumberedListItem,
        Quote,
):
    _cls.model_rebuild()


def _merge_text(nodes: list) -> list:
    """Collapse adjacent StyledText runs sharing identical styles."""
    merged: list = []
    for node in nodes:
        if (isinstance(node, StyledText) and merged
                and isinstance(merged[-1], StyledText)
                and merged[-1].styles == node.styles):
            merged[-1] = StyledText(text=merged[-1].text + node.text,
                                    styles=node.styles)
        else:
            merged.append(node)
    return merged


def _convert_inlines(inlines: list,
                     styles: dict | None = None) -> list[StyledText]:
    styles = styles or {}
    out: list = []
    for il in inlines:
        t = il["t"]
        if t == "Str":
            out.append(StyledText(text=il["c"], styles=TextStyles(**styles)))
        elif t == "Space":
            out.append(StyledText(text=" ", styles=TextStyles(**styles)))
        elif t in ("SoftBreak", "LineBreak"):
            out.append(StyledText(text="\n", styles=TextStyles(**styles)))
        elif t == "Emph":
            out += _convert_inlines(il["c"], {**styles, "italic": True})
        elif t == "Strong":
            out += _convert_inlines(il["c"], {**styles, "bold": True})
        elif t == "Strikeout":
            out += _convert_inlines(il["c"], {**styles, "strike": True})
        elif t == "Underline":
            out += _convert_inlines(il["c"], {**styles, "underline": True})
        elif t == "Code":
            out.append(
                StyledText(text=il["c"][1],
                           styles=TextStyles(**{
                               **styles, "code": True
                           })))
        elif t == "Link":
            _, link_inlines, target = il["c"]
            out.append(
                Link(href=target[0], content=_convert_inlines(link_inlines)))
        elif t in ("Superscript", "Subscript", "SmallCaps", "Span", "Quoted"):
            payload = il["c"]
            nested = payload[-1] if isinstance(payload, list) else payload
            out += _convert_inlines(nested, styles)
    return _merge_text(out)


def _convert_list_items(items: list,
                        item_type: str) -> Sequence[DocumentBlock]:
    """Each pandoc list item is a list of blocks; the first becomes the item
    content, remaining blocks (including nested lists) become nested."""
    cls = {
        "bulletListItem": BulletListItem,
        "numberedListItem": NumberedListItem,
    }[item_type]
    result = []
    for item in items:
        blocks: list = []
        for pb in item:
            blocks += _convert_block(pb)
        if not blocks:
            continue
        head = blocks[0]
        content = getattr(head, "content", [])
        result.append(cls(content=content, nested=blocks[1:]))
    return result


def _convert_block(pb: dict) -> Sequence[DocumentBlock]:
    t = pb["t"]
    match t:
        case "Para" | "Plain":
            return [Paragraph(content=_convert_inlines(pb["c"]))]

        case "Header":
            level, _attr, inlines = pb["c"]
            return [
                Heading(
                    props=HeadingProps(level=min(max(level, 1), 3)),
                    content=_convert_inlines(inlines),
                )
            ]

        case "CodeBlock":
            attr, code = pb["c"]
            classes = attr[1]
            lang = classes[0] if classes else "text"
            return [
                Code(
                    props=CodeBlockProps(language=lang),
                    content=[StyledText(text=code)],
                )
            ]

        case "BulletList":
            return _convert_list_items(pb["c"], "bulletListItem")

        case "OrderedList":
            return _convert_list_items(pb["c"][1], "numberedListItem")

        case "BlockQuote":
            nested: list = []
            for inner in pb["c"]:
                nested += _convert_block(inner)
            return [Quote(nested=nested)]

        case "HorizontalRule":
            return []

        case _:
            log.warning(f"Implicitly handled document block of type '{t}'")
            return [Paragraph(content=_convert_inlines(pb.get("c", [])))]


def pandoc_to_document(path: Path) -> Document:
    ast = json.loads(pandoc("-t", "json", str(path)))
    nested: list = []
    for pb in ast["blocks"]:
        nested += _convert_block(pb)
    return Document(nested=nested)


def _assign_hashes(block) -> str:
    """Set `block.id` to SHA256(own content + nested hashes), recursively."""
    nested_hashes = [_assign_hashes(nested) for nested in block.nested]
    payload = block.model_dump(exclude={"id", "nested"})
    hasher = hashlib.sha256()
    hasher.update(json.dumps(payload, sort_keys=True).encode())
    for n in nested_hashes:
        hasher.update(n.encode())
    digest = hasher.hexdigest()
    block.hash = digest
    return digest


def _flatten(
    block: DocumentBlock,
    documents: list[IndexDocument],
    links: list[IndexLink],
    parent_hash: str | None,
    order: int,
) -> None:
    # Store the block without its nested blocks; structure lives in links.
    data = block.model_copy(update={"nested": []})
    documents.append(IndexDocument(hash=block.hash, data=data))
    if parent_hash is not None:
        links.append(
            IndexLink(
                from_=parent_hash,
                to_=block.hash,
                extra=NestedLinkMeta(order=order),
            ))
    for i, nested in enumerate(block.nested):
        _flatten(nested, documents, links, block.hash, i)


class DocumentBlockIndexerResult(MultiDocumentModel, extra="forbid"):
    pass


class DocumentBlockIndexer(BaseIndexer):
    asset_name = "full_document"
    result_model = DocumentBlockIndexerResult

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)

    def run(
        self,
        ctx: RunContext,
        request: IndexerRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> IndexerOutput:
        path = ctx.get_path(request.file_ref)

        root = pandoc_to_document(path)
        _assign_hashes(root)

        documents: list[IndexDocument] = []
        links: list[IndexLink] = []
        _flatten(root, documents, links, parent_hash=None, order=0)

        return IndexerOutput(
            indexer_id=self.asset_name,
            result=MultiDocumentModel(documents=documents, links=links),
        )
