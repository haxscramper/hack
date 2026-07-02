from argparse import ArgumentError
import hashlib
import json
import logging
from pathlib import Path
from typing import Annotated, Literal, Union

import magic
from beartype.typing import ClassVar, Sequence, cast
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


class DocumentLink(IndexLink, extra="forbid"):
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


class Div(DocumentBlock):
    type: Literal["div"] = "div"
    props: DivProps = Field(default_factory=DivProps)


for _cls in (
        Document,
        Paragraph,
        Heading,
        Code,
        BulletListItem,
        NumberedListItem,
        Quote,
        Div,
):
    _cls.model_rebuild()


def _build(
        cls: type[DocumentBlock],
        *,
        nested: Sequence[DocumentBlock] = (),
        **kwargs,
) -> DocumentBlock:
    """Construct a block with its hash computed inline from its own payload
    plus the hashes of already-built nested children."""
    nested = list(nested)
    block = cls.model_construct(hash="", nested=nested, **kwargs)
    payload = block.model_dump(exclude={"id", "nested", "hash"})
    hasher = hashlib.sha256()
    hasher.update(json.dumps(payload, sort_keys=True).encode())
    for n in nested:
        hasher.update(n.hash.encode())
    block.hash = hasher.hexdigest()
    return block


def _merge_text(nodes: list[InlineContent]) -> list[InlineContent]:
    merged: list[InlineContent] = []
    for node in nodes:
        if (isinstance(node, StyledText) and merged
                and isinstance(merged[-1], StyledText)
                and merged[-1].styles == node.styles):
            merged[-1] = StyledText(
                text=merged[-1].text + node.text,
                styles=node.styles,
            )
        else:
            merged.append(node)
    return merged


def _convert_inlines(
    inlines: list,
    styles: dict | None = None,
) -> list[InlineContent]:
    styles = styles or {}
    out: list[InlineContent] = []
    for il in inlines:
        match il:
            case {"t": "Str"}:
                out.append(
                    StyledText(text=il["c"], styles=TextStyles(**styles)))

            case {"t": "Space"}:
                out.append(StyledText(text=" ", styles=TextStyles(**styles)))

            case {"t": "SoftBreak" | "LineBreak"}:
                out.append(StyledText(text="\n", styles=TextStyles(**styles)))

            case {"t": "Emph"}:
                out += _convert_inlines(il["c"], {**styles, "italic": True})

            case {"t": "Strong"}:
                out += _convert_inlines(il["c"], {**styles, "bold": True})

            case {"t": "Strikeout"}:
                out += _convert_inlines(il["c"], {**styles, "strike": True})

            case {"t": "Underline"}:
                out += _convert_inlines(il["c"], {**styles, "underline": True})

            case {"t": "Code"}:
                out.append(
                    StyledText(
                        text=il["c"][1],
                        styles=TextStyles(**{
                            **styles, "code": True
                        }),
                    ))

            case {"t": "Link"}:
                _, link_inlines, target = il["c"]
                out.append(
                    Link(href=target[0],
                         content=_convert_inlines(
                             link_inlines)))  # type: ignore[arg-type]

            case {
                "t":
                "Superscript" | "Subscript" | "SmallCaps" | "Span" | "Quoted"
            }:
                payload = il["c"]
                nested = payload[-1] if isinstance(payload, list) else payload
                out += _convert_inlines(nested, styles)

            case str():
                out.append(StyledText(text=il))

            case _:
                raise RuntimeError(f"Unhandled inline element: {il}")

    return _merge_text(out)


def _inline_text(inlines: list[InlineContent]) -> str:
    parts: list[str] = []
    for item in inlines:
        if isinstance(item, StyledText):
            parts.append(item.text)
        elif isinstance(item, Link):
            parts.append("".join(seg.text for seg in item.content))
    return "".join(parts)


def _extract_caption_text(blocks: list[dict]) -> str | None:
    lines: list[str] = []
    for block in blocks:
        if block["t"] in {"Plain", "Para"}:
            text = _inline_text(_convert_inlines(block["c"])).strip()
            if text:
                lines.append(text)
    if not lines:
        return None
    return "\n".join(lines)


def _attach_caption(block: DocumentBlock, caption: str) -> None:
    props = getattr(block, "props", None)
    if props is None:
        return
    if "caption" not in props.__class__.model_fields:
        return


def _convert_list_items(items: list,
                        item_type: str) -> Sequence[DocumentBlock]:
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
        result.append(_build(cls, content=content, nested=blocks[1:]))
    return result


def _convert_block(pb: dict) -> Sequence[DocumentBlock]:
    t = pb["t"]
    match t:
        case "Para" | "Plain":
            return [_build(Paragraph, content=_convert_inlines(pb["c"]))]

        case "Header":
            level, _attr, inlines = pb["c"]
            clamped = cast(Literal[1, 2, 3], min(max(level, 1), 3))
            return [
                _build(
                    Heading,
                    props=HeadingProps(level=clamped),
                    content=_convert_inlines(inlines),
                )
            ]

        case "CodeBlock":
            attr, code = pb["c"]
            classes = attr[1]
            lang = classes[0] if classes else "text"
            return [
                _build(
                    Code,
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
            return [_build(Quote, nested=nested)]

        case "HorizontalRule":
            return []

        case "Div":
            attr, inner = pb["c"]
            identifier, classes, keyvals = attr

            if "captioned-content" in classes:
                caption: str | None = None
                remaining = inner

                if remaining and remaining[0].get("t") == "Div":
                    cap_attr, cap_blocks = remaining[0]["c"]
                    _cap_identifier, cap_classes, _cap_keyvals = cap_attr
                    if "caption" in cap_classes:
                        caption = _extract_caption_text(cap_blocks)
                        remaining = remaining[1:]

                converted: list[DocumentBlock] = []
                for child in remaining:
                    converted += _convert_block(child)

                if caption and converted:
                    _attach_caption(converted[0], caption)

                return converted

            nested: list[DocumentBlock] = []
            for child in inner:
                nested += _convert_block(child)

            return [
                _build(
                    Div,
                    props=DivProps(
                        identifier=identifier,
                        classes=classes,
                        attributes={
                            k: v
                            for k, v in keyvals
                        },
                    ),
                    nested=nested,
                )
            ]

        case _:
            log.warning(f"Implicitly handled document block of type '{t}'")
            return [
                _build(Paragraph, content=_convert_inlines(pb.get("c", [])))
            ]


def pandoc_to_document(path: Path) -> Document:
    ast = json.loads(pandoc("-t", "json", str(path)))
    Path("/tmp/pandoc-result.json").write_text(json.dumps(ast, indent=2))
    nested: list = []
    for pb in ast["blocks"]:
        nested += _convert_block(pb)
    return _build(Document, nested=nested)


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
    documents.append(block.model_copy(update={"nested": []}))
    if parent_hash is not None:
        links.append(
            DocumentLink(
                from_=parent_hash,
                to_=block.hash,
                order=order,
            ))

    for i, nested in enumerate(block.nested):
        _flatten(nested, documents, links, block.hash, i)


class DocumentBlockIndexerResult(MultiDocumentModel, extra="forbid"):
    pass


class DocumentBlockIndexer(BaseIndexer):
    asset_name = "document_block"
    result_model = DocumentBlockIndexerResult

    def __init__(self, **kwargs) -> None:
        super().__init__(**kwargs)
        self._magic = magic.Magic(mime=True)

    def can_run(self, path: Path) -> bool:
        mime = self._magic.from_file(str(path.resolve()))
        if mime.startswith("text/"):
            return True

        else:
            return False

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
