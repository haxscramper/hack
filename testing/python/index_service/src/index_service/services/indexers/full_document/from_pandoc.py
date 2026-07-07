import json
import logging
from pathlib import Path

import plumbum
from beartype.typing import Literal, Sequence, cast

from index_service.services.indexers.full_document.full_document_types import (
    BulletListItem,
    Code,
    CodeBlockProps,
    Div,
    DivProps,
    Document,
    DocumentBlock,
    Heading,
    HeadingProps,
    InlineContent,
    Link,
    Markup,
    Math,
    NumberedListItem,
    Paragraph,
    Quote,
    RawBlock,
    StyledText,
    Table,
    TableCell,
    TableCellProps,
    TableProps,
    TableRow,
    TextAlignment,
    TextStyles,
    build,
    merge_text,
)

log = logging.getLogger(__name__)


def _convert_inlines(
        inlines: list,
        styles: TextStyles = TextStyles(),
) -> list[InlineContent]:
    styles = styles or {}
    out: list[InlineContent] = []
    for il in inlines:
        match il:
            case {"t": "Str"}:
                out.append(StyledText(text=il["c"], styles=styles))

            case {"t": "Space"}:
                out.append(StyledText(text=" ", styles=styles))

            case {"t": "SoftBreak" | "LineBreak"}:
                out.append(StyledText(text="\n", styles=styles))

            case {"t": "Emph"}:
                out += _convert_inlines(il["c"], styles.extend(Markup.ITALIC))

            case {"t": "Strong"}:
                out += _convert_inlines(il["c"], styles.extend(Markup.BOLD))

            case {"t": "Strikeout"}:
                out += _convert_inlines(il["c"], styles.extend(Markup.STRIKE))

            case {"t": "Underline"}:
                out += _convert_inlines(il["c"], styles.extend(Markup.UNDERLINE))

            case {"t": "Code"}:
                out.append(StyledText(
                    text=il["c"][1],
                    styles=styles.extend(Markup.CODE),
                ))

            case {"t": "Link"}:
                _, link_inlines, target = il["c"]
                out.append(Link(
                    href=target[0],
                    content=_convert_inlines(link_inlines)))  # type: ignore[arg-type]

            case {"t": "Image"}:
                _, link_inlines, target = il["c"]
                out.append(Link(
                    href=target[0],
                    content=_convert_inlines(link_inlines)))  # type: ignore[arg-type]

            case {"t": "Superscript" | "Subscript" | "SmallCaps" | "Span" | "Quoted"}:
                payload = il["c"]
                nested = payload[-1] if isinstance(payload, list) else payload
                out += _convert_inlines(nested, styles)

            case {"t": "Math"}:
                out.append(StyledText(text=il["c"][1], styles=styles.extend(Markup.MATH)))

            case {"t": "RawInline"}:
                out.append(
                    StyledText(text="".join(il["c"]), styles=styles.extend(Markup.RAW)))

            case str():
                out.append(StyledText(text=il))

            case _:
                raise RuntimeError(f"Unhandled inline element: {il}")

    return merge_text(out)


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


def _convert_list_items(items: list, item_type: str,
                        file_hash: str) -> Sequence[DocumentBlock]:
    cls = {
        "bulletListItem": BulletListItem,
        "numberedListItem": NumberedListItem,
    }[item_type]
    result = []
    for item in items:
        blocks: list = []
        for pb in item:
            blocks += _convert_block(pb, file_hash=file_hash)
        if not blocks:
            continue
        head = blocks[0]
        content = getattr(head, "content", [])
        result.append(build(cls, content=content, nested=blocks[1:], file_hash=file_hash))
    return result


# add helpers near other conversion helpers


def _pandoc_alignment_to_text_alignment(alignment: object) -> TextAlignment:
    tag = alignment.get("t") if isinstance(alignment, dict) else str(alignment)
    return {
        "AlignLeft": "left",
        "AlignCenter": "center",
        "AlignRight": "right",
        "AlignDefault": "left",
    }.get(tag, "left")


def _blocks_to_inline_content(blocks: list[dict]) -> list[InlineContent]:
    out: list[InlineContent] = []
    for block in blocks:
        match block.get("t"):
            case "Plain" | "Para":
                chunk = _convert_inlines(block["c"])
            case "CodeBlock":
                chunk = [StyledText(text=block["c"][1], styles=TextStyles(code=True))]
            case _:
                chunk = []

        if chunk:
            if out:
                out.append(StyledText(text="\n"))
            out.extend(chunk)

    return merge_text(out)


def _extract_table_caption(caption_obj: object) -> str | None:
    if isinstance(caption_obj, list):
        if caption_obj and isinstance(caption_obj[0], dict) and "t" in caption_obj[0]:
            return _extract_caption_text(caption_obj)

        for part in caption_obj:
            text = _extract_table_caption(part)
            if text:
                return text

    if isinstance(caption_obj, dict):
        for key in ("long", "blocks", "c"):
            if key in caption_obj:
                text = _extract_table_caption(caption_obj[key])
                if text:
                    return text

    return None


def _row_to_table_row(row_obj: list, *, is_header: bool, file_hash: str) -> TableRow:
    _row_attr, cells_obj = row_obj
    cells: list[TableCell] = []

    for cell_obj in cells_obj:
        _cell_attr, alignment, row_span, col_span, blocks = cell_obj
        cells.append(
            build(  # type: ignore
                TableCell,
                nested=[],
                file_hash=file_hash,
                props=TableCellProps(
                    textAlignment=_pandoc_alignment_to_text_alignment(alignment),
                    rowSpan=max(1, int(row_span)),
                    colSpan=max(1, int(col_span)),
                    isHeader=is_header,
                ),
                content=_blocks_to_inline_content(blocks),
            ))

    return build(
        TableRow,
        nested=list(),
        cells=cells,
        file_hash=file_hash,
    )  # type: ignore


def _convert_table(pb: dict, file_hash: str) -> Table:
    attr, caption, _colspecs, thead, tbodies, tfoot = pb["c"]
    _identifier, _classes, _keyvals = attr

    rows: list[TableRow] = []

    # thead: [attr, rows]
    if isinstance(thead, list) and len(thead) == 2:
        for row in thead[1]:
            rows.append(_row_to_table_row(row, is_header=True, file_hash=file_hash))

    # tbodies: [[attr, row_head_cols, head_rows, body_rows], ...]
    for tbody in tbodies:
        if not isinstance(tbody, list) or len(tbody) != 4:
            continue
        for row in tbody[2]:
            rows.append(_row_to_table_row(row, is_header=True, file_hash=file_hash))
        for row in tbody[3]:
            rows.append(_row_to_table_row(row, is_header=False, file_hash=file_hash))

    # tfoot: [attr, rows]
    if isinstance(tfoot, list) and len(tfoot) == 2:
        for row in tfoot[1]:
            rows.append(_row_to_table_row(row, is_header=False, file_hash=file_hash))

    return cast(
        Table,
        build(
            Table,
            props=TableProps(caption=_extract_table_caption(caption)),
            nested=rows,
            file_hash=file_hash,
        ),
    )


def _convert_block(pb: dict, file_hash: str) -> Sequence[DocumentBlock]:
    t = pb["t"]
    match t:
        case "Para" | "Plain":
            return [
                build(
                    Paragraph,
                    file_hash=file_hash,
                    content=_convert_inlines(pb["c"], TextStyles()),
                )
            ]

        case "Header":
            level, attr, inlines = pb["c"]
            props = HeadingProps(level=level)
            props.extra["subtree_auto_id"] = attr[0]
            for entry in attr[2]:
                props.extra[entry[0]] = entry[1]

            return [
                build(
                    Heading,
                    file_hash=file_hash,
                    props=props,
                    content=_convert_inlines(inlines, TextStyles()),
                    nested=[],
                )
            ]

        case "CodeBlock":
            attr, code = pb["c"]
            classes = attr[1]
            lang = classes[0] if classes else "text"
            return [
                build(
                    Code,
                    file_hash=file_hash,
                    props=CodeBlockProps(language=lang),
                    content=[StyledText(text=code)],
                )
            ]

        case "BulletList":
            return _convert_list_items(pb["c"], "bulletListItem", file_hash=file_hash)

        case "OrderedList":
            return _convert_list_items(pb["c"][1],
                                       "numberedListItem",
                                       file_hash=file_hash)

        case "DefinitionList":
            blocks: list[DocumentBlock] = []
            for term_inlines, definitions in pb["c"]:
                term = _convert_inlines(term_inlines)

                for def_blocks in definitions:
                    definition_inlines: list[InlineContent] = []
                    for db in def_blocks:
                        match db:
                            case {"t": "Plain" | "Para"}:
                                definition_inlines += _convert_inlines(db.get("c", []))
                            case _:
                                blocks += _convert_block(db, file_hash=file_hash)

                    if definition_inlines:
                        blocks.append(
                            build(
                                Paragraph,
                                file_hash=file_hash,
                                content=merge_text(term + [StyledText(text=" :: ")] +
                                                   definition_inlines),
                            ))

            return blocks

        case "BlockQuote":
            nested: list = []
            for inner in pb["c"]:
                nested += _convert_block(inner, file_hash=file_hash)
            return [build(Quote, file_hash=file_hash, nested=nested)]

        case "HorizontalRule":
            return []

        case "Table":
            return [_convert_table(pb, file_hash=file_hash)]

        case "RawBlock":
            return [
                build(
                    RawBlock,
                    file_hash=file_hash,
                    content=pb["c"][1],
                    lang=pb["c"][0],
                )
            ]

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
                    converted += _convert_block(child, file_hash=file_hash)

                if caption and converted:
                    _attach_caption(converted[0], caption)

                return converted

            nested: list[DocumentBlock] = []
            for child in inner:
                nested += _convert_block(child, file_hash=file_hash)

            return [
                build(
                    Div,
                    props=DivProps(
                        identifier=identifier,
                        classes=classes,
                        attributes={
                            k: v for k, v in keyvals
                        },
                    ),
                    file_hash=file_hash,
                    nested=nested,
                )
            ]

        case _:
            raise ValueError(f"Implicitly handled document block of type '{t}': {pb}")
            return [build(Paragraph, content=_convert_inlines(pb.get("c", [])))]


def _nest_by_heading_level(blocks: Sequence[DocumentBlock]) -> list[DocumentBlock]:
    root: list[DocumentBlock] = []
    heading_stack: list[Heading] = []

    for block in blocks:
        if isinstance(block, Heading):
            level = block.props.level

            while heading_stack and heading_stack[-1].props.level >= level:
                heading_stack.pop()

            if heading_stack:
                heading_stack[-1].nested.append(block)
            else:
                root.append(block)

            heading_stack.append(block)
            continue

        if heading_stack:
            heading_stack[-1].nested.append(block)
        else:
            root.append(block)

    return root


def pandoc_to_document(path: Path, file_hash: str) -> Document:
    pandoc = plumbum.local["pandoc"]
    ast = json.loads(pandoc("-t", "json", str(path)))
    Path("/tmp/pandoc-result.json").write_text(json.dumps(ast, indent=2))

    flat_blocks: list[DocumentBlock] = []
    for pb in ast["blocks"]:
        flat_blocks += _convert_block(pb, file_hash=file_hash)

    nested = _nest_by_heading_level(flat_blocks)
    result = build(Document, nested=nested, file_hash=file_hash)
    # Path(f"/tmp/ingest-structure-{path.name}.json").write_text(
    #     result.model_dump_json(
    #         indent=2,
    #         serialize_as_any=True,
    #     ))
    return result
