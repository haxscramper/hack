# Architecture & Implementation Design

## 1. Project Overview & Requirements
The goal of this project is to build a robust PDF OCR and post-processing tool utilizing local vision models (via docling/ollama) and a PySide6 graphical user interface. The application allows users to preview and refine OCR results before finalizing them into an HTML document. 

Key features include:
- **Headless OCR Component**: Converts PDFs to images, processes them via local AI to extract doctags, and saves per-page JSON dumps containing the raw docling response, a spatial tree of the doctags, and persistent user annotations.
- **Three-Pane GUI**: 
  - **Left Panel**: A searchable file list of input directories with cached thumbnails and completion status indicators.
  - **Center Panel (Preview & Overlay)**: Displays individual PDF pages or a range of overlaid pages (where white pixels are made transparent to stack black text). Users can view the spatial bounding boxes of doctags, select them via click or drag, and modify attributes (e.g., mark headers/footers as unnecessary).
  - **Right Panel (HTML & LLM)**: Previews the final generated HTML. Includes an optional integration with a local LLM (Qwen 3.5 via llama.cpp) to intelligently merge broken paragraphs across pages and flag spelling/formatting issues.

## 2. Final Project Structure
The application will be broken down into modular components to separate the headless OCR, GUI, data models, and LLM processing:

```text
.
├── pyproject.toml
├── uv.lock
├── config.json              # Sample input/output configuration
├── main.py                  # Entry point (CLI & GUI launcher)
├── config.py                # Pydantic schema for `config.json`
├── models.py                # Pydantic models for spatial doctags & annotations
├── ocr_pipeline.py          # Headless PDF -> Image -> Ollama/Docling component
├── llm_processor.py         # llama.cpp (qwen 3.5) prompts and execution logic
├── html_generator.py        # Logic to merge blocks and output final HTML
└── gui/
    ├── __init__.py
    ├── main_window.py       # Main PySide6 Window (3-column layout)
    ├── left_panel.py        # File list, search, thumbnail caching (QListView/QAbstractListModel)
    ├── center_panel.py      # QGraphicsView for single/overlay page viewing
    └── right_panel.py       # HTML preview (QTextBrowser) & LLM controls
```

## 3. Spatial Data Model (`models.py`)
Docling's spatial placement needs a concrete, uniquely addressable tree structure so the GUI can accurately overlay bounding boxes and track user edits.

```python
from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any

class BoundingBox(BaseModel):
    x: float
    y: float
    width: float
    height: float

class DocTag(BaseModel):
    id: str = Field(description="Unique ID for GUI selection, e.g., 'page1-tag4'")
    tag_name: str = Field(description="e.g., 'h1', 'p', 'table'")
    bbox: Optional[BoundingBox] = None
    text: Optional[str] = ""
    attributes: Dict[str, Any] = Field(default_factory=dict)
    
    # Tree Structure
    children: List['DocTag'] = Field(default_factory=list)
    
    # User Annotations (GUI overrides)
    user_removed: bool = False
    user_edited_text: Optional[str] = None
    user_tag_override: Optional[str] = None

class PageData(BaseModel):
    page_number: int
    raw_docling_response: str
    spatial_tags: List[DocTag] = Field(default_factory=list)
    image_cache_path: str
```
*Each PDF gets its own sub-directory in the `output_dir` containing these `PageData` JSON dumps (e.g., `page_01.json`).*

## 4. LLM Prompts (Qwen 3.5 via llama.cpp)
We will use Qwen 3.5 with specific prompt templates for post-processing the parsed blocks.

### A. Paragraph Merging / Formatting Prompt
Used when a paragraph is broken across two pages or has mid-sentence line breaks from OCR.
```text
<|im_start|>system
You are an expert document editor. Your task is to seamlessly merge text blocks extracted from PDF pages into a clean HTML format.
Rules:
1. Fix broken paragraphs that span across multiple blocks.
2. Remove hyphenations at the end of lines.
3. Keep the original meaning and wording perfectly intact.
4. Output ONLY the corrected text/HTML without any conversational filler.
<|im_end|>
<|im_start|>user
[Block 1]: {text_block_1}
[Block 2]: {text_block_2}
Please merge and correct these blocks.
<|im_end|>
<|im_start|>assistant
```

### B. Issue Flagging Prompt
Used to highlight potential OCR artifacts.
```text
<|im_start|>system
You are a proofreader. Review the following OCR-extracted text and identify:
1. Spelling mistakes or garbled text.
2. Likely OCR artifacts (e.g., 'l' instead of '1', random symbols).
3. Formatting inconsistencies.
Output your findings as a strict JSON list of objects: [{"issue_type": "spelling", "original": "teh", "suggestion": "the", "context": "...teh dog..."}]
<|im_end|>
<|im_start|>user
{ocr_text}
<|im_end|>
<|im_start|>assistant
```

## 6. External tools

### Docling
The application leverages the `docling_core` library to parse the VLM output. The core data models used from docling are:
- `DocTagsDocument` and `DocTagsPage`: Used to ingest the raw XML/doctags string containing spatial `<loc_N>` tags.
- `DoclingDocument`: The parsed, hierarchical representation of the document created via `DoclingDocument.load_from_doctags()`. 
- `iterate_items()`: A method on `DoclingDocument` that yields items (e.g. `TextItem`, `PictureItem`, `TableItem`) across the document. We use this to extract semantic labels (like "text", "page_footer"), text content, and spatial data.
- Spatial data in `docling_core` is accessible via the item's `prov` (provenance) attribute. The first provenance entry contains a `bbox` which provides normalized bounding box coordinates (`l`, `t`, `r`, `b`) mapped continuously between [0, 1].

These structures allow the pipeline to abstract away string manipulation and safely extract a flattened or hierarchical tree of parsed page elements, which are then mapped to our custom `DocTag` and `BoundingBox` models for GUI consumption.
