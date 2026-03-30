# For working on this project

- use `uv` for running/managing the project

# Initial implementation plan

- Have a GUI to preview transcribed pages. docling parses doctags in some format that can be overlaid on top of the page to show where each element was -- I want to be able to see a page with this overlay. If docling does not provide a clear data structure to represent the spatial locations of all the doctags on the page, implement the structure for this. I should be able to select a doctag and mark it as unnecessary for later post-processing. 
- The second mode of the PDF file is "overlaid pages". In this mode a range of pages should be displayed "on top" of each other -- preview of each page is post-processed to make the white part transparent, and the black parts can overlay on top of each other. The doctags annotations are overlaid as well. They must also be selectable, and if there are multiple ones overlaid on top of each other, I want to be able to select them all at once. The purpose of this mode is to preview the post-OCR PDF and remove some text elements like page numbers, headings, footers and maybe some other things that are not necessary for the text-only format. 
- Instead of accepting a single input and output file paths, the script should have a list of input directories and paths, and a single output directory. In the output directory the script will create one sub-directory for each PDF, where it would put the cached files and the final result document. The conversion functionality should support running in headless mode, the rest of teh GUI-related features can start a window
- use pyside6 to implement the GUI
- the configuration for the app input directories is read from a JSON file -- create a pydantic schema to describe the configuration file format and read it. 
- The GUI structure is as follows: 
  - On the center side, occupying the 1/3 of horizontal space is the preview of the page or the overlaid collection of pages. 
    - If viewing individual pages, the part below the page preview should show the `<` and `>` buttons, to move between pages. 
    - If viewing in overlaid collection, the part below should show the two-ended slider indicatring the range of overlaid pages. 
  - On the left side, is the list of files in the input directories. The list is presented using list/grid model view, with the green highlight for the fully completed pages. Preview for the PDF files has thumbnails, which are cached in the XDG-appropriate directory. The input PDF list is searchable using text area below the file list. 
  - On the right side, final 1/3 is the preview of the final HTML document. The script should support post-processing of the text using LLM to join the paragraphs that were broken across multiple pages, indicate potential spelling and other issues. The part below the HTML has the "update" button that will re-run the conversion to HTML. 

The application implementation is split into several components (files)

- PDF OCR component -- take input configuration (list of input directories or individual files) and process all of them into the OCR information JSON dumps (each page result is saved individually to allow interruption mid-processing and incremental builds). This part can run in headless mode. The JSON dumps for pages contains three parts: 
  - The original response from the docling -- without any processing, fully
  - The spatially aware placement of the tags. A tree-like arrangement of the doctags, with all the original metadata and placement information. Each doctag on a page must be uniquely addressable. There must also a list of doctag order if the spatial placement is not good enough. 
    ``` 
    Page
      Doctag1 (x, y, width, height, text?)
      Doctag2 (x, y, width, height, text?, other=arguments)
        NestedTag2 (...)
        NestedTag3 (...)
    ```
  - User-annotations. This is where the user selections and annotations are persistently stored from the pdf preview step (the second part of the PDF processing pipeline). 
- Original PDF preview. See one page at a time or overlay of the pages. This is the second component in the PDF processing pipeline. It takes in the original PDF file (to generate preview) and results of the OCR (to generate overlay information for user to select). In this GUI the user can select one or more doctags, mark them as removed, or change their text or attributes (like marking a section with `tag=h1`). The selection should support drag-based selection, or click-based selection. After this stage the data model is enhanced with the user-provided annotations to discard or amend the doctag ranges. 
- HTML preview. Based on the list of individual pages, create final HTML. Use locally running LLM that I'm running via llama.cpp to merge the document blocks in a final PDF or flag the possible issues. The usage of LLM should be optional, there must also be a version that performs simple mechanical conversion of the input blocks to the final HTML
  - Define the prompts for the LLM to identify issues
  - Define prompts for the LLM to identify the merge/conversion solution
  - Model used is the qwen 3.5 family 



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
/home/haxscramper/defaultdirs/workspace/repos/hack/testing/python/pdf_converter/
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

## 5. Implementation Steps & Todos

- [x] **1. Refactor configuration**: Create Pydantic schema for JSON config (input dirs, output dir). `AppConfig` will parse lists of target files/directories.
- [x] **2. Refactor OCR Component**: Separate headless processing logic from CLI and migrate output structure. The `call_ollama_vlm` function will securely parse the XML strings to extract the spatial logic.
- [x] **3. Update data storage**: Implement per-page JSON dumps containing raw docling, spatial tree, and user annotations. Each PDF creates an output directory where these JSON dumps live safely.
- [x] **4. Implement spatial tree model**: Create classes for `Page`, `Doctag`, and nested elements with spatial geometry (using the models defined above).
- [x] **5. Setup PySide6 GUI**: Create main window with 3-column layout (Left: Files, Center: Preview, Right: HTML).
- [x] **6. Implement Left Panel**: Build file list/grid model using `QAbstractListModel`. Thumbnails are generated via `pdf2image` and saved to `xdg_cache_home()`. Add search filtering and green completion status highlights.
- [ ] **7. Implement Center Panel (Single View)**: Add PDF page rendering (`QGraphicsPixmapItem`), `<`/`>` navigation, and doctag bounding box overlays (`QGraphicsRectItem`).
- [ ] **8. Implement Center Panel (Overlay View)**: Load page into `QImage` and mask out white pixels to make them transparent. Stack multiple images using `QGraphicsPixmapItem` transparency. Add page range slider and multi-tag overlay rendering.
- [ ] **9. Implement Interaction**: Configure `QGraphicsScene` to handle click/drag selection (using `QGraphicsView.RubberBandDrag`). Add context actions to toggle `user_removed` (visually graying out or reddening the tags).
- [ ] **10. Implement Right Panel (HTML)**: Add HTML preview widget (`QTextBrowser`) and an "Update" HTML generation button.
- [ ] **11. Implement LLM Integration**: Add llama.cpp prompts (qwen 3.5) for paragraph merging and issue flagging. Include a mechanical fallback that skips the LLM if disabled.
- [ ] **12. Wire application state**: Connect GUI annotations to persistent JSON data, ensuring UI updates trigger file saves and HTML rebuilds.
