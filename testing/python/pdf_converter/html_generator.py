import logging
from typing import List

from models import PageData, DocTag
from llm_processor import merge_blocks_llm, flag_issues_llm

logger = logging.getLogger(__name__)

def map_tag_to_html(tag_name: str) -> str:
    mapping = {
        "section_header": "h2",
        "text": "p",
        "list": "ul",
        "list_item": "li",
        "table": "table",
        "footnote": "p",
        "formula": "p",
        "page_footer": "footer",
        "picture": "div",
        "root": "div",
        "unspecified": "div",
        "h1": "h1",
        "h2": "h2",
        "h3": "h3",
        "p": "p",
    }
    return mapping.get(tag_name, "div")

def generate_html_content(pages_data: List[PageData], use_llm: bool = False) -> str:
    html_parts = ["<html><head><style>body { font-family: sans-serif; }</style></head><body>"]
    
    last_p_tag = None
    
    for page in pages_data:
        html_parts.append(f"<hr/><h3>Page {page.page_number}</h3>")
        
        def process_tag(tag: DocTag):
            nonlocal last_p_tag
            if tag.user_removed:
                return
                
            raw_tag_name = tag.user_tag_override or tag.tag_name
            html_tag = map_tag_to_html(raw_tag_name)
            
            text = tag.user_edited_text if tag.user_edited_text is not None else tag.text
            
            if text:
                if use_llm and html_tag == "p" and last_p_tag is not None:
                    # check if previous text ends with hyphen or doesn't have period
                    prev_text = last_p_tag
                    # Let's just merge all consecutive p tags or just do simple rule
                    if prev_text.endswith('-') or not prev_text.endswith(('.', '!', '?')):
                        merged = merge_blocks_llm(prev_text, text)
                        if merged:
                            logger.info(f"Merged blocks:\n[1] {prev_text}\n[2] {text}\n[M] {merged}")
                            # Replace the last element (which should be the previous paragraph)
                            html_parts[-1] = f"<{html_tag}>{merged}</{html_tag}>"
                            last_p_tag = merged
                            return
                            
                html_parts.append(f"<{html_tag}>{text}</{html_tag}>")
                if html_tag == "p":
                    last_p_tag = text
                else:
                    last_p_tag = None
            else:
                html_parts.append(f"<{html_tag}>")
                for child in tag.children:
                    process_tag(child)
                html_parts.append(f"</{html_tag}>")
        
        for tag in page.spatial_tags:
            process_tag(tag)
            
    html_parts.append("</body></html>")
    return "\n".join(html_parts)
