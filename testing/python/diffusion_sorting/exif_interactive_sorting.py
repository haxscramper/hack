import base64
import concurrent.futures
import hashlib
import logging
import os
from dataclasses import dataclass
from pathlib import Path

import pandas as pd
import plotly.express as px
import streamlit as st
from PIL import Image
from beartype import beartype
from scipy.sparse import csr_matrix
from sklearn.decomposition import TruncatedSVD
from sklearn.feature_extraction.text import TfidfVectorizer

from exif_preview import ImageParams, get_full_params, log, Tag, LoRATag

# --- Global Configurations ---
THUMBNAIL_MAX_WIDTH = 128
THUMBNAIL_MAX_HEIGHT = 128
APP_CACHE_NAME = "exif_explorer_app"
MAX_HISTORY_SIZE = 4  # Number of selected images to remember for the similarity view


@beartype
def _get_xdg_cache_dir() -> Path:
    """Returns the XDG-appropriate cache directory for thumbnails."""
    xdg_cache_home = os.environ.get("XDG_CACHE_HOME",
                                    os.path.expanduser("~/.cache"))
    cache_dir = Path(xdg_cache_home) / APP_CACHE_NAME / "thumbnails"
    cache_dir.mkdir(parents=True, exist_ok=True)
    return cache_dir


@beartype
def _process_thumbnail(original_path: str) -> tuple[str, str]:
    """Generates a thumbnail, caches it, and returns the base64 data URI."""
    path_obj = Path(original_path)
    file_hash = hashlib.md5(str(
        path_obj.resolve()).encode('utf-8')).hexdigest()
    cache_dir = _get_xdg_cache_dir()
    thumb_path = cache_dir / f"{file_hash}.webp"

    if not thumb_path.exists():
        try:
            with Image.open(path_obj) as img:
                img.thumbnail((THUMBNAIL_MAX_WIDTH, THUMBNAIL_MAX_HEIGHT))
                img.save(thumb_path, format="WEBP")
        except Exception as e:
            logging.warning(
                f"Failed to generate thumbnail for {original_path}: {e}")
            return (original_path, "")

    try:
        with open(thumb_path, "rb") as f:
            b64_data = base64.b64encode(f.read()).decode('utf-8')
        return (original_path, f"data:image/webp;base64,{b64_data}")
    except Exception as e:
        logging.warning(f"Failed to encode thumbnail {thumb_path}: {e}")
        return (original_path, "")


@st.cache_data(show_spinner="Generating Thumbnails...")
def _get_all_thumbnails(paths: list[str]) -> dict[str, str]:
    """Executes thumbnail generation in multi-threaded mode."""
    results_dict = {}
    with concurrent.futures.ThreadPoolExecutor() as executor:
        for original_path, b64_uri in executor.map(_process_thumbnail, paths):
            if b64_uri:
                results_dict[original_path] = b64_uri
    return results_dict


@beartype
def _extract_features(param: ImageParams) -> list[str]:
    features: list[str] = []
    if param.parsed_prompt is not None:
        for tag in param.parsed_prompt:
            if isinstance(tag, Tag):
                features.append(tag.text.strip().lower())
            elif isinstance(tag, LoRATag):
                features.append(tag.id2.strip().lower())
            elif isinstance(tag, str):
                features.append(tag.strip().lower())
            else:
                raise TypeError(f"Unexpected ParsedTag type: {type(tag)}")
    for lora in param.loras:
        features.append(lora.name.strip().lower())
    return features


@beartype
def _identity_tokenizer(tokens: list[str]) -> list[str]:
    return tokens


@beartype
@dataclass
class _GetTfidfModelResultType:
    vectorizer: "TfidfVectorizer"
    matrix: "csr_matrix"
    feature_names: list[str]


@st.cache_resource
@beartype
def _get_tfidf_model(
        tokenized_texts: list[list[str]]) -> _GetTfidfModelResultType:
    vectorizer = TfidfVectorizer(tokenizer=_identity_tokenizer,
                                 preprocessor=_identity_tokenizer,
                                 token_pattern=None,
                                 max_df=0.90,
                                 min_df=2)
    matrix = vectorizer.fit_transform(tokenized_texts)
    feature_names = vectorizer.get_feature_names_out().tolist()
    return _GetTfidfModelResultType(vectorizer=vectorizer,
                                    matrix=matrix,
                                    feature_names=feature_names)


@st.cache_resource
@beartype
def _get_cached_params(workspace_root: Path) -> list[ImageParams]:
    return get_full_params(
        reference_dirs=[
            workspace_root.joinpath("reference_tensor_art_mirror"),
            workspace_root.joinpath("tensor_saved_high_res_mirror"),
        ],
        files="*.webp",
    )


@beartype
@dataclass
class _TagCount:
    Tag: str
    Count: int


@beartype
def _get_sorted_features(tokenized_texts: list[list[str]],
                         valid_features: list[str]) -> list[_TagCount]:
    counts: dict[str, int] = {}
    for tokens in tokenized_texts:
        for tag in tokens:
            if tag in counts:
                counts[tag] += 1
            else:
                counts[tag] = 1

    valid_set = set(valid_features)
    sorted_items = sorted(counts.items(), key=lambda x: x[1], reverse=True)
    return [
        _TagCount(Tag=tag, Count=count) for tag, count in sorted_items
        if tag in valid_set
    ]


@beartype
def setup_app(image_params: list[ImageParams]) -> None:
    st.set_page_config(layout="wide")

    # Custom CSS to reduce the left margin of the page
    st.markdown("""
        <style>
        .block-container {
            padding-left: 1rem !important; /* Reduces the massive default left margin */
            padding-right: 1rem !important;
        }
        </style>
    """,
                unsafe_allow_html=True)

    # --- Session State Initialization ---
    if "draft_weights" not in st.session_state:
        st.session_state.draft_weights = {}
    if "applied_weights" not in st.session_state:
        st.session_state.applied_weights = {}
    if "selection_history" not in st.session_state:
        st.session_state.selection_history = []

    # --- Data Processing ---
    tokenized_prompts = [_extract_features(param) for param in image_params]
    tfidf_result = _get_tfidf_model(tokenized_prompts)
    base_matrix = tfidf_result.matrix
    feature_names = tfidf_result.feature_names
    vectorizer = tfidf_result.vectorizer

    working_matrix = base_matrix.copy()
    for tag, weight in st.session_state.applied_weights.items():
        tag_index = vectorizer.vocabulary_.get(tag)
        if tag_index is not None:
            working_matrix[:,
                           tag_index] = working_matrix[:, tag_index] * weight

    # SVD: Compute 3 components to bypass the dense mean vector
    svd = TruncatedSVD(n_components=3, random_state=42)
    coords = svd.fit_transform(working_matrix)

    df = pd.DataFrame({
        "id":
        range(len(image_params)),
        "basename": [Path(param.original_path).name for param in image_params],
        "x":
        coords[:, 1],
        "y":
        coords[:, 2]
    })

    # --- Layout Definitions ---
    left_col, right_col = st.columns([2, 7])

    # We create a container in the left column to render after the scatterplot logic
    left_container = left_col.container()

    # --- Right Column: Plotly Scatterplot ---
    with right_col:
        st.write("### Tag Distribution Map")
        show_thumbnails = st.toggle("Show Image Thumbnails", value=False)

        fig = px.scatter(df,
                         x="x",
                         y="y",
                         hover_name="basename",
                         hover_data={
                             "x": False,
                             "y": False,
                             "id": False,
                             "basename": False
                         },
                         render_mode="webgl")

        if show_thumbnails:
            all_paths = [str(param.original_path) for param in image_params]
            b64_thumbnails = _get_all_thumbnails(all_paths)

            images = []
            for param, x_val, y_val in zip(image_params, df["x"], df["y"]):
                b64_uri = b64_thumbnails.get(str(param.original_path))
                if b64_uri:
                    images.append(
                        dict(source=b64_uri,
                             x=x_val,
                             y=y_val,
                             xref="x",
                             yref="y",
                             sizex=0.005,
                             sizey=0.005,
                             xanchor="center",
                             yanchor="middle",
                             layer="below"))

            fig.update_layout(images=images)
            fig.update_traces(marker=dict(opacity=0))

        fig.update_layout(height=850,
                          margin=dict(l=0, r=0, t=0, b=0),
                          dragmode="pan")

        # Updated to width="stretch" per 2026 deprecation warning
        event = st.plotly_chart(fig,
                                on_select="rerun",
                                selection_mode="points",
                                width="stretch")

    # --- Process Chart Events ---
    if len(event.selection.points) > 0:
        selected_index = event.selection.points[0]["point_index"]
        if not st.session_state.selection_history or st.session_state.selection_history[
                -1] != selected_index:
            st.session_state.selection_history.append(selected_index)
            if len(st.session_state.selection_history) > MAX_HISTORY_SIZE:
                st.session_state.selection_history.pop(0)

    # --- Left Column: Tools and Previews ---
    with left_container:
        # 1. Weight Configuration Expander
        with st.expander("Weight Configuration", expanded=False):
            st.write(
                "Sliders for tag weight changing. Tags are sorted by dataset frequency. Click a tag to assign a new weight."
            )

            tag_counts = _get_sorted_features(tokenized_prompts, feature_names)
            tag_df = pd.DataFrame(tag_counts)

            # Updated to width="stretch"
            event_df = st.dataframe(tag_df,
                                    width="stretch",
                                    hide_index=True,
                                    on_select="rerun",
                                    selection_mode="single-row")

            selected_rows = event_df.selection.rows
            if len(selected_rows) > 0:
                selected_tag = tag_df.iloc[selected_rows[0]]["Tag"]
                current_val = st.session_state.draft_weights.get(
                    selected_tag, 1.0)
                new_val = st.number_input(f"Weight: {selected_tag}",
                                          value=float(current_val),
                                          step=0.1)
                st.session_state.draft_weights[selected_tag] = new_val

            if st.button("Rebuild Matrix"):
                st.session_state.applied_weights = st.session_state.draft_weights.copy(
                )
                st.rerun()

        # 2. Current Selection Preview Expander
        with st.expander("Current Image & Prompt", expanded=True):
            if st.session_state.selection_history:
                current_idx = st.session_state.selection_history[-1]
                selected_param = image_params[current_idx]
                image_path = Path(selected_param.original_path)

                st.markdown(f"**{image_path.name}**")
                # Updated to width="stretch"
                st.image(str(image_path), width="stretch")
                st.write("**Prompt:**")
                st.write(selected_param.prompt)
            else:
                st.info(
                    "Click an image on the scatterplot to preview it here.")

        # 3. History & Similarity Expander
        with st.expander("Selection History & Similarities", expanded=True):
            history = st.session_state.selection_history

            if len(history) < 2:
                st.info(
                    f"Select at least 2 images to compare similarities. (Tracking last {MAX_HISTORY_SIZE})"
                )
            else:
                # Iterate backwards so the newest pairs show at the top
                for i in range(len(history) - 1, 0, -1):
                    idx1 = history[i - 1]  # Older image in the pair
                    idx2 = history[i]  # Newer image in the pair
                    param1 = image_params[idx1]
                    param2 = image_params[idx2]

                    path1 = Path(param1.original_path)
                    path2 = Path(param2.original_path)

                    # Row 1: The two images side-by-side with basenames
                    colA, colB = st.columns(2)
                    with colA:
                        st.markdown(f"**{path1.name}**")
                        # Updated to width="stretch"
                        st.image(str(path1), width="stretch")
                    with colB:
                        st.markdown(f"**{path2.name}**")
                        # Updated to width="stretch"
                        st.image(str(path2), width="stretch")

                    # Row 2: The similarities text area below them
                    tags1 = set(_extract_features(param1))
                    tags2 = set(_extract_features(param2))
                    overlap = tags1.intersection(tags2)

                    if overlap:
                        st.success(
                            f"**Overlapping Tags ({len(overlap)}):**\n\n" +
                            ", ".join(sorted(overlap)))
                    else:
                        st.warning(
                            "No overlapping tags between these two images.")

                    # Visual separator before the next pair (unless it's the last one we're drawing)
                    if i > 1:
                        st.divider()


if __name__ == "__main__":
    import sys
    if len(sys.argv) < 2:
        print("Usage: streamlit run app.py <workspace_root_path>")
        sys.exit(1)

    workspace_root = Path(sys.argv[1])
    mirror_list = _get_cached_params(workspace_root)

    logging.info("Starting app")
    setup_app(mirror_list)
