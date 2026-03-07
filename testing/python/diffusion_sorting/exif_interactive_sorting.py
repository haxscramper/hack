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
import umap
from PIL import Image
from beartype import beartype
from sklearn.feature_extraction.text import TfidfVectorizer

from exif_preview import ImageParams, get_full_params, Tag, LoRATag
from exif_tag_category import TagCategorizer

# --- Global Configurations ---
THUMBNAIL_MAX_WIDTH = 128
THUMBNAIL_MAX_HEIGHT = 128
APP_CACHE_NAME = "exif_explorer_app"
MAX_HISTORY_SIZE = 4


@beartype
def _get_xdg_cache_dir() -> Path:
    xdg_cache_home = os.environ.get("XDG_CACHE_HOME",
                                    os.path.expanduser("~/.cache"))
    cache_dir = Path(xdg_cache_home) / APP_CACHE_NAME / "thumbnails"
    cache_dir.mkdir(parents=True, exist_ok=True)
    return cache_dir


@beartype
def _process_thumbnail(original_path: str) -> tuple[str, str]:
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
        return (original_path, "")


@st.cache_data(show_spinner="Generating Thumbnails...")
def _get_all_thumbnails(paths: list[str]) -> dict[str, str]:
    results_dict = {}
    with concurrent.futures.ThreadPoolExecutor() as executor:
        for original_path, b64_uri in executor.map(_process_thumbnail, paths):
            if b64_uri:
                results_dict[original_path] = b64_uri
    return results_dict


@st.cache_resource
def get_categorizer() -> TagCategorizer:
    # Caching the categorizer prevents reloading YAMLs/LLM on every UI click
    return TagCategorizer()


@beartype
def _extract_features(param: ImageParams,
                      categorizer: TagCategorizer) -> list[str]:
    features: list[str] = []
    if param.parsed_prompt is not None:
        for tag in param.parsed_prompt:
            if isinstance(tag, Tag):
                features.append(categorizer.normalize(tag.text))
            elif isinstance(tag, LoRATag):
                features.append(categorizer.normalize(tag.id2))
            elif isinstance(tag, str):
                features.append(categorizer.normalize(tag))
            else:
                raise TypeError(f"Unexpected ParsedTag type: {type(tag)}")
    for lora in param.loras:
        features.append(categorizer.normalize(lora.name))
    return features


@beartype
def _identity_tokenizer(tokens: list[str]) -> list[str]:
    return tokens


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
def setup_app(image_params: list["ImageParams"]) -> None:
    st.set_page_config(layout="wide")

    st.markdown("""
        <style>
        .block-container {
            padding-left: 1rem !important;
            padding-right: 1rem !important;
        }
        </style>
    """,
                unsafe_allow_html=True)

    if "selection_history" not in st.session_state:
        st.session_state.selection_history = []

    categorizer = get_categorizer()

    # 1. Extract and normalize all tags
    all_tokenized_prompts = [
        _extract_features(param, categorizer) for param in image_params
    ]

    # 2. Identify all unique tags and unique categories present in the dataset
    all_unique_tags = set()
    dataset_categories = set()
    for prompt in all_tokenized_prompts:
        for tag in prompt:
            all_unique_tags.add(tag)
            dataset_categories.add(categorizer.get_category(tag))

    sorted_unique_tags = sorted(list(all_unique_tags))
    sorted_dataset_categories = sorted(list(dataset_categories))

    # --- Layout Definitions ---
    left_col, right_col = st.columns([3, 7])
    left_container = left_col.container()

    # --- Sidebar / Left Controls ---
    with left_container:
        with st.expander("Tag Filtering & Categories", expanded=False):
            st.write(
                "Exclude tags or entire categories from affecting the clustering layout."
            )

            # Category Selection Checkboxes
            st.markdown("**Include Categories:**")
            active_categories = set()

            # Create a 2-column layout for denser checkboxes
            cb_col1, cb_col2 = st.columns(2)
            for idx, cat in enumerate(sorted_dataset_categories):
                target_col = cb_col1 if idx % 2 == 0 else cb_col2
                # Default to True (included)
                if target_col.checkbox(cat, value=True, key=f"cat_{cat}"):
                    active_categories.add(cat)

            st.divider()
            # Explicit Tag Exclusion
            excluded_tags = st.multiselect("Search and Exclude Specific Tags:",
                                           options=sorted_unique_tags,
                                           default=[])
            excluded_set = set(excluded_tags)

    # --- Data Processing (Filtering based on UI state) ---
    filtered_prompts = []
    for prompt_tags in all_tokenized_prompts:
        # Keep tag ONLY if its category is checked AND it is not explicitly excluded
        valid_tags = [
            t for t in prompt_tags
            if categorizer.get_category(t) in active_categories
            and t not in excluded_set
        ]
        filtered_prompts.append(valid_tags)

    # TF-IDF on the filtered data
    vectorizer = TfidfVectorizer(tokenizer=_identity_tokenizer,
                                 preprocessor=_identity_tokenizer,
                                 token_pattern=None,
                                 max_df=0.90,
                                 min_df=2)

    # Handle edge case where filtering removes all tags from dataset
    try:
        matrix = vectorizer.fit_transform(filtered_prompts)

        # UMAP Clustering (Replaces SVD)
        # UMAP naturally creates separated islands. We use n_components=2.
        reducer = umap.UMAP(n_components=2, random_state=42, metric='cosine')
        coords = reducer.fit_transform(matrix)

        df = pd.DataFrame({
            "id":
            range(len(image_params)),
            "basename":
            [Path(param.original_path).name for param in image_params],
            "x":
            coords[:,
                   0],  # UMAP uses 0 and 1 (No mean-shift component to drop)
            "y":
            coords[:, 1]
        })
    except ValueError:
        st.error(
            "Filtering was too aggressive. No tags left to cluster! Please include more categories."
        )
        st.stop()

    # --- Right Column: Plotly Scatterplot ---
    with right_col:
        st.write("### UMAP Tag Distribution Map")
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

            # Dynamic scaling based on UMAP range
            x_range = df["x"].max() - df["x"].min()
            y_range = df["y"].max() - df["y"].min()
            img_size_x = x_range * 0.03
            img_size_y = y_range * 0.03

            for param, x_val, y_val in zip(image_params, df["x"], df["y"]):
                b64_uri = b64_thumbnails.get(str(param.original_path))
                if b64_uri:
                    images.append(
                        dict(source=b64_uri,
                             x=x_val,
                             y=y_val,
                             xref="x",
                             yref="y",
                             sizex=img_size_x,
                             sizey=img_size_y,
                             xanchor="center",
                             yanchor="middle",
                             layer="below"))

            fig.update_layout(images=images)
            fig.update_traces(marker=dict(opacity=0))

        fig.update_layout(height=850,
                          margin=dict(l=0, r=0, t=0, b=0),
                          dragmode="pan")

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

    # --- Left Column: Previews ---
    with left_container:
        with st.expander("Current Image & Prompt", expanded=True):
            if st.session_state.selection_history:
                current_idx = st.session_state.selection_history[-1]
                selected_param = image_params[current_idx]
                image_path = Path(selected_param.original_path)

                st.markdown(f"**{image_path.name}**")
                st.image(str(image_path), width="stretch")
                st.write("**Prompt:**")
                st.write(selected_param.prompt)
            else:
                st.info(
                    "Click an image on the scatterplot to preview it here.")

        with st.expander("Selection History & Similarities", expanded=True):
            history = st.session_state.selection_history

            if len(history) < 2:
                st.info(
                    f"Select at least 2 images to compare similarities. (Tracking last {MAX_HISTORY_SIZE})"
                )
            else:
                # Iterate backwards
                for i in range(len(history) - 1, 0, -1):
                    idx1 = history[i - 1]
                    idx2 = history[i]
                    param1 = image_params[idx1]
                    param2 = image_params[idx2]

                    path1 = Path(param1.original_path)
                    path2 = Path(param2.original_path)

                    colA, colB = st.columns(2)
                    with colA:
                        st.markdown(f"**{path1.name}**")
                        st.image(str(path1), width="stretch")
                    with colB:
                        st.markdown(f"**{path2.name}**")
                        st.image(str(path2), width="stretch")

                    # Compare Normalized Tags
                    tags1 = set(_extract_features(param1, categorizer))
                    tags2 = set(_extract_features(param2, categorizer))
                    overlap = tags1.intersection(tags2)

                    if overlap:
                        st.success(
                            f"**Overlapping Tags ({len(overlap)}):**\n\n" +
                            ", ".join(sorted(overlap)))
                    else:
                        st.warning(
                            "No overlapping tags between these two images.")

                    if i > 1:
                        st.divider()


if __name__ == "__main__":
    import sys
    # Initialize logging
    logging.basicConfig(level=logging.INFO,
                        format="%(levelname)s: %(message)s")

    if len(sys.argv) < 2:
        print("Usage: streamlit run app.py <workspace_root_path>")
        sys.exit(1)

    workspace_root = Path(sys.argv[1])
    mirror_list = _get_cached_params(workspace_root)

    logging.info("Starting app")
    setup_app(mirror_list)
