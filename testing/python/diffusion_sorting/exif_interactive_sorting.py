from beartype import beartype
from exif_preview import ImageParams, get_full_params, log, Tag, LoRATag
import logging
import re
from dataclasses import dataclass
from pathlib import Path

import pandas as pd
import plotly.express as px
import streamlit as st
from scipy.sparse import csr_matrix
from sklearn.decomposition import TruncatedSVD
from sklearn.feature_extraction.text import TfidfVectorizer


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
def setup_app(image_params: list["ImageParams"]) -> None:
    st.set_page_config(layout="wide")

    if "draft_weights" not in st.session_state:
        st.session_state.draft_weights = {}
    if "applied_weights" not in st.session_state:
        st.session_state.applied_weights = {}

    tokenized_prompts = [_extract_features(param) for param in image_params]

    tfidf_result = _get_tfidf_model(tokenized_prompts)
    base_matrix = tfidf_result.matrix
    feature_names = tfidf_result.feature_names
    vectorizer = tfidf_result.vectorizer

    left_col, mid_col, right_col = st.columns([2, 5, 2])

    with left_col:
        st.write(
            "Sliders for tag weight changing. The tags are sorted based on how common they are in the dataset. Clicking on the tag in the list will show the spinbox, and you can enter any weight."
        )

        tag_counts = _get_sorted_features(tokenized_prompts, feature_names)
        tag_df = pd.DataFrame(tag_counts)

        event_df = st.dataframe(tag_df,
                                width="stretch",
                                hide_index=True,
                                on_select="rerun",
                                selection_mode="single-row")

        selected_rows = event_df.selection.rows
        if 0 < len(selected_rows):
            selected_tag = tag_df.iloc[selected_rows[0]]["Tag"]
            current_val = st.session_state.draft_weights.get(selected_tag, 1.0)
            new_val = st.number_input(f"Weight: {selected_tag}",
                                      value=float(current_val),
                                      step=0.1)
            st.session_state.draft_weights[selected_tag] = new_val

        if st.button("rebuild"):
            st.session_state.applied_weights = st.session_state.draft_weights.copy(
            )
            st.rerun()

    working_matrix = base_matrix.copy()

    for tag, weight in st.session_state.applied_weights.items():
        tag_index = vectorizer.vocabulary_.get(tag)
        if tag_index is not None:
            working_matrix[:,
                           tag_index] = working_matrix[:, tag_index] * weight

    svd = TruncatedSVD(n_components=2, random_state=42)
    coords = svd.fit_transform(working_matrix)

    df = pd.DataFrame({
        "id":
        range(len(image_params)),
        "basename": [Path(param.original_path).name for param in image_params],
        "x":
        coords[:, 0],
        "y":
        coords[:, 1]
    })

    with mid_col:
        plot_container = st.container()
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
            images = []
            for param, x_val, y_val in zip(image_params, coords[:, 0],
                                           coords[:, 1]):
                images.append(
                    dict(source=Path(param.original_path).as_uri(),
                         x=x_val,
                         y=y_val,
                         sizex=0.05,
                         sizey=0.05,
                         xanchor="center",
                         yanchor="middle",
                         layer="below"))
            fig.update_layout(images=images)
            fig.update_traces(marker=dict(opacity=0))

        fig.update_layout(height=900,
                          margin=dict(l=0, r=0, t=0, b=0),
                          dragmode="pan")

        with plot_container:
            event = st.plotly_chart(fig,
                                    on_select="rerun",
                                    selection_mode="points",
                                    width="stretch")

    with right_col:
        st.write("### Image prompt")
        if 0 < len(event.selection.points):
            selected_index = event.selection.points[0]["point_index"]
            selected_param = image_params[selected_index]

            st.write(selected_param.prompt)

            image_path = Path(selected_param.original_path)
            st.write(f"**{image_path.name}**")

            st.write("### Image full view")
            st.image(str(image_path))
            logging.info(f"Loaded image from {image_path}")


if __name__ == "__main__":
    import sys
    workspace_root = Path(sys.argv[1])
    mirror_list = _get_cached_params(workspace_root)

    logging.info("Starting app")
    setup_app(mirror_list)
