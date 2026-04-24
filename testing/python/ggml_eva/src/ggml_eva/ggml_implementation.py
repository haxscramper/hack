#!/usr/bin/env python

from pathlib import Path
import ctypes

import cv2
import ggml
import numpy as np
import pandas as pd
from safetensors import safe_open

CATEGORY_MAP = {
    0: "general",
    1: "artist",
    3: "copyright",
    4: "character",
    5: "meta",
    9: "rating",
}

IMAGE_SIZE = 448
PATCH_SIZE = 16
GRID_SIZE = 28
NUM_TOKENS = 784
EMBED_DIM = 768
NUM_HEADS = 12
HEAD_DIM = 64
MLP_DIM = 3072
NUM_BLOCKS = 12
NUM_CLASSES = 10861
LN_EPS = 1e-6


def load_safetensors(path: Path) -> dict[str, np.ndarray]:
    result: dict[str, np.ndarray] = {}
    with safe_open(path, framework="pt", device="cpu") as f:
        for key in f.keys():
            result[key] = f.get_tensor(key).cpu().numpy().astype(np.float32,
                                                                 copy=True)
    return result


def preprocess_image(image_path: Path) -> np.ndarray:
    img = cv2.imread(str(image_path))
    if img is None:
        raise FileNotFoundError(image_path)

    img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
    img = cv2.resize(img, (IMAGE_SIZE, IMAGE_SIZE),
                     interpolation=cv2.INTER_AREA)
    img = img.astype(np.float32) / 255.0
    img = (img - 0.5) / 0.5
    img = np.transpose(img, (2, 0, 1))
    img = np.ascontiguousarray(img)
    return img


def tensor_bytes(shape: tuple[int, ...], dtype: np.dtype = np.float32) -> int:
    count = 1
    for dim in shape:
        count *= dim
    return count * np.dtype(dtype).itemsize


def copy_array_to_tensor(tensor, array: np.ndarray) -> None:
    array = np.ascontiguousarray(array, dtype=np.float32)
    nbytes = array.nbytes
    dst = ggml.ggml_get_data(tensor)
    ctypes.memmove(dst, array.ctypes.data, nbytes)


def new_tensor_1d_from_array(ctx, array: np.ndarray):
    array = np.ascontiguousarray(array, dtype=np.float32)
    t = ggml.ggml_new_tensor_1d(ctx, ggml.GGML_TYPE_F32, array.shape[0])
    copy_array_to_tensor(t, array)
    return t


def new_tensor_2d_from_array(ctx, array: np.ndarray):
    array = np.ascontiguousarray(array, dtype=np.float32)
    rows, cols = array.shape
    t = ggml.ggml_new_tensor_2d(ctx, ggml.GGML_TYPE_F32, cols, rows)
    copy_array_to_tensor(t, array)
    return t


def new_tensor_3d_from_array(ctx, array: np.ndarray):
    array = np.ascontiguousarray(array, dtype=np.float32)
    d0, d1, d2 = array.shape
    t = ggml.ggml_new_tensor_3d(ctx, ggml.GGML_TYPE_F32, d2, d1, d0)
    copy_array_to_tensor(t, array)
    return t


def new_tensor_4d_from_array(ctx, array: np.ndarray):
    array = np.ascontiguousarray(array, dtype=np.float32)
    d0, d1, d2, d3 = array.shape
    t = ggml.ggml_new_tensor_4d(ctx, ggml.GGML_TYPE_F32, d3, d2, d1, d0)
    copy_array_to_tensor(t, array)
    return t


def make_weight_tensors(ctx, weights: dict[str,
                                           np.ndarray]) -> dict[str, object]:
    tensors: dict[str, object] = {}

    for key, value in weights.items():
        if value.ndim == 1:
            tensors[key] = new_tensor_1d_from_array(ctx, value)
        elif value.ndim == 2:
            tensors[key] = new_tensor_2d_from_array(ctx, value)
        elif value.ndim == 3:
            tensors[key] = new_tensor_3d_from_array(ctx, value)
        elif value.ndim == 4:
            tensors[key] = new_tensor_4d_from_array(ctx, value)
        else:
            raise ValueError(f"unsupported ndim for {key}: {value.ndim}")

    return tensors


def ggml_cont(ctx, x):
    return ggml.ggml_cont(ctx, x)


def ggml_permute_to_bthd(ctx, x):
    return ggml.ggml_permute(ctx, x, 0, 2, 1, 3)


def linear(ctx, x, weight, bias):
    y = ggml.ggml_mul_mat(ctx, weight, x)
    bias_reshaped = ggml.ggml_reshape_2d(
        ctx, bias, 1,
        ggml.ggml_nelements(x) // ggml.ggml_ne0(x))
    y = ggml.ggml_add(ctx, y, bias_reshaped)
    return y


def layer_norm_last_dim(ctx, x, weight, bias):
    y = ggml.ggml_norm(ctx, x, LN_EPS)
    y = ggml.ggml_mul(ctx, y, weight)
    y = ggml.ggml_add(ctx, y, bias)
    return y


def gelu_tanh(ctx, x):
    return ggml.ggml_gelu(ctx, x)


def repeat_bias_2d(ctx, bias, cols, rows):
    reshaped = ggml.ggml_reshape_2d(ctx, bias, 1, rows)
    return ggml.ggml_repeat(
        ctx, reshaped,
        ggml.ggml_new_tensor_2d(ctx, ggml.GGML_TYPE_F32, cols, rows))


def add_bias_2d(ctx, x, bias):
    cols = ggml.ggml_ne0(x)
    rows = ggml.ggml_ne1(x)
    reshaped = ggml.ggml_reshape_2d(ctx, bias, 1, rows)
    repeated = ggml.ggml_repeat(ctx, reshaped, x)
    return ggml.ggml_add(ctx, x, repeated)


def patch_embed(ctx, image, weight, bias):
    x = ggml.ggml_conv_2d(
        ctx,
        weight,
        image,
        PATCH_SIZE,
        PATCH_SIZE,
        0,
        0,
        1,
        1,
    )

    bias_4d = ggml.ggml_reshape_4d(ctx, bias, 1, 1, EMBED_DIM, 1)
    bias_4d = ggml.ggml_repeat(ctx, bias_4d, x)
    x = ggml.ggml_add(ctx, x, bias_4d)

    return x


def flatten_patches(ctx, x):
    x = ggml.ggml_cont(ctx, x)
    x = ggml.ggml_reshape_2d(ctx, x, GRID_SIZE * GRID_SIZE, EMBED_DIM)
    x = ggml.ggml_transpose(ctx, x)
    x = ggml.ggml_cont(ctx, x)
    return x


def add_pos_embed(ctx, x, pos_embed):
    pos = ggml.ggml_reshape_2d(ctx, pos_embed, EMBED_DIM, NUM_TOKENS)
    return ggml.ggml_add(ctx, x, pos)


def attention(ctx, x, tensors: dict[str, object], block_idx: int):
    prefix = f"blocks.{block_idx}.attn"

    qkv = ggml.ggml_mul_mat(ctx, tensors[f"{prefix}.qkv.weight"], x)
    qkv = add_bias_2d(ctx, qkv, tensors[f"{prefix}.qkv.bias"])

    q = ggml.ggml_view_2d(
        ctx,
        qkv,
        EMBED_DIM,
        NUM_TOKENS,
        ggml.ggml_element_size(qkv) * (3 * EMBED_DIM),
        0,
    )
    k = ggml.ggml_view_2d(
        ctx,
        qkv,
        EMBED_DIM,
        NUM_TOKENS,
        ggml.ggml_element_size(qkv) * (3 * EMBED_DIM),
        ggml.ggml_element_size(qkv) * EMBED_DIM,
    )
    v = ggml.ggml_view_2d(
        ctx,
        qkv,
        EMBED_DIM,
        NUM_TOKENS,
        ggml.ggml_element_size(qkv) * (3 * EMBED_DIM),
        ggml.ggml_element_size(qkv) * (2 * EMBED_DIM),
    )

    q = ggml.ggml_cont(ctx, q)
    k = ggml.ggml_cont(ctx, k)
    v = ggml.ggml_cont(ctx, v)

    q = ggml.ggml_reshape_3d(ctx, q, HEAD_DIM, NUM_HEADS, NUM_TOKENS)
    k = ggml.ggml_reshape_3d(ctx, k, HEAD_DIM, NUM_HEADS, NUM_TOKENS)
    v = ggml.ggml_reshape_3d(ctx, v, HEAD_DIM, NUM_HEADS, NUM_TOKENS)

    k_t = ggml.ggml_permute(ctx, k, 1, 2, 0, 3)
    q_p = ggml.ggml_permute(ctx, q, 1, 2, 0, 3)

    attn = ggml.ggml_mul_mat(ctx, k_t, q_p)

    scale_arr = np.array([HEAD_DIM**-0.5], dtype=np.float32)
    scale = new_tensor_1d_from_array(ctx, scale_arr)
    attn = ggml.ggml_scale(ctx, attn, scale)

    attn = ggml.ggml_soft_max(ctx, attn)

    v_p = ggml.ggml_permute(ctx, v, 1, 2, 0, 3)
    out = ggml.ggml_mul_mat(ctx, v_p, attn)

    out = ggml.ggml_permute(ctx, out, 2, 0, 1, 3)
    out = ggml.ggml_cont(ctx, out)
    out = ggml.ggml_reshape_2d(ctx, out, EMBED_DIM, NUM_TOKENS)

    out = ggml.ggml_mul_mat(ctx, tensors[f"{prefix}.proj.weight"], out)
    out = add_bias_2d(ctx, out, tensors[f"{prefix}.proj.bias"])

    return out


def mlp(ctx, x, tensors: dict[str, object], block_idx: int):
    prefix = f"blocks.{block_idx}.mlp"

    y = ggml.ggml_mul_mat(ctx, tensors[f"{prefix}.fc1.weight"], x)
    y = add_bias_2d(ctx, y, tensors[f"{prefix}.fc1.bias"])
    y = gelu_tanh(ctx, y)
    y = ggml.ggml_mul_mat(ctx, tensors[f"{prefix}.fc2.weight"], y)
    y = add_bias_2d(ctx, y, tensors[f"{prefix}.fc2.bias"])

    return y


def block(ctx, x, tensors: dict[str, object], block_idx: int):
    n1 = layer_norm_last_dim(
        ctx,
        x,
        tensors[f"blocks.{block_idx}.norm1.weight"],
        tensors[f"blocks.{block_idx}.norm1.bias"],
    )
    x = ggml.ggml_add(ctx, x, attention(ctx, n1, tensors, block_idx))

    n2 = layer_norm_last_dim(
        ctx,
        x,
        tensors[f"blocks.{block_idx}.norm2.weight"],
        tensors[f"blocks.{block_idx}.norm2.bias"],
    )
    x = ggml.ggml_add(ctx, x, mlp(ctx, n2, tensors, block_idx))

    return x


def global_avg_pool_tokens(ctx, x):
    x3 = ggml.ggml_reshape_3d(ctx, x, EMBED_DIM, NUM_TOKENS, 1)
    pooled = ggml.ggml_pool_1d(ctx, x3, ggml.GGML_OP_POOL_AVG, NUM_TOKENS,
                               NUM_TOKENS, 0)
    pooled = ggml.ggml_cont(ctx, pooled)
    pooled = ggml.ggml_reshape_1d(ctx, pooled, EMBED_DIM)
    return pooled


def head(ctx, x, weight, bias):
    x2 = ggml.ggml_reshape_2d(ctx, x, EMBED_DIM, 1)
    y = ggml.ggml_mul_mat(ctx, weight, x2)
    y = add_bias_2d(ctx, y, bias)
    y = ggml.ggml_sigmoid(ctx, y)
    return y


def build_model_graph(ctx, tensors: dict[str, object], input_image_tensor):
    x = patch_embed(
        ctx,
        input_image_tensor,
        tensors["patch_embed.proj.weight"],
        tensors["patch_embed.proj.bias"],
    )
    x = flatten_patches(ctx, x)
    x = add_pos_embed(ctx, x, tensors["pos_embed"])

    for i in range(NUM_BLOCKS):
        x = block(ctx, x, tensors, i)

    x = layer_norm_last_dim(ctx, x, tensors["norm.weight"],
                            tensors["norm.bias"])
    x = global_avg_pool_tokens(ctx, x)
    x = head(ctx, x, tensors["head.weight"], tensors["head.bias"])

    gf = ggml.ggml_new_graph(ctx)
    ggml.ggml_build_forward_expand(gf, x)
    return gf, x


class GgmlWdTagger:

    def __init__(self,
                 model_path: Path,
                 tags_csv_path: Path,
                 threshold: float = 0.01):
        self.threshold = threshold
        self.tags_df = pd.read_csv(str(tags_csv_path))
        self.weights = load_safetensors(model_path)

        mem_size = 1024 * 1024 * 1024
        params = ggml.ggml_init_params(mem_size=mem_size, mem_buffer=None)
        self.ctx = ggml.ggml_init(params)

        self.tensor_map = make_weight_tensors(self.ctx, self.weights)
        self.input_tensor = ggml.ggml_new_tensor_4d(
            self.ctx,
            ggml.GGML_TYPE_F32,
            IMAGE_SIZE,
            IMAGE_SIZE,
            3,
            1,
        )

        self.graph, self.output_tensor = build_model_graph(
            self.ctx, self.tensor_map, self.input_tensor)

    def tag_image(self, image_path: Path) -> list[tuple[str, str, float]]:
        image = preprocess_image(image_path)
        image = np.expand_dims(image, axis=0)
        image = np.ascontiguousarray(image, dtype=np.float32)
        copy_array_to_tensor(self.input_tensor, image)
        ggml.ggml_graph_compute_with_ctx(self.ctx, self.graph, 1)

        out = np.ctypeslib.as_array(
            ctypes.cast(ggml.ggml_get_data(self.output_tensor),
                        ctypes.POINTER(ctypes.c_float)),
            shape=(NUM_CLASSES, ),
        ).copy()

        result: list[tuple[str, str, float]] = []
        for i, prob in enumerate(out):
            if prob >= self.threshold:
                row = self.tags_df.iloc[i]
                category = CATEGORY_MAP.get(int(row["category"]),
                                            f"cat_{int(row['category'])}")
                result.append((category, str(row["name"]), float(prob)))

        result.sort(key=lambda x: x[2], reverse=True)
        return result

    def close(self) -> None:
        if self.ctx is not None:
            ggml.ggml_free(self.ctx)
            self.ctx = None


def main() -> None:
    model_path = Path("model.safetensors")
    tags_csv_path = Path("selected_tags.csv")
    image_path = Path("test.png")

    tagger = GgmlWdTagger(model_path, tags_csv_path)
    try:
        tags = tagger.tag_image(image_path)
        for category, name, prob in tags[:40]:
            print(f"{prob:8.5f}  {category:10}  {name}")
    finally:
        tagger.close()


if __name__ == "__main__":
    main()
