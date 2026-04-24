#!/usr/bin/env python

from pathlib import Path

import cv2
import pandas as pd
import numpy as np
import torch
import torch.nn.functional as F
from safetensors import safe_open


def load_safetensors(path: Path) -> dict[str, torch.Tensor]:
    result: dict[str, torch.Tensor] = {}
    with safe_open(path, framework="pt", device="cpu") as f:
        for key in f.keys():
            result[key] = f.get_tensor(key)
    return result


def preprocess_image(image_path: Path, image_size: int = 448) -> torch.Tensor:
    img = cv2.imread(str(image_path))
    if img is None:
        raise FileNotFoundError(image_path)

    img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
    img = cv2.resize(img, (image_size, image_size),
                     interpolation=cv2.INTER_AREA)
    img = img.astype(np.float32) / 255.0
    img = (img - 0.5) / 0.5
    img = np.transpose(img, (2, 0, 1))
    img = np.expand_dims(img, 0)

    return torch.from_numpy(img)


def layer_norm(x: torch.Tensor,
               weight: torch.Tensor,
               bias: torch.Tensor,
               eps: float = 1e-6) -> torch.Tensor:
    return F.layer_norm(x, (x.shape[-1], ), weight=weight, bias=bias, eps=eps)


def linear(x: torch.Tensor, weight: torch.Tensor,
           bias: torch.Tensor) -> torch.Tensor:
    return F.linear(x, weight, bias)


def gelu_tanh(x: torch.Tensor) -> torch.Tensor:
    return F.gelu(x, approximate="tanh")


def attention(x: torch.Tensor,
              weights: dict[str, torch.Tensor],
              block_idx: int,
              num_heads: int = 12) -> torch.Tensor:
    prefix = f"blocks.{block_idx}.attn"

    qkv = linear(
        x,
        weights[f"{prefix}.qkv.weight"],
        weights[f"{prefix}.qkv.bias"],
    )

    batch, tokens, three_dim = qkv.shape
    head_dim = x.shape[-1] // num_heads

    qkv = qkv.reshape(batch, tokens, 3, num_heads, head_dim)
    qkv = qkv.permute(2, 0, 3, 1, 4)

    q = qkv[0]
    k = qkv[1]
    v = qkv[2]

    scale = head_dim**-0.5
    attn = torch.matmul(q, k.transpose(-2, -1)) * scale
    attn = torch.softmax(attn, dim=-1)

    out = torch.matmul(attn, v)
    out = out.transpose(1, 2).reshape(batch, tokens, x.shape[-1])

    out = linear(
        out,
        weights[f"{prefix}.proj.weight"],
        weights[f"{prefix}.proj.bias"],
    )

    return out


def mlp(x: torch.Tensor, weights: dict[str, torch.Tensor],
        block_idx: int) -> torch.Tensor:
    prefix = f"blocks.{block_idx}.mlp"

    x = linear(
        x,
        weights[f"{prefix}.fc1.weight"],
        weights[f"{prefix}.fc1.bias"],
    )
    x = gelu_tanh(x)
    x = linear(
        x,
        weights[f"{prefix}.fc2.weight"],
        weights[f"{prefix}.fc2.bias"],
    )
    return x


def block(x: torch.Tensor, weights: dict[str, torch.Tensor],
          block_idx: int) -> torch.Tensor:
    norm1 = layer_norm(
        x,
        weights[f"blocks.{block_idx}.norm1.weight"],
        weights[f"blocks.{block_idx}.norm1.bias"],
    )
    x = x + attention(norm1, weights, block_idx)

    norm2 = layer_norm(
        x,
        weights[f"blocks.{block_idx}.norm2.weight"],
        weights[f"blocks.{block_idx}.norm2.bias"],
    )
    x = x + mlp(norm2, weights, block_idx)

    return x


def forward(image: torch.Tensor, weights: dict[str,
                                               torch.Tensor]) -> torch.Tensor:
    x = F.conv2d(
        image,
        weights["patch_embed.proj.weight"],
        weights["patch_embed.proj.bias"],
        stride=16,
    )

    batch, channels, height, width = x.shape
    x = x.reshape(batch, channels, height * width)
    x = x.transpose(1, 2)

    x = x + weights["pos_embed"]

    for i in range(12):
        x = block(x, weights, i)

    x = layer_norm(x, weights["norm.weight"], weights["norm.bias"])
    x = x.mean(dim=1)

    logits = linear(x, weights["head.weight"], weights["head.bias"])
    probs = torch.sigmoid(logits)

    return probs


def main() -> None:
    model_path = Path("model.safetensors")
    image_path = Path("test.png")

    weights = load_safetensors(model_path)
    image = preprocess_image(image_path)
    tags_df = pd.read_csv(str("selected_tags.csv"))

    with torch.no_grad():
        probs = forward(image, weights)

    topk = torch.topk(probs[0], k=20)
    for score, idx in zip(topk.values.tolist(), topk.indices.tolist()):
        print(tags_df.iloc[idx]["name"], idx, score)


if __name__ == "__main__":
    main()
