import os
import sys
import urllib.request
import numpy as np
import onnxruntime as ort
import cv2
import pandas as pd


# ==========================================
# 1. AUTOMATIC MODEL DOWNLOAD
# ==========================================
def download_progress(block_num, block_size, total_size):
    """Callback function to report download progress."""
    downloaded = block_num * block_size
    if total_size > 0:
        percent = min(downloaded * 100 / total_size, 100.0)
        sys.stdout.write(f"\rDownloading: {percent:.1f}%")
        sys.stdout.flush()


def ensure_file_exists(filename, url):
    """Downloads the file if it doesn't exist locally."""
    if not os.path.exists(filename):
        print(f"'{filename}' not found. Downloading from Hugging Face...")
        urllib.request.urlretrieve(url, filename, reporthook=download_progress)
        print(f"\nSuccessfully downloaded {filename}!")
    else:
        print(f"'{filename}' already exists locally. Skipping download.")


# Hugging Face direct resolution URLs
repo_url = "https://huggingface.co/SmilingWolf/wd-vit-tagger-v3/resolve/main/"
ensure_file_exists("model.onnx", repo_url + "model.onnx")
ensure_file_exists("selected_tags.csv", repo_url + "selected_tags.csv")
print("-" * 40)

# ==========================================
# 2. HARDWARE TARGET CONFIGURATION
# ==========================================
# Strictly target the MIGraphX backend for AMD Radeon GPUs
target_ep = 'MIGraphXExecutionProvider'

# Check if your installed ONNX Runtime actually supports MIGraphX
if target_ep not in ort.get_available_providers():
    raise RuntimeError(
        f"{target_ep} is not available in your ONNX Runtime build!\n"
        f"Available EPs: {ort.get_available_providers()}")

# ==========================================
# 3. ENFORCE "NO CPU FALLBACK" RULE
# ==========================================
sess_options = ort.SessionOptions()

# Explicitly disable the silent CPU fallback mechanism in ONNX Runtime.
# If MIGraphX cannot handle a specific graph operation, it will strictly crash
# rather than silently utilizing your Ryzen CPU.
sess_options.add_session_config_entry("session.disable_cpu_ep_fallback", "1")

print(f"Loading model strictly to {target_ep}...")
session = ort.InferenceSession(
    "model.onnx",
    sess_options=sess_options,
    # Pass ONLY MIGraphX. Omitting 'CPUExecutionProvider' is required.
    providers=[target_ep])


# ==========================================
# 4. SMILINGWOLF IMAGE PREPROCESSING
# ==========================================
def preprocess_image(image_path, target_size=448):
    """
    Standard preprocessing for wd-vit-tagger-v3 ONNX:
    - Load image and convert to RGB
    - Resize to 448x448
    - Float32 conversion
    - Shape: (Batch=1, Height=448, Width=448, Channels=3)
    """
    img = cv2.imread(image_path)
    if img is None:
        raise FileNotFoundError(f"Could not load image at: {image_path}")

    # Convert OpenCV's default BGR to RGB
    img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)

    # Resize keeping standard scaling (INTER_AREA is standard for downsampling)
    img = cv2.resize(img, (target_size, target_size),
                     interpolation=cv2.INTER_AREA)

    # Convert to standard float32 array mapped between 0-255
    img_array = img.astype(np.float32)

    # Add the batch dimension: (448, 448, 3) -> (1, 448, 448, 3)
    img_array = np.expand_dims(img_array, axis=0)

    return img_array


# ==========================================
# 5. RUN INFERENCE ON THE ACCELERATOR
# ==========================================
# Hardcoded image path as requested
image_path = "/home/haxscramper/defaultdirs/input/bafkreifby7okcwkx22stopo3wvh6hrnqw7qzuuyqgcqdwclwgfq4bwlouu.jpg"
input_data = preprocess_image(image_path)

# Dynamically get the input/output tensor names from the ONNX graph
input_name = session.get_inputs()[0].name
output_name = session.get_outputs()[0].name

print(f"Running inference on image: {image_path.split('/')[-1]}...")
# This will execute 100% on the AMD GPU due to our strict rules
raw_predictions = session.run([output_name], {input_name: input_data})[0]

# ==========================================
# 6. MAP PREDICTIONS TO TAGS
# ==========================================
tags_df = pd.read_csv("selected_tags.csv")
confidence_threshold = 0.35  # Standard SmilingWolf threshold

# The model outputs a batch of results. We only sent 1 image, so we take index 0.
probabilities = raw_predictions[0]

predicted_tags = {}
for i, prob in enumerate(probabilities):
    if prob > confidence_threshold:
        tag_name = tags_df['name'].iloc[i]
        predicted_tags[tag_name] = prob

# Sort the tags from highest confidence to lowest
predicted_tags = dict(
    sorted(predicted_tags.items(), key=lambda item: item[1], reverse=True))

print("\n--- Detected Tags ---")
for tag, prob in predicted_tags.items():
    print(f"{tag}: {prob:.4f}")
