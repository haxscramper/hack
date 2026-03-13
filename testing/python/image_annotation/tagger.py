import sys
import urllib.request
import numpy as np
import onnxruntime as ort
import cv2
import pandas as pd
import json
import logging
import sqlite3
import hashlib
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from beartype import beartype

logging.basicConfig(
    level=logging.INFO,
    format="%(levelname)s %(filename)s:%(lineno)d: %(message)s",
)

DB_PATH = Path("/tmp/image_annotation.sqlite")
SKIP_TAGGED = False

CATEGORY_MAP = {
    0: "general",
    1: "artist",
    3: "copyright",
    4: "character",
    5: "meta",
    9: "rating",
}

REPO_URL = "https://huggingface.co/SmilingWolf/wd-vit-tagger-v3/resolve/main/"
MODEL_FILE = Path("model.onnx")
TAGS_FILE = Path("selected_tags.csv")

def init_db():
    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()
    cursor.execute("""
        CREATE TABLE IF NOT EXISTS entries (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            original_name TEXT,
            full_path TEXT UNIQUE,
            md5_digest TEXT
        )
    """)
    cursor.execute("""
        CREATE TABLE IF NOT EXISTS tags (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            tag_id INTEGER UNIQUE,
            name TEXT
        )
    """)
    cursor.execute("""
        CREATE TABLE IF NOT EXISTS entry_tags (
            entry_id INTEGER,
            tag_id INTEGER,
            probability REAL,
            FOREIGN KEY(entry_id) REFERENCES entries(id),
            FOREIGN KEY(tag_id) REFERENCES tags(id),
            UNIQUE(entry_id, tag_id)
        )
    """)
    conn.commit()
    return conn

@beartype
def get_md5(file_path: Path) -> str:
    hash_md5 = hashlib.md5()
    with open(file_path, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()

@beartype
def process_entry_db(file_path: Path) -> tuple[str, str, str]:
    full_path = str(file_path.absolute())
    original_name = file_path.name
    md5_digest = get_md5(file_path)
    return full_path, original_name, md5_digest

def populate_entries(image_paths: list[Path], conn: sqlite3.Connection):
    cursor = conn.cursor()
    cursor.execute("SELECT full_path FROM entries")
    existing_paths = {row[0] for row in cursor.fetchall()}

    to_process = [p for p in image_paths if str(p.absolute()) not in existing_paths]
    if not to_process:
        return

    logging.info(f"Calculating MD5 digests for {len(to_process)} new entries...")
    results = []
    with ThreadPoolExecutor() as executor:
        for res in executor.map(process_entry_db, to_process):
            results.append(res)
    
    cursor.executemany(
        "INSERT OR IGNORE INTO entries (full_path, original_name, md5_digest) VALUES (?, ?, ?)",
        results
    )
    conn.commit()

def sync_tags(tags_df: pd.DataFrame, conn: sqlite3.Connection):
    cursor = conn.cursor()
    tags_data = []
    for _, row in tags_df.iterrows():
        cat_id = int(row['category'])
        cat_name = CATEGORY_MAP.get(cat_id, f"cat_{cat_id}")
        tag_name = f"infer/wd, o/{cat_name}/{row['name']}"
        tags_data.append((int(row['tag_id']), tag_name))
        
    cursor.executemany(
        "INSERT OR IGNORE INTO tags (tag_id, name) VALUES (?, ?)",
        tags_data
    )
    conn.commit()

@beartype
def download_progress(block_num: int, block_size: int,
                      total_size: int) -> None:
    downloaded = block_num * block_size
    if 0 < total_size:
        percent = min(downloaded * 100 / total_size, 100.0)
        # logging.debug(f"Downloading: {percent:.1f}%")

@beartype
def ensure_file_exists(filename: Path, url: str) -> None:
    if not filename.exists():
        logging.info(
            f"File {filename} not found. Downloading from Hugging Face...")
        urllib.request.urlretrieve(url,
                                   str(filename),
                                   reporthook=download_progress)
        logging.info(f"Successfully downloaded {filename}")
    else:
        logging.info(
            f"File {filename} already exists locally. Skipping download.")


@beartype
def preprocess_image(image_path: Path, target_size: int = 448) -> np.ndarray:
    img = cv2.imread(str(image_path))
    if img is None:
        raise FileNotFoundError(f"Could not load image at: {image_path}")

    img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
    img = cv2.resize(img, (target_size, target_size),
                     interpolation=cv2.INTER_AREA)
    img_array = img.astype(np.float32)
    img_array = np.expand_dims(img_array, axis=0)

    return img_array




ensure_file_exists(MODEL_FILE, REPO_URL + "model.onnx")
ensure_file_exists(TAGS_FILE, REPO_URL + "selected_tags.csv")

target_ep = "MIGraphXExecutionProvider"
available_providers = ort.get_available_providers()

if target_ep not in available_providers:
    # Fallback for systems without MIGraphX
    target_ep = "CPUExecutionProvider"
    logging.warning(f"MIGraphXExecutionProvider not found, falling back to {target_ep}")

sess_options = ort.SessionOptions()
sess_options.add_session_config_entry("session.disable_cpu_ep_fallback", "1")

logging.info(f"Loading model strictly to {target_ep}...")
session = ort.InferenceSession(str(MODEL_FILE),
                               sess_options=sess_options,
                               providers=[target_ep])

input_dir = Path(
    "/home/haxscramper/defaultdirs/input/grabber/banana_generations")
valid_extensions = {".jpg", ".jpeg", ".png", ".bmp", ".webp"}

input_name = session.get_inputs()[0].name
output_name = session.get_outputs()[0].name

tags_df = pd.read_csv(str(TAGS_FILE))
confidence_threshold = 0.01

# Initialize Database and pre-process entries
conn = init_db()
sync_tags(tags_df, conn)

image_paths = [p for p in input_dir.rglob("*") if p.is_file() and p.suffix.lower() in valid_extensions]
populate_entries(image_paths, conn)

cursor = conn.cursor()

for image_path in image_paths:
    full_path = str(image_path.absolute())
    
    # Get entry ID
    cursor.execute("SELECT id FROM entries WHERE full_path = ?", (full_path,))
    entry_row = cursor.fetchone()
    if not entry_row:
        continue
    entry_id = entry_row[0]
    
    # Check if already tagged
    if SKIP_TAGGED:
        cursor.execute("SELECT 1 FROM entry_tags WHERE entry_id = ? LIMIT 1", (entry_id,))
        if cursor.fetchone():
            logging.info(f"Skipping already tagged image: {image_path.name}")
            continue

    logging.info(f"Running inference on image: {image_path.name}")

    input_data = preprocess_image(image_path)
    raw_predictions = session.run([output_name],
                                  {input_name: input_data})[0]
    probabilities = raw_predictions[0]

    entry_tags_data = []
    for i, prob in enumerate(probabilities):
        if prob >= confidence_threshold:
            tag_id = int(tags_df["tag_id"].iloc[i])
            cursor.execute("SELECT id FROM tags WHERE tag_id = ?", (tag_id,))
            tag_db_row = cursor.fetchone()
            if tag_db_row:
                tag_db_id = tag_db_row[0]
                entry_tags_data.append((entry_id, tag_db_id, float(prob)))

    if entry_tags_data:
        cursor.execute("DELETE FROM entry_tags WHERE entry_id = ?", (entry_id,))
        cursor.executemany(
            "INSERT INTO entry_tags (entry_id, tag_id, probability) VALUES (?, ?, ?)",
            entry_tags_data
        )
        conn.commit()

    logging.info(f"Saved {len(entry_tags_data)} tags for: {image_path.name}")

conn.close()
