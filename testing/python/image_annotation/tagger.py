import sys
import urllib.request
import numpy as np
import onnxruntime as ort
import cv2
import pandas as pd
import json
import logging
import hashlib
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from beartype import beartype
import click

from sqlalchemy import create_engine, Column, Integer, String, Float, ForeignKey, UniqueConstraint
from sqlalchemy.orm import declarative_base, sessionmaker

import os
from PySide6.QtWidgets import (QApplication, QMainWindow, QWidget, QVBoxLayout, 
                             QHBoxLayout, QListWidget, QLineEdit, QSplitter,
                             QLabel, QScrollArea, QGridLayout, QPushButton, QDoubleSpinBox)
from PySide6.QtGui import QPixmap
from PySide6.QtCore import Qt

logging.basicConfig(
    level=logging.INFO,
    format="%(levelname)s %(filename)s:%(lineno)d: %(message)s",
)

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

Base = declarative_base()


class Entry(Base):
    __tablename__ = "entries"
    id = Column(Integer, primary_key=True, autoincrement=True)
    original_name = Column(String)
    full_path = Column(String, unique=True)
    md5_digest = Column(String)


class Tag(Base):
    __tablename__ = "tags"
    id = Column(Integer, primary_key=True, autoincrement=True)
    tag_id = Column(Integer, unique=True)
    name = Column(String)


class EntryTag(Base):
    __tablename__ = "entry_tags"
    id = Column(Integer, primary_key=True, autoincrement=True)
    entry_id = Column(Integer, ForeignKey("entries.id"))
    tag_id = Column(Integer, ForeignKey("tags.id"))
    probability = Column(Float)
    __table_args__ = (UniqueConstraint("entry_id", "tag_id"), )


def init_db(db_path: Path):
    engine = create_engine(f"sqlite:///{db_path}")
    Base.metadata.create_all(engine)
    return engine


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


def populate_entries(image_paths: list[Path], session):
    existing_paths = {row[0] for row in session.query(Entry.full_path).all()}

    to_process = [
        p for p in image_paths if str(p.absolute()) not in existing_paths
    ]
    if not to_process:
        return

    logging.info(
        f"Calculating MD5 digests for {len(to_process)} new entries...")
    results = []
    with ThreadPoolExecutor() as executor:
        for res in executor.map(process_entry_db, to_process):
            results.append(res)

    for full_path, original_name, md5_digest in results:
        entry = Entry(full_path=full_path,
                      original_name=original_name,
                      md5_digest=md5_digest)
        session.add(entry)

    try:
        session.commit()
    except Exception as e:
        session.rollback()
        logging.error(f"Error populating entries: {e}")


def sync_tags(tags_df: pd.DataFrame, session):
    existing_tags = {row[0] for row in session.query(Tag.tag_id).all()}
    tags_to_add = []
    for _, row in tags_df.iterrows():
        tag_id = int(row["tag_id"])
        if tag_id not in existing_tags:
            cat_id = int(row["category"])
            cat_name = CATEGORY_MAP.get(cat_id, f"cat_{cat_id}")
            tag_name = f"infer/wd/{cat_name}/{row["name"]}"
            tags_to_add.append(Tag(tag_id=tag_id, name=tag_name))

    if tags_to_add:
        session.add_all(tags_to_add)
        session.commit()


@beartype
def download_progress(block_num: int, block_size: int,
                      total_size: int) -> None:
    pass


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


def get_xdg_cache_dir() -> Path:
    xdg_cache_home = os.environ.get("XDG_CACHE_HOME")
    if xdg_cache_home:
        return Path(xdg_cache_home) / "haxscramper_tagger" / "thumbnails"
    return Path.home() / ".cache" / "haxscramper_tagger" / "thumbnails"

def generate_thumbnail(args: tuple[str, str]) -> str:
    original_path, md5_digest = args
    cache_dir = get_xdg_cache_dir()
    cache_dir.mkdir(parents=True, exist_ok=True)
    thumb_path = cache_dir / f"{md5_digest}_150x150.jpg"
    
    if thumb_path.exists():
        return str(thumb_path)
        
    img = cv2.imread(original_path)
    if img is not None:
        h, w = img.shape[:2]
        if h > 0 and w > 0:
            scale = min(150/h, 150/w)
            new_h, new_w = max(1, int(h*scale)), max(1, int(w*scale))
            resized = cv2.resize(img, (new_w, new_h), interpolation=cv2.INTER_AREA)
            cv2.imwrite(str(thumb_path), resized)
            return str(thumb_path)
    return original_path

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


@click.group()
def cli():
    pass


@cli.command()
@click.argument("input_dir",
                type=click.Path(exists=True,
                                file_okay=False,
                                dir_okay=True,
                                path_type=Path))
@click.option("--skip-tagged",
              type=click.BOOL,
              required=False,
              default=True,
              help="Skip already tagged images (True/False)")
def annotate(input_dir: Path, skip_tagged: bool):
    db_path = input_dir / "haxscramper_tagger.sqlite"

    ensure_file_exists(MODEL_FILE, REPO_URL + "model.onnx")
    ensure_file_exists(TAGS_FILE, REPO_URL + "selected_tags.csv")

    target_ep = "MIGraphXExecutionProvider"
    available_providers = ort.get_available_providers()
    if target_ep not in available_providers:
        target_ep = "CPUExecutionProvider"
        logging.warning(
            f"MIGraphXExecutionProvider not found, falling back to {target_ep}"
        )

    sess_options = ort.SessionOptions()
    sess_options.add_session_config_entry("session.disable_cpu_ep_fallback",
                                          "1")

    logging.info(f"Loading model strictly to {target_ep}...")
    ort_session = ort.InferenceSession(str(MODEL_FILE),
                                       sess_options=sess_options,
                                       providers=[target_ep])

    valid_extensions = {".jpg", ".jpeg", ".png", ".bmp", ".webp"}
    input_name = ort_session.get_inputs()[0].name
    output_name = ort_session.get_outputs()[0].name
    tags_df = pd.read_csv(str(TAGS_FILE))
    confidence_threshold = 0.01

    engine = init_db(db_path)
    Session = sessionmaker(bind=engine)
    session = Session()

    sync_tags(tags_df, session)
    image_paths = [
        p for p in input_dir.rglob("*")
        if p.is_file() and p.suffix.lower() in valid_extensions
    ]
    populate_entries(image_paths, session)

    tag_id_map = {row.tag_id: row.id for row in session.query(Tag).all()}

    seen_md5s = set()
    total_images = len(image_paths)
    for idx, image_path in enumerate(image_paths, start=1):
        full_path = str(image_path.absolute())
        entry = session.query(Entry).filter_by(full_path=full_path).first()
        if not entry:
            continue

        md5_digest = entry.md5_digest
        if md5_digest in seen_md5s:
            logging.warning(f"[{idx}/{total_images}] Skipping duplicate MD5 image: {image_path.name}")
            continue
        seen_md5s.add(md5_digest)

        old_entry = session.query(Entry).filter(Entry.md5_digest == md5_digest).first()
        if old_entry:
            if skip_tagged:
                has_tags = session.query(EntryTag).filter_by(
                    entry_id=old_entry.id).first()
                    
                if has_tags:
                    logging.info(
                        f"[{idx}/{total_images}] Skipping already tagged image: {image_path.name}")
                    continue

        try:
            input_data = preprocess_image(image_path)
        except Exception as e:
            logging.error(f"Failed to process image {image_path.name}: {e}")
            continue

        entry_tags_data = []
        raw_predictions = ort_session.run([output_name],
                                        {input_name: input_data})[0]
        probabilities = raw_predictions[0]

        for i, prob in enumerate(probabilities):
            if prob >= confidence_threshold:
                tag_id = int(tags_df["tag_id"].iloc[i])
                tag_db_id = tag_id_map.get(tag_id)
                if tag_db_id is not None:
                    entry_tags_data.append(
                        EntryTag(entry_id=entry.id,
                                tag_id=tag_db_id,
                                probability=float(prob)))

        if entry_tags_data:
            session.query(EntryTag).filter_by(entry_id=entry.id).delete()
            session.add_all(entry_tags_data)
            session.commit()

        logging.info(
            f"[{idx}/{total_images}] Saved {len(entry_tags_data)} tags for: {image_path.name} MD5 {md5_digest}")

    session.close()


class ImageLabel(QLabel):
    def __init__(self, entry, thumb_path, click_cb):
        super().__init__()
        self.entry = entry
        self.click_cb = click_cb
        pixmap = QPixmap(thumb_path)
        self.setPixmap(pixmap)
        self.setCursor(Qt.CursorShape.PointingHandCursor)

    def mousePressEvent(self, event):
        self.click_cb(self.entry)


class ViewerWindow(QMainWindow):

    def __init__(self, db_session):
        super().__init__()
        self.db_session = db_session
        self.setWindowTitle("Image Tagger Viewer")
        self.resize(1024, 768)
        self.current_entry = None

        main_widget = QWidget()
        self.setCentralWidget(main_widget)
        main_layout = QVBoxLayout(main_widget)

        search_layout = QHBoxLayout()
        self.search_tag_input = QLineEdit()
        self.search_tag_input.setPlaceholderText("Search tag name...")
        self.search_tag_input.returnPressed.connect(self.load_images)

        self.search_conf_input = QDoubleSpinBox()
        self.search_conf_input.setRange(0.0, 1.0)
        self.search_conf_input.setSingleStep(0.1)
        self.search_conf_input.setValue(0.5)

        search_btn = QPushButton("Search")
        search_btn.clicked.connect(self.load_images)

        search_layout.addWidget(QLabel("Tag:"))
        search_layout.addWidget(self.search_tag_input)
        search_layout.addWidget(QLabel("Min Confidence:"))
        search_layout.addWidget(self.search_conf_input)
        search_layout.addWidget(search_btn)
        search_layout.addStretch()

        main_layout.addLayout(search_layout)

        self.splitter = QSplitter(Qt.Orientation.Horizontal)
        main_layout.addWidget(self.splitter)

        self.scroll_area = QScrollArea()
        self.scroll_area.setWidgetResizable(True)
        self.grid_widget = QWidget()
        self.grid_layout = QGridLayout(self.grid_widget)
        self.scroll_area.setWidget(self.grid_widget)
        self.splitter.addWidget(self.scroll_area)

        self.right_splitter = QSplitter(Qt.Orientation.Vertical)
        self.splitter.addWidget(self.right_splitter)

        self.preview_label = QLabel("Select an image")
        self.preview_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.right_splitter.addWidget(self.preview_label)

        self.tags_list = QListWidget()
        self.right_splitter.addWidget(self.tags_list)
        
        self.right_splitter.setSizes([666, 333])
        self.splitter.setSizes([512, 512])

        self.load_images()

    def load_images(self):
        for i in reversed(range(self.grid_layout.count())):
            item = self.grid_layout.itemAt(i)
            if item and item.widget():
                widgetToRemove = item.widget()
                self.grid_layout.removeWidget(widgetToRemove)
                widgetToRemove.setParent(None)

        tag_name = self.search_tag_input.text().strip()
        conf = self.search_conf_input.value()

        query = self.db_session.query(Entry)
        if tag_name:
            query = query.join(EntryTag).join(Tag).filter(
                Tag.name.like(f"%{tag_name}%"), EntryTag.probability >= conf)

        entries = query.limit(100).all()

        with ThreadPoolExecutor() as executor:
            thumb_paths = list(executor.map(
                generate_thumbnail, 
                [(e.full_path, e.md5_digest) for e in entries]
            ))

        row, col = 0, 0
        cols = 4
        for entry, thumb_path in zip(entries, thumb_paths):
            lbl = ImageLabel(entry, thumb_path, self.on_image_click)
            self.grid_layout.addWidget(lbl, row, col)
            col += 1
            if col >= cols:
                col = 0
                row += 1

    def on_image_click(self, entry):
        self.current_entry = entry
        self.update_preview()

        self.tags_list.clear()
        entry_tags = self.db_session.query(EntryTag).filter_by(
            entry_id=entry.id).order_by(EntryTag.probability.desc()).all()
        for et in entry_tags:
            tag = self.db_session.query(Tag).filter_by(id=et.tag_id).first()
            if tag:
                self.tags_list.addItem(f"{tag.name}: {et.probability:.2f}")

    def resizeEvent(self, event):
        super().resizeEvent(event)
        if self.current_entry:
            self.update_preview()

    def update_preview(self):
        if not self.current_entry:
            return
        pixmap = QPixmap(self.current_entry.full_path)
        avail_size = self.preview_label.size()
        scaled_pixmap = pixmap.scaled(
            avail_size, Qt.AspectRatioMode.KeepAspectRatio,
            Qt.TransformationMode.SmoothTransformation)
        self.preview_label.setPixmap(scaled_pixmap)


@cli.command()
@click.argument("input_dir",
                type=click.Path(exists=True,
                                file_okay=False,
                                dir_okay=True,
                                path_type=Path))
def view(input_dir: Path):
    db_path = input_dir / "haxscramper_tagger.sqlite"
    if not db_path.exists():
        click.echo(
            f"Database not found at {db_path}. Please run \"annotate\" first.")
        sys.exit(1)

    engine = init_db(db_path)
    Session = sessionmaker(bind=engine)
    session = Session()

    app = QApplication(sys.argv)
    window = ViewerWindow(session)
    window.show()
    sys.exit(app.exec())


if __name__ == "__main__":
    cli()
