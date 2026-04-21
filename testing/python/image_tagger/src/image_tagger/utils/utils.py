from pathlib import Path
from PIL import Image

TEMPLATE_DIR = Path("/tmp/image_tagger_tests/image_template_directory")

DIRECTORY_STRUCTURE = {
    "sub1": {
        ".": 3,  # 3 images in sub1 itself
        "sub11": 0,
        "sub12": 8,
        "sub13": 2,
    },
    "sub2": {
        ".": 1,  # 1 image in sub2 itself
        "sub21": 0,
        "sub22": 0,
        "sub23": {
            ".": 4,  # 4 images in sub23 itself
            "sub231": 2,
            "sub232": 2,
        },
    },
}


def _generate_monotone_image(
        path: Path,
        size: tuple[int, int] = (512, 512),
        color: tuple[int, int, int] = (128, 128, 128),
):
    """Generate a monotone color image and save it to the given path."""
    img = Image.new("RGB", size, color)
    img.save(path)


def _count_specified_images(structure: dict) -> int:
    """Recursively count total images specified in directory structure."""
    total = 0
    for key, value in structure.items():
        if isinstance(value, dict):
            total += _count_specified_images(value)
        else:
            total += value
    return total


def _populate_template_directory(
        template_dir: Path,
        num_images: int = 32,
        size: tuple[int, int] = (512, 512),
):
    """Populate the template directory with monotone color images."""
    import shutil
    import colorsys

    if template_dir.exists():
        shutil.rmtree(template_dir)
    template_dir.mkdir(parents=True)

    # Count images needed for subdirectories
    specified_count = _count_specified_images(DIRECTORY_STRUCTURE)
    root_level_count = num_images - specified_count

    # Define where each image should go
    image_destinations: list[Path] = []

    def collect_destinations(structure: dict, parent: Path):
        for name, value in structure.items():
            if name == ".":
                # Images go directly in current directory
                for _ in range(value):
                    image_destinations.append(parent)
            elif isinstance(value, dict):
                subdir = parent / name
                collect_destinations(value, subdir)
            else:
                subdir = parent / name
                for _ in range(value):
                    image_destinations.append(subdir)

    collect_destinations(DIRECTORY_STRUCTURE, template_dir)

    # Fill rest with root level images
    for _ in range(root_level_count):
        image_destinations.append(template_dir)

    # Generate and place images
    for i, dest in enumerate(image_destinations):
        dest.mkdir(parents=True, exist_ok=True)
        hue = i / num_images
        color = tuple(int(c * 255) for c in colorsys.hsv_to_rgb(hue, 1.0, 1.0))
        _generate_monotone_image(
            dest / f"image_{color[0]}_{color[1]}_{color[2]}.png", size, color)
