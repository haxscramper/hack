#!/usr/bin/env python
import json
import subprocess
import click

VAAPI_DEVICE = "/dev/dri/renderD128"

# resolution presets: target height; width derived from aspect ratio
RESOLUTIONS = {
    "SD": 480,
    "HD": 720,
    "FULLHD": 1080,
}

# codec -> {backend: encoder}
CODECS = {
    "h264": {
        "vaapi": "h264_vaapi",
        "vulkan": "h264_vulkan"
    },
    "hevc": {
        "vaapi": "hevc_vaapi",
        "vulkan": "hevc_vulkan"
    },
    "av1": {
        "vaapi": "av1_vaapi",
        "vulkan": "av1_vulkan"
    },
    "vp8": {
        "vaapi": "vp8_vaapi"
    },
    "vp9": {
        "vaapi": "vp9_vaapi"
    },
    "mpeg2": {
        "vaapi": "mpeg2_vaapi"
    },
    "mjpeg": {
        "vaapi": "mjpeg_vaapi"
    },
    "ffv1": {
        "vulkan": "ffv1_vulkan"
    },
}


def probe(path):
    out = subprocess.run(
        [
            "ffprobe",
            "-v",
            "error",
            "-select_streams",
            "v:0",
            "-show_entries",
            "stream=width,height,bit_rate,r_frame_rate",
            "-show_entries",
            "format=bit_rate",
            "-of",
            "json",
            str(path),
        ],
        check=True,
        capture_output=True,
        text=True,
    )
    data = json.loads(out.stdout)
    stream = data["streams"][0]
    fmt = data.get("format", {})

    width = int(stream["width"])
    height = int(stream["height"])

    bit_rate = stream.get("bit_rate")
    if bit_rate is None or bit_rate in ("N/A", "0"):
        bit_rate = fmt.get("bit_rate")
    bit_rate = int(bit_rate)

    num, den = stream["r_frame_rate"].split("/")
    fps = float(num) / float(den)

    return width, height, bit_rate, fps


def even(value):
    value = int(round(value))
    return value if value % 2 == 0 else value + 1


@click.command()
@click.argument("input_path", type=click.Path(exists=True, dir_okay=False))
@click.argument("output_path", type=click.Path(dir_okay=False))
@click.option(
    "--resolution",
    type=click.Choice(list(RESOLUTIONS), case_sensitive=False),
    default=None,
    help="Target resolution preset. Omit to keep source resolution.",
)
@click.option(
    "--framerate",
    type=int,
    default=None,
    help="Target framerate. Omit to keep source framerate.",
)
@click.option(
    "--codec",
    type=click.Choice(list(CODECS), case_sensitive=False),
    default="h264",
    help="Output codec (hardware-accelerated only).",
)
@click.option(
    "--backend",
    type=click.Choice(["vaapi", "vulkan"], case_sensitive=False),
    default="vaapi",
    help="Hardware backend.",
)
def main(input_path, output_path, resolution, framerate, codec, backend):
    codec = codec.lower()
    backend = backend.lower()

    encoder = CODECS[codec].get(backend)
    if encoder is None:
        supported = ", ".join(CODECS[codec])
        raise click.BadParameter(
            f"codec '{codec}' has no '{backend}' encoder (available: {supported})"
        )

    src_w, src_h, src_bitrate, src_fps = probe(input_path)

    # target resolution
    if resolution is not None:
        target_h = RESOLUTIONS[resolution.upper()]
        scale = target_h / src_h
        out_w = even(src_w * scale)
        out_h = even(target_h)
    else:
        scale = 1.0
        out_w = even(src_w)
        out_h = even(src_h)

    # target framerate
    out_fps = framerate if framerate is not None else src_fps

    # bitrate scaled by pixel ratio and framerate ratio
    pixel_ratio = (out_w * out_h) / (src_w * src_h)
    fps_ratio = out_fps / src_fps
    out_bitrate = int(src_bitrate * pixel_ratio * fps_ratio)

    if backend == "vaapi":
        cmd = [
            "ffmpeg",
            "-y",
            "-hwaccel",
            "vaapi",
            "-hwaccel_device",
            VAAPI_DEVICE,
            "-hwaccel_output_format",
            "vaapi",
            "-i",
            str(input_path),
        ]
        vf = []
        if framerate is not None:
            vf.append(f"fps={out_fps}")
        vf.append(f"scale_vaapi=w={out_w}:h={out_h}")
        cmd += ["-vf", ",".join(vf)]
    else:  # vulkan
        cmd = [
            "ffmpeg",
            "-y",
            "-init_hw_device",
            f"vulkan=vk:0",
            "-hwaccel",
            "vulkan",
            "-hwaccel_output_format",
            "vulkan",
            "-filter_hw_device",
            "vk",
            "-i",
            str(input_path),
        ]
        vf = []
        if framerate is not None:
            vf.append(f"fps={out_fps}")
        vf.append(f"scale_vulkan=w={out_w}:h={out_h}")
        cmd += ["-vf", ",".join(vf)]

    cmd += [
        "-c:v",
        encoder,
        # "-compression_level",
        # "7",
        "-b:v",
        str(out_bitrate),
        "-c:a",
        "copy",
        str(output_path),
    ]

    click.echo(
        f"Source : {src_w}x{src_h} @ {src_fps:.3f}fps, {src_bitrate} bps")
    click.echo(f"Output : {out_w}x{out_h} @ {out_fps}fps, {out_bitrate} bps")
    click.echo(f"Encoder: {encoder}")
    click.echo("Command: " + " ".join(cmd))

    subprocess.run(cmd, check=True)


if __name__ == "__main__":
    main()
