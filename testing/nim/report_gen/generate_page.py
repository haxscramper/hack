#!/usr/bin/env python
import glob
import re
from pygments import highlight
from pygments.lexers import CLexer, YamlLexer
from pygments.formatters import HtmlFormatter
import argparse

parser = argparse.ArgumentParser(description='Generate html')

parser.add_argument('--files-glob',
                    dest='files_glob',
                    required=True,
                    help='POSIX glob for list of files to process')

parser.add_argument('--syntax-suffix',
                    dest="syntax_suffix",
                    required=True,
                    help="Suffix added for file syntax dump")

parser.add_argument("--syntax-prefix",
                    dest="syntax_prefix",
                    required=True,
                    help="Prefx that will be added to path" +
                    "to generate image file path")

parser.add_argument("--image-suffix",
                    dest="image_suffix",
                    required=True,
                    help="Suffix for image file")

parser.add_argument("--image-prefix",
                    dest="image_prefix",
                    required=True,
                    help="Prefix that will be added to path" +
                    "to generate image file path")

parser.add_argument("--output-file",
                    dest="output_file",
                    default="out.tmp.html",
                    help="Name of the output file")

parser.add_argument("--files-prefix",
                    dest="files_prefix",
                    required=True,
                    help="Prefix that will be removed from" +
                    "file name to generate image/" + "synt path")
args = parser.parse_args()

files = [f for f in glob.glob(args.files_glob)]

files.sort()

print("Output file:", args.output_file)
print("Image suffix:", args.image_suffix)
print("Image prefix:", args.image_prefix)
print("Syntax suffix:", args.syntax_suffix)
print("Syntax prefix:", args.syntax_prefix)
print("Files glob:", args.files_glob)

rows = []
for f in files:
    rows.append("""
        <tr>
        <td><b>{}</b></td>
        <td><b>non</b></td>
        <td>file:</td>
        </tr>""".format(f))

    code_filename = f[len(args.files_prefix):]
    image_file = args.image_prefix + code_filename + args.image_suffix
    synt_file = args.syntax_prefix + code_filename + args.syntax_suffix

    print("")
    print("   code:", f)
    print("   file:", code_filename)
    print("   imag:", image_file)
    print("   synt:", synt_file)

    code = highlight(open(f).read(), CLexer(), HtmlFormatter())
    synt = highlight(open(synt_file).read(), YamlLexer(), HtmlFormatter())

    imag = "<img src=\"{}\"></img>".format(image_file)

    row = "<tr><td>{}</td><td>{}</td><td>{}</td></tr>".format(code, imag, synt)

    rows.append(row)

head = """
<!DOCTYPE html>
<html>
<head>
    <title>diagram tests</title>
</head>
"""

style = """
<style>
{}
</style>
""".format(HtmlFormatter().get_style_defs('.highlight'))

table_header = """
<tr>
<td><b>source code</b></td>
<td><b>Syntax tree dump</b></td>
<td><b>Source file location</b></td>
</tr>
"""

table = "<table border=\"3\">" + table_header + "\n".join(rows) + "</table>"

out = head + style + "<body>" + table + "</body><html>"

with open(args.output_file, "w") as f:
    f.write(out)
