#!/usr/bin/env python

from matplotlib import rcParams

rcParams["font.family"] = "consolas"

from copy import deepcopy
import sqlite3
import pprint
import itertools
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import numpy as np

con = sqlite3.connect("/tmp/db.sqlite")
cur = con.cursor()

commit_table = {}

min_change = None
max_change = None

hash_table = {}

for row in cur.execute(open("table_per_period.sql").read()):
    commit = row[0]
    change = row[1]
    count = row[2]
    hash_table[f"{commit}"] = row[3]
    if commit not in commit_table:
        commit_table[commit] = {}

    if change <= (min_change or change):
        min_change = change
    if (max_change or change) <= change:
        max_change = change

    commit_table[commit][change] = count


change_count = 0

for commit in commit_table:
    for change in range(min_change, max_change + 1):
        if change not in commit_table[commit]:
            commit_table[commit][change] = 0

        change_count = max(change_count, len(commit_table[commit]))

commit_count: int = len(commit_table)
changes = set()
max_per_commit = 0

data = np.zeros([change_count, commit_count]).astype(int)

for commit_idx, commit in enumerate(sorted(commit_table.keys())):
    per_commit = 0
    for change_idx, change in enumerate(sorted(commit_table[commit].keys())):
        changes.add(change)
        lines = commit_table[commit][change]
        data[change_idx][commit_idx] = lines
        per_commit += lines

    max_per_commit = max(max_per_commit, per_commit)


columns = [f"{change}" for change in sorted(changes)]
rows = [f"{commit}" for commit in sorted(commit_table.keys())]

print("  period> ", "".join([f"{count:<6}" for count in columns]), sep="")
for idx, commit in enumerate(data):
    print(
        f"{idx:<2} {columns[idx]:<6} ",
        "".join([f"{count:<6}" for count in commit]),
        sep="",
    )

    # Get some pastel shades for the colors
colors = plt.cm.rainbow(np.linspace(0, 0.8, len(columns)))
index = np.arange(len(rows))

fig = plt.figure(figsize=(10, 12), constrained_layout=True)
spec = gridspec.GridSpec(ncols=1, nrows=2, figure=fig)
barplot = fig.add_subplot(spec[0, 0])
heatmap = fig.add_subplot(spec[1, 0])
barplot.set_ylabel("SLOC total")
barplot.margins(x=0)
# Initialize the vertical-offset for the stacked bar chart.
y_offset = np.zeros(len(rows))

# Plot bars and create text labels for the table
cell_text = []
cellColours = []
for commit_idx, samples in enumerate(data):
    barplot.bar(
        index,
        samples,
        width=1.0,
        bottom=y_offset,
        color=colors[commit_idx],
        edgecolor="black",
    )
    y_offset = y_offset + samples
    cell_text.append([f"{int(x)}" for x in samples])
    res_colors = []
    count_max = samples.max()
    print("max=", count_max)
    for entry in samples:
        if entry == 0:
            res_colors.append("white")

        else:
            avg = entry / count_max
            color = deepcopy(colors[commit_idx])
            color[0] = (color[0] - 1.0) * avg + 1.0
            color[1] = (color[1] - 1.0) * avg + 1.0
            color[2] = (color[2] - 1.0) * avg + 1.0
            res_colors.append(color)

    cellColours.append(res_colors)

heatmap.axis("off")
heatmap.axis("tight")
table = heatmap.table(
    cellText=cell_text,
    rowLabels=columns,
    cellColours=cellColours,
    rowColours=colors,
    colLabels=rows,
    loc="top",
)


heatmap.table(
    cellText=[[str(it) for it in data.sum(axis=0)]], rowLabels=["Total"], loc="bottom"
)

heatmap.set_yticks([])
heatmap.set_xticks([])
# heatmap.set_xlabel("Sampling period")
# heatmap.set_ylabel("Code origin")

plt.savefig("/tmp/db.png", bbox_inches="tight")
