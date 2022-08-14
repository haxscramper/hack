#!/usr/bin/env python

import sqlite3
import pprint
import itertools
import matplotlib.pyplot as plt
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

data = [[[] for _ in range(commit_count)] for _ in range(change_count)]

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
        f"{idx:<2} {rows[idx]:<6} ",
        "".join([f"{count:<6}" for count in commit]),
        sep="",
    )


values = np.arange(0, max_per_commit, 500)

# Get some pastel shades for the colors
colors = plt.cm.rainbow(np.linspace(0, 0.8, len(rows)))
plt.figure(figsize=(14, 11), dpi=120)

index = np.arange(len(columns))
bar_width = 1.0

# Initialize the vertical-offset for the stacked bar chart.
y_offset = np.zeros(len(columns))

# Plot bars and create text labels for the table
cell_text = []

# normalized = [np.array(samples) for samples in data]
# norm_sum = itertools.reduce(normalized, lambda a, x: a + x)
# normalized = [samples / norm_sum * 100 for samples in normalized]
plt.margins(x=0)
for commit_idx, samples in enumerate(data):
    plt.bar(
        index,
        samples,
        bar_width,
        bottom=y_offset,
        color=colors[commit_idx],
        edgecolor="black",
    )
    y_offset = y_offset + samples
    cell_text.append([f"{int(x)}" for x in samples])

# Add a table at the bottom of the axes
the_table = plt.table(
    cellText=list(reversed(cell_text)),
    rowLabels=[
        hash_table[rows[idx]][0:8] + ".. " + row
        for (idx, row) in reversed(list(enumerate(rows)))
    ],
    rowColours=list(reversed(colors)),
    colLabels=columns,
    loc="bottom",
)

the_table.set_fontsize(12)
the_table.scale(1, 2)

# Adjust layout to make room for the table:
# plt.subplots_adjust(left=0.2, bottom=0.2)

plt.ylabel("Lines by origin period")
plt.xticks([])
plt.title("Commit year")
plt.savefig("/tmp/db.png", bbox_inches="tight")
