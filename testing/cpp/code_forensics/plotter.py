#!/usr/bin/env python

import matplotlib.pyplot as plt
import numpy as np
from scipy.interpolate import make_interp_spline
import csv
from typing import *


def smooth(scalars: List[float], weight: float) -> List[float]:
    # Weight between 0 and 1

    # First value in the plot (first timestep)
    last = scalars[0]
    smoothed = list()
    for point in scalars:
        # Calculate smoothed value
        smoothed_val = last * weight + (1 - weight) * point
        smoothed.append(smoothed_val)  # Save it
        last = smoothed_val  # Anchor the last smoothed value

    return smoothed


def plot_commit_data(path: str, outfile: str):
    x = []
    y = []

    with open(path, "r") as csvfile:
        plots = csv.reader(csvfile, delimiter=",")
        for row in plots:
            x.append(int(row[0]))
            y.append(int(row[1]))

    plt.figure(figsize=(15, 10), dpi=80)
    plt.xlabel("Week from the start")
    plt.ylabel("Commits per week")
    plt.plot(x, y, color="black")
    plt.plot(x, smooth(y, 0.9), linewidth=4, color="red")
    plt.savefig(outfile)


plot_commit_data("/tmp/commit-count.csv", "/tmp/commit-count-matplotlib.png")
