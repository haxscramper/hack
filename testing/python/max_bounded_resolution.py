#!/usr/bin/env python

# Given values
megapixels_limit = 8.2
aspect_ratio_width = 2
aspect_ratio_height = 3

# Convert megapixels to total number of pixels
pixels_limit = int(megapixels_limit * 1_000_000)

# Calculate width and height while maintaining the aspect ratio
# Let width = 2x and height = 3x, and solve for x such that (width * height) <= pixels_limit
# 2x * 3x = pixels_limit
# 6x^2 = pixels_limit
# x = sqrt(pixels_limit / 6)
import math

x = math.sqrt(pixels_limit / (aspect_ratio_width * aspect_ratio_height))

# Calculate width and height
width = int(aspect_ratio_width * x)
height = int(aspect_ratio_height * x)

print((width, height))
