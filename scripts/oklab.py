#!/usr/bin/env python3

from math import *

# oklab -> lyz with unnormalized weights
def lyz(r, g, b):
    o3 = 0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b
    m3 = 0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b
    s3 = 0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b

    o = o3 ** (1.0 / 3.0)
    m = m3 ** (1.0 / 3.0)
    s = s3 ** (1.0 / 3.0)

    return (
        0.2104542553 * o + 0.7936177850 * m - 0.0040720468 * s,
        0.0259040371 * o + 0.7827717662 * m - 0.8086757660 * s,
        1.9779984951 * o - 2.4285922050 * m + 0.4505937099 * s,
    )

def hue(l, y, z):
    normalized = atan2(y, z) / (2 * pi)
    return normalized if normalized >= 0 else normalized + 1
