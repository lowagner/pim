#!/usr/bin/env python3

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

class BestExtremum:
    def __init__(self, key, value):
        self.min_value = value
        self.max_value = value
        self.min_key = key
        self.max_key = key 

    def check(self, key, value):
        if value > self.max_value:
            self.max_key = key
            self.max_value = value
        if value < self.min_value:
            self.min_key = key
            self.min_value = value

    def __repr__(self):
        return "%s -> %s, %s -> %s"%(self.min_key, self.min_value, self.max_key, self.max_value)

extrema = [BestExtremum(None, 0.5), BestExtremum(None, 0), BestExtremum(None, 0)]
print(extrema)

for i in range(256):
    r = i / 255.0
    for j in range(256):
        g = j / 255.0
        for k in range(256):
            b = k / 255.0
            color = lyz(r, g, b)
            key = (r, g, b)
            extrema[0].check(key, color[0])
            extrema[1].check(key, color[1])
            extrema[2].check(key, color[2])

print(extrema)
