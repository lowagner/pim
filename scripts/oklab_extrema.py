#!/usr/bin/env python3

import oklab

class Extremum:
    def __init__(self, name, value):
        self.name = name
        self.min_value = value
        self.max_value = value
        self.min_key = None
        self.max_key = None

    def check(self, key, value):
        if value > self.max_value:
            self.max_key = key
            self.max_value = value
        if value < self.min_value:
            self.min_key = key
            self.min_value = value

    def __repr__(self):
        return "{%s min(%s -> %s), max(%s -> %s)}"%(self.name, self.min_key, self.min_value, self.max_key, self.max_value)

if __name__ == "__main__":
    extrema = [Extremum('l', 0.5), Extremum('y', 0), Extremum('z', 0), Extremum('s', 0.01)]

    for i in range(256):
        r = i / 255.0
        for j in range(256):
            g = j / 255.0
            for k in range(256):
                b = k / 255.0
                color = oklab.lyz(r, g, b)
                key = (r, g, b)
                extrema[0].check(key, color[0])
                extrema[1].check(key, color[1])
                extrema[2].check(key, color[2])
                extrema[3].check(key, (color[1]**2 + color[2]**2)**0.5)

    print(extrema)

