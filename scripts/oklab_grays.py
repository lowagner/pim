#!/usr/bin/env python3

import oklab

if __name__ == "__main__":
    for i in range(256):
        w = i / 255.0
        color = oklab.lyz(w, w, w)
        print("w = %s -> %s -> hue = %s"%(w, color, oklab.hue(*color)))
