#!/bin/bash

# find all files jpg/svg/png and make their compressed 50% .webp version
find ./../images/ -type f \( -name "*.jpg" -o -name "*.png" -o -name "*.svg" \) -exec bash -c 'cwebp -q 50 "$1" -o "${1%.*}.webp"' _ {} \;
